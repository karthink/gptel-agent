;;; gptel-agent-tools.el --- LLM tools for gptel-agent     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds the following gptel tools.
;; System:
;; - "Bash"           : Execute a Bash command.
;;
;; Web:
;; - "WebSearch"             : Search the web for the first five results to a query.
;; - "Read"               : Fetch and read the contents of a URL.
;; - "YouTube"       : Find the description and video transcript for a youtube video.
;;
;; Filesystem:
;; - "Mkdir"  : Create a new directory.
;; - "Glob"      : Find files matching a glob pattern
;; - "Grep"      : Grep for text in file(s).
;; - "Read" : Read a specific line range from a file.
;; - "Insert"  : Insert text at a specific line number in a file.
;; - "Edit"      : Replace text in file(s) using string match or unified diff.
;; - "Write"      : Create a new file with content.

;;; Code:



(require 'gptel)
(require 'eww)
(require 'url-http)
(eval-when-compile (require 'cl-lib))

(declare-function org-escape-code-in-region "org-src")
(declare-function gptel-agent-read-file "gptel-agent")

(defvar url-http-end-of-headers)
(defvar gptel-agent--agents)
(defvar gptel-agent--skills)
(defvar gptel-agent--hrule)
(defvar gptel-agent--parent-buffer)
(defvar gptel-agent--parent-callback)
(defvar gptel-agent--agent-type)
(defvar gptel-agent--task-description)
(defvar gptel-agent--pending-user-requests)
(defvar gptel-agent--auto-confirm-tools)
(defvar gptel-agent--request-id-counter)
(defvar gptel-agent-kill-finished-buffers)
(defvar gptel-agent--active-subagents)

(declare-function gptel-agent--generate-request-id "gptel-agent")
(declare-function gptel-agent--cleanup-subagent "gptel-agent")
(declare-function gptel-agent--task-preview-setup "gptel-agent")
(declare-function gptel-agent--task "gptel-agent")

;;; AgentFinish tool - Sub-agent signals completion

(defun gptel-agent--finish (status result &optional summary)
  "Handle AgentFinish tool call from sub-agent.
STATUS is \"success\", \"error\", or \"partial\".
RESULT is the text to return to the delegating agent.
SUMMARY is an optional one-line summary.
Communicates result back to parent and cleans up.

If no parent callback exists (top-level agent), the result is inserted
into the current buffer as a completion summary instead."
  (let ((parent-cb gptel-agent--parent-callback)
        (parent-buf gptel-agent--parent-buffer)
        (agent-type gptel-agent--agent-type)
        (description gptel-agent--task-description)
        (agent-buffer (current-buffer)))
    ;; Format the response
    (let ((response
           (format "%s result for task: %s\n\nStatus: %s\n%s%s"
                   (capitalize (or agent-type "Agent"))
                   (or description "unknown task")
                   status
                   (if summary (format "Summary: %s\n\n" summary) "\n")
                   result)))
      (cond
       ;; Case 1: Has parent callback - deliver results to parent (normal sub-agent flow)
       (parent-cb
        ;; Nil out callback immediately to prevent double-invocation
        ;; (the DONE handler checks this to know if AgentFinish was called)
        (setq gptel-agent--parent-callback nil)
        ;; Call parent callback with result
        (when (buffer-live-p parent-buf)
          (with-current-buffer parent-buf
            ;; Delete the task overlay in parent buffer
            (dolist (ov (overlays-in (point-min) (point-max)))
              (when (and (overlay-get ov 'gptel-agent)
                         (equal (overlay-get ov 'gptel-agent-buffer) agent-buffer))
                (delete-overlay ov)))
            ;; Invoke the parent callback
            (funcall parent-cb response)))
        ;; Clean up
        (gptel-agent--cleanup-subagent agent-buffer (intern status))
        ;; Return confirmation to the LLM
        (format "Results delivered to delegating agent. Task completed with status: %s" status))
       
       ;; Case 2: No parent callback - top-level agent, display result in current buffer
       (t
        ;; Insert completion summary in current buffer
        (goto-char (point-max))
        (insert "\n\n"
                (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                            'face 'shadow)
                "\n"
                (propertize (format "✓ Workflow Complete (Status: %s)" status)
                            'face (if (equal status "success") 'success 'warning))
                "\n"
                (if summary (format "Summary: %s\n\n" summary) "\n")
                result
                "\n"
                (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                            'face 'shadow)
                "\n")
        ;; Return confirmation to the LLM
        (format "Workflow complete. Status: %s. Results displayed in buffer." status))))))

;;; AskUser tool - Sub-agent requests user input

(defun gptel-agent--ask-user (callback question &optional options context default)
  "Handle AskUser tool call - present question to user in parent buffer.
CALLBACK is called with the user's response when available.
QUESTION is the question to ask.
OPTIONS is an optional vector of suggested choices.
CONTEXT is optional additional context for the user.
DEFAULT is an optional default answer.

If no parent buffer exists (top-level agent), the question is displayed
in the current buffer instead."
  (let ((agent-buffer (current-buffer))
        (parent-buffer gptel-agent--parent-buffer)
        (request-id (gptel-agent--generate-request-id))
        (options-list (when options (append options nil)))) ; Convert vector to list
    (cond
     ;; Parent buffer exists but is dead
     ((and parent-buffer (not (buffer-live-p parent-buffer)))
      (funcall callback "Error: Parent buffer is no longer available."))
     (t
      ;; Use parent buffer if available, otherwise use current buffer (top-level agent)
      (let ((target-buffer (or parent-buffer agent-buffer)))
        ;; Store pending request in this agent buffer
        (push (list :id request-id
                    :callback callback
                    :question question
                    :options options-list
                    :context context
                    :default default
                    :timestamp (current-time))
              gptel-agent--pending-user-requests)
        ;; Display in target buffer
        (with-current-buffer target-buffer
          (gptel-agent--display-user-request 
           agent-buffer request-id question options-list context default))
        ;; Don't call callback yet - it will be called when user responds
        ;; Return nil to indicate we're waiting
        nil)))))

(defun gptel-agent--display-user-request (agent-buffer request-id question options context default)
  "Display a user request prompt in the current (parent) buffer.
AGENT-BUFFER is the sub-agent requesting input.
REQUEST-ID identifies this specific request.
QUESTION is the question to display.
OPTIONS is a list of suggested choices.
CONTEXT is additional context.
DEFAULT is the default answer."
  (let ((inhibit-read-only t)
        (agent-name (buffer-local-value 'gptel-agent--agent-type agent-buffer))
        (task-desc (buffer-local-value 'gptel-agent--task-description agent-buffer)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((start-pos (point))
          (inner-from))
      (insert
       "(" (propertize "AskUser" 'font-lock-face 'font-lock-keyword-face)
       " " (propertize (format "\"%s\"" (or agent-name "sub-agent"))
                       'font-lock-face 'font-lock-escape-face)
       " " (propertize (format "\"%s\"" (truncate-string-to-width
                                         (or task-desc "task") 30 nil nil "…"))
                       'font-lock-face 'font-lock-constant-face)
       ")\n")
      (setq inner-from (point))
      ;; Question
      (insert (propertize question 'font-lock-face 'font-lock-doc-face) "\n")
      ;; Context
      (when context
        (insert "\n"
                (propertize "Context: " 'font-lock-face 'font-lock-comment-face)
                context "\n"))
      ;; Options
      (when options
        (insert "\n"
                (propertize "Options:\n" 'font-lock-face 'font-lock-comment-face))
        (cl-loop for opt in options
                 for i from 1
                 do (insert (propertize (format "  %d. " i)
                                        'font-lock-face 'font-lock-constant-face)
                            opt "\n")))
      ;; Default
      (when default
        (insert "\n"
                (propertize "Default: " 'font-lock-face 'font-lock-comment-face)
                (propertize default 'font-lock-face 'font-lock-string-face)
                "\n"))
      ;; Keybinding hints ABOVE the input area
      (insert "\n"
              (propertize "[ C-c C-c: submit | C-c C-k: cancel ]"
                          'font-lock-face 'font-lock-comment-face)
              "\n\n")
      ;; Response area
      (insert (propertize "Response: " 'font-lock-face 'font-lock-keyword-face))
      (let ((response-start (point))
            (overlay-end-marker (make-marker)))
        (insert "\n")
        ;; User types here
        ;; Set marker at the end to track where overlay should extend
        (set-marker overlay-end-marker (point))
        ;; Apply block background to the content area
        (font-lock-append-text-property
         inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
        ;; Store metadata as text property on the region
        (put-text-property start-pos (point)
                           'gptel-agent-request
                           (list :agent-buffer agent-buffer
                                 :request-id request-id
                                 :response-start response-start
                                 :default default))
        ;; Create overlay for the input area with special keymap
        ;; Use marker for end position so overlay extends as user types
        (let ((ov (make-overlay start-pos overlay-end-marker)))
          (overlay-put ov 'gptel-agent-request-overlay t)
          (overlay-put ov 'gptel-agent-buffer agent-buffer)
          (overlay-put ov 'gptel-request-id request-id)
          (overlay-put ov 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "C-c C-c") #'gptel-agent--submit-user-response)
                         (define-key map (kbd "C-c C-k") #'gptel-agent--cancel-user-response)
                         map)))
        ;; Position cursor for input
        (goto-char response-start)))))

(defun gptel-agent--find-request-at-point ()
  "Find the AskUser request overlay at or near point.
Returns the overlay, or nil if not found."
  (or (cl-find-if (lambda (ov) (overlay-get ov 'gptel-agent-request-overlay))
                  (overlays-at (point)))
      (cl-find-if (lambda (ov) (overlay-get ov 'gptel-agent-request-overlay))
                  (overlays-in (line-beginning-position) (line-end-position)))
      ;; Search backwards for request overlay
      (save-excursion
        (let ((found nil))
          (while (and (not found) (not (bobp)))
            (forward-line -1)
            (setq found (cl-find-if (lambda (ov) (overlay-get ov 'gptel-agent-request-overlay))
                                    (overlays-at (point)))))
          found))))

(defun gptel-agent--submit-user-response ()
  "Submit the user's response to the requesting sub-agent."
  (interactive)
  (let* ((ov (gptel-agent--find-request-at-point)))
    (unless ov
      (user-error "Not in an agent request area"))
    (let* ((agent-buffer (overlay-get ov 'gptel-agent-buffer))
           (request-id (overlay-get ov 'gptel-request-id))
           (props (get-text-property (overlay-start ov) 'gptel-agent-request))
           (response-start (plist-get props :response-start))
           (default (plist-get props :default)))
      ;; Extract response text (from response-start to end of overlay)
      (let ((response (string-trim
                       (buffer-substring-no-properties
                        response-start
                        (overlay-end ov)))))
        ;; Use default if response is empty
        (when (string-empty-p response)
          (setq response (or default "")))
        ;; Clean up the request display
        (let ((inhibit-read-only t))
          (delete-region (overlay-start ov) (overlay-end ov)))
        (delete-overlay ov)
        ;; Insert confirmation
        (insert (propertize (format "[Sent: %s]\n"
                                    (truncate-string-to-width response 50 nil nil "…"))
                            'face 'font-lock-comment-face))
        ;; Deliver response to sub-agent
        (gptel-agent--deliver-user-response agent-buffer request-id response)))))

(defun gptel-agent--cancel-user-response ()
  "Cancel the user request without sending a response."
  (interactive)
  (let* ((ov (gptel-agent--find-request-at-point)))
    (unless ov
      (user-error "Not in an agent request area"))
    (let* ((agent-buffer (overlay-get ov 'gptel-agent-buffer))
           (request-id (overlay-get ov 'gptel-request-id)))
      ;; Clean up the request display
      (let ((inhibit-read-only t))
        (delete-region (overlay-start ov) (overlay-end ov)))
      (delete-overlay ov)
      ;; Insert cancellation notice
      (insert (propertize "[Cancelled]\n" 'face 'font-lock-warning-face))
      ;; Deliver cancellation to sub-agent
      (gptel-agent--deliver-user-response 
       agent-buffer request-id 
       "[User cancelled the request without providing input]"))))

(defun gptel-agent--deliver-user-response (agent-buffer request-id response)
  "Deliver user RESPONSE to AGENT-BUFFER for REQUEST-ID."
  (if (not (buffer-live-p agent-buffer))
      (message "Warning: Sub-agent buffer no longer exists")
    (with-current-buffer agent-buffer
      (let ((request (cl-find request-id gptel-agent--pending-user-requests
                              :key (lambda (r) (plist-get r :id))
                              :test #'equal)))
        (if (not request)
            (message "Warning: Request %s not found in sub-agent" request-id)
          ;; Remove from pending list
          (setq gptel-agent--pending-user-requests
                (cl-remove request gptel-agent--pending-user-requests))
          ;; Call the callback with the user's response
          (funcall (plist-get request :callback) response))))))

;;; Tool confirmation via AskUser for sub-agents

(defun gptel-agent--format-tool-call-for-confirmation (tool-spec arg-values)
  "Format a tool call for user confirmation display.
TOOL-SPEC is the gptel-tool struct.
ARG-VALUES is a list of argument values."
  (let ((name (gptel-tool-name tool-spec))
        (args (gptel-tool-args tool-spec)))
    (concat
     (propertize (format "Tool: %s\n" name) 'face 'font-lock-keyword-face)
     (propertize (format "Description: %s\n" 
                         (truncate-string-to-width 
                          (gptel-tool-description tool-spec) 80 nil nil "…"))
                 'face 'font-lock-doc-face)
     (propertize "Arguments:\n" 'face 'bold)
     (cl-loop for arg in args
              for val in arg-values
              concat (format "  %s: %s\n"
                             (propertize (plist-get arg :name) 'face 'font-lock-variable-name-face)
                             (propertize (truncate-string-to-width 
                                          (gptel--to-string val) 100 nil nil "…")
                                         'face 'font-lock-string-face))))))

(defun gptel-agent--confirm-tool-via-askuser (tool-spec arg-values callback)
  "Request user confirmation for a tool call via AskUser.
TOOL-SPEC is the gptel-tool struct.
ARG-VALUES is a list of argument values.
CALLBACK is called with the user's decision: \"yes\", \"no\", or \"yes-all\"."
  (let* ((tool-name (gptel-tool-name tool-spec))
         (context (gptel-agent--format-tool-call-for-confirmation tool-spec arg-values))
         (question (format "The sub-agent wants to run tool: %s\n\nDo you want to allow this?"
                           tool-name))
         (options ["Yes" "No" "Yes to all (auto-approve remaining tools)"]))
    (gptel-agent--ask-user
     (lambda (response)
       (let ((answer (downcase (string-trim response))))
         (cond
          ((or (string= answer "yes") (string= answer "1") (string= answer "y"))
           (funcall callback "yes"))
          ((or (string-match-p "yes to all\\|3\\|all\\|!" answer))
           (funcall callback "yes-all"))
          (t
           (funcall callback "no")))))
     question options context "Yes")))

(defun gptel-agent--handle-tool-use-with-confirmation (fsm)
  "Handle tool use in sub-agent context, using AskUser for confirmation.
FSM is the gptel state machine.
This replaces `gptel--handle-tool-use' for sub-agent buffers when tools
require confirmation."
  (when-let* ((info (gptel-fsm-info fsm))
              (backend (plist-get info :backend))
              ;; Only act on remaining tool calls without results
              (tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                      (plist-get info :tool-use)))
              (ntools (length tool-use))
              (tool-idx 0))
    (with-current-buffer (plist-get info :buffer)
      (let ((result-alist) (pending-confirms))
        (mapc
         (lambda (tool-call)
           (let* ((args (plist-get tool-call :args))
                  (name (plist-get tool-call :name))
                  (arg-values nil)
                  (tool-spec
                   (cl-find-if
                    (lambda (ts) (equal (gptel-tool-name ts) name))
                    (plist-get info :tools)))
                  (process-tool-result
                   (lambda (result)
                     (plist-put info :tool-success t)
                     (let ((result (gptel--to-string result)))
                       (plist-put tool-call :result result)
                       (push (list tool-spec args result) result-alist))
                     (cl-incf tool-idx)
                     (when (>= tool-idx ntools)
                       (gptel--inject-prompt
                        backend (plist-get info :data)
                        (gptel--parse-tool-results
                         backend (plist-get info :tool-use)))
                       (funcall (plist-get info :callback)
                                (cons 'tool-result result-alist) info)
                       (gptel--fsm-transition fsm)))))
             (if (null tool-spec)
                 (if (equal name gptel--ersatz-json-tool)
                     (funcall (plist-get info :callback)
                              (gptel--json-encode (plist-get tool-call :args))
                              info)
                   (message "Unknown tool called by model: %s" name))
               (setq arg-values
                     (mapcar
                      (lambda (arg)
                        (let ((key (intern (concat ":" (plist-get arg :name)))))
                          (plist-get args key)))
                      (gptel-tool-args tool-spec)))
               ;; Check if we're in a sub-agent and tool requires confirmation
               (let ((needs-confirm
                      (and gptel-agent--parent-buffer
                           gptel-confirm-tool-calls
                           (not gptel-agent--auto-confirm-tools)
                           (or (eq gptel-confirm-tool-calls t)
                               (and-let* ((confirm (gptel-tool-confirm tool-spec)))
                                 (or (not (functionp confirm))
                                     (apply confirm arg-values)))))))
                 (if needs-confirm
                     ;; Use AskUser for confirmation in sub-agent
                     (push (list tool-spec arg-values process-tool-result tool-call)
                           pending-confirms)
                   ;; Run tool directly
                   (if (gptel-tool-async tool-spec)
                       (apply (gptel-tool-function tool-spec)
                              process-tool-result arg-values)
                     (let ((result
                            (condition-case errdata
                                (apply (gptel-tool-function tool-spec) arg-values)
                              (error (mapconcat #'gptel--to-string errdata " ")))))
                       (funcall process-tool-result result))))))))
         tool-use)
        ;; Process pending confirmations one at a time
        (when pending-confirms
          (gptel-agent--process-pending-confirmations 
           (nreverse pending-confirms) fsm info))))))

(defun gptel-agent--process-pending-confirmations (pending-list fsm info)
  "Process PENDING-LIST of tool calls requiring confirmation.
FSM is the state machine, INFO is the request info."
  (if (null pending-list)
      ;; All confirmations processed, check if we need to transition
      (when (plist-get info :tool-success)
        (gptel--fsm-transition fsm))
    (pcase-let ((`(,tool-spec ,arg-values ,process-result ,tool-call) (car pending-list)))
      (gptel-agent--confirm-tool-via-askuser
       tool-spec arg-values
       (lambda (decision)
         (pcase decision
           ("yes"
            ;; Run the tool
            (if (gptel-tool-async tool-spec)
                (apply (gptel-tool-function tool-spec)
                       (lambda (result)
                         (funcall process-result result)
                         (gptel-agent--process-pending-confirmations
                          (cdr pending-list) fsm info))
                       arg-values)
              (let ((result
                     (condition-case errdata
                         (apply (gptel-tool-function tool-spec) arg-values)
                       (error (mapconcat #'gptel--to-string errdata " ")))))
                (funcall process-result result)
                (gptel-agent--process-pending-confirmations
                 (cdr pending-list) fsm info))))
           ("yes-all"
            ;; Enable auto-confirm for remaining tools
            (setq gptel-agent--auto-confirm-tools t)
            ;; Run the tool
            (if (gptel-tool-async tool-spec)
                (apply (gptel-tool-function tool-spec)
                       (lambda (result)
                         (funcall process-result result)
                         (gptel-agent--process-pending-confirmations
                          (cdr pending-list) fsm info))
                       arg-values)
              (let ((result
                     (condition-case errdata
                         (apply (gptel-tool-function tool-spec) arg-values)
                       (error (mapconcat #'gptel--to-string errdata " ")))))
                (funcall process-result result)
                (gptel-agent--process-pending-confirmations
                 (cdr pending-list) fsm info))))
           ("no"
            ;; Reject the tool call
            (plist-put tool-call :result "[Tool call rejected by user]")
            (plist-put info :tool-success t)
            (gptel-agent--process-pending-confirmations
             (cdr pending-list) fsm info))))))))

;;; Tool use preview
(defun gptel-agent--confirm-overlay (from to &optional no-hide)
  "Set up tool call preview overlay FROM TO.

If NO-HIDE is non-nil, don't hide the overlay body by default."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'gptel-agent-tool t)
    (overlay-put ov 'priority 10)
    (overlay-put ov 'keymap
                 (make-composed-keymap
                  (define-keymap
                    "n"     'gptel-agent--next-overlay
                    "p"     'gptel-agent--previous-overlay
                    "q"     'gptel--reject-tool-calls
                    "<tab>" 'gptel-agent--cycle-overlay
                    "TAB"   'gptel-agent--cycle-overlay)
                  gptel-tool-call-actions-map))
    (unless no-hide
      (gptel-agent--cycle-overlay ov))
    ov))

(defun gptel-agent--cycle-overlay (ov)
  "Cycle tool call preview overlay OV at point."
  (interactive (list (cdr (get-char-property-and-overlay
                           (point) 'gptel-agent-tool))))
  (save-excursion
    (goto-char (overlay-start ov))
    (let ((line-end (line-end-position))
          (end      (overlay-end ov)))
      (pcase-let ((`(,value . ,hide-ov)
                   (get-char-property-and-overlay line-end 'invisible)))
        (if (and hide-ov (eq value t))
            (delete-overlay hide-ov)
          (unless hide-ov (setq hide-ov (make-overlay line-end (1- end) nil t)))
          (overlay-put hide-ov 'evaporate t)
          (overlay-put hide-ov 'invisible t)
          (overlay-put hide-ov 'before-string " ▼"))))))

(defun gptel-agent--next-overlay ()
  "Jump to the next `gptel-agent' tool overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'gptel-agent-tool)))
              (end (overlay-end ov)))
    (when (get-char-property end 'gptel-tool)
      (goto-char end))))

(defun gptel-agent--previous-overlay ()
  "Jump to the previous `gptel-agent' tool overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (1- (point)) 'gptel-agent-tool))))
    (goto-char (overlay-start ov))))

(defsubst gptel-agent--block-bg ()
  "Return a background face suitable for displaying code."
  (cond
   ((derived-mode-p 'org-mode) 'org-block)
   ((derived-mode-p 'markdown-mode) 'markdown-code-face)
   (t `( :background ,(face-attribute 'mode-line-inactive :background)
         :extend t))))

(defun gptel-agent--fontify-block (path-or-mode start end)
  "Fontify region from START to END.

Fontification is assuming it is the contents of file PATH-OR-MODE (if it
is a string), or major-mode (if it is a symbol).  Applied font-lock-face
properties persist through refontification."
  (let ((lang-mode)                     ; (org-src-get-lang-mode lang)
        (org-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties org-buffer start end)
      (insert " ")                      ; Add space to ensure property change
      (delay-mode-hooks
        (if (symbolp path-or-mode)
            (setq lang-mode path-or-mode)
          (let ((buffer-file-name path-or-mode))
            (setq lang-mode
                  (or (cdr (assoc-string
                            (concat
                             "\\." (file-name-extension path-or-mode) "\\'")
                            auto-mode-alist))
                      (progn (set-auto-mode t) major-mode)))))
        (funcall lang-mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (1- (point-max))) ; Skip the added space
          (let* ((next (next-property-change pos nil (1- (point-max))))
                 (face-prop (get-text-property pos 'face)))
            (when face-prop
              (put-text-property
               (+ start (- pos (point-min)))
               (+ start (- (or next (1- (point-max))) (point-min)))
               'font-lock-face face-prop org-buffer))
            (setq pos (or next (1- (point-max))))))))))

;;; System tools
;; "Execute Bash commands to inspect files and system state.

;; This tool provides access to a Bash shell with GNU coreutils (or equivalents)
;; available. You can use any standard Linux commands including: cd, ls, file, cat,
;; grep, awk, sed, head, tail, wc, find, sort, uniq, cut, tr, and more.

;; PURPOSE:
;; - Efficiently inspect files and system state WITHOUT consuming excessive
;; tokens. This is preferred over reading entire large files.
;; - Modify files or system state as appropriate, using cp, mv, rm, patch,
;; git subcommands (git log, commit, branch and more) and so on.

;; BEST PRACTICES:
;; - Use pipes to combine commands: 'cat file.log | grep ERROR | tail -20'
;; - For large files, use head/tail: 'head -50 file.txt' or 'tail -100 file.log'
;; - Use grep with context: 'grep -A 5 -B 5 pattern file.txt'
;; - Check file sizes first: 'wc -l file.txt' before reading
;; - Use file command to identify file types: 'file *'
;; - Combine with other tools: 'find . -name \"*.el\" | head -10'

;; EXAMPLES:
;; - List files with details: 'ls -lah /path/to/dir'
;; - Print lines 25-35 of a long file/stream: 'sed -n \"25,35p\" app.log'
;; - Find recent errors: 'grep -i error /var/log/app.log | tail -20'
;; - Check file type: 'file document.pdf'
;; - Count lines: 'wc -l *.txt'
;; - Search with context: 'grep -A 3 \"function foo\" script.sh'

;; The command will be executed in the current working directory. Output is
;; returned as a string. Long outputs should be filtered/limited using pipes."

;; - Can run commands in background with `run_in_background: true`
;; - Default timeout is 2 minutes (120000ms), max is 10 minutes

(defun gptel-agent--eval-elisp-preview-setup (arg-values _info)
  "Setup preview overlay for Elisp evaluation tool call.

ARG-VALUES is the list of arguments for the tool call."
  (let ((expr (car arg-values))
        (from (point)) (inner-from))
    (insert
     "(" (propertize "Eval" 'font-lock-face 'font-lock-keyword-face)
     ")\n")
    (setq inner-from (point))
    (insert expr)
    (gptel-agent--fontify-block 'emacs-lisp-mode inner-from (point))
    ;; (add-text-properties inner-from (point) '(line-prefix "  " wrap-prefix "  "))
    (insert "\n\n")
    (font-lock-append-text-property
     inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--execute-bash-preview-setup (arg-values _info)
  "Setup preview overlay for Bash command execution tool call.

ARG-VALUES is the list of arguments for the tool call."
  (let ((command (car arg-values))
        (from (point)) (inner-from))
    (insert
     "(" (propertize "Bash" 'font-lock-face 'font-lock-keyword-face)
     ")\n")
    (setq inner-from (point))
    (insert command)
    (gptel-agent--fontify-block 'sh-mode inner-from (point))
    (insert "\n\n")
    (font-lock-append-text-property
     inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--execute-bash (callback command)
  "Execute COMMAND asynchronously in bash and call CALLBACK with output.

CALLBACK is called with the command output string when the process finishes.
COMMAND is the bash command string to execute."
  (let* ((output-buffer (generate-new-buffer " *gptel-agent-bash*"))
         (proc (make-process
                :name "gptel-agent-bash"
                :buffer output-buffer
                :command (list "bash" "-c" command)
                :connection-type 'pipe
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (let* ((exit-code (process-exit-status process))
                           (output (with-current-buffer (process-buffer process)
                                     (buffer-string))))
                      (kill-buffer (process-buffer process))
                      (funcall callback
                               (if (zerop exit-code)
                                   output
                                 (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
                                         exit-code output)))))))))
    proc))

;;; Web tools

(defun gptel-agent--fetch-with-timeout (url url-cb tool-cb failed-msg &rest args)
  "Fetch URL and call URL-CB in the result buffer.

Call TOOL-CB if there is an error or a timeout.  TOOL-CB and ARGS are
passed to URL-CB.  FAILED-MSG is a fragment used for messaging.  Handles
cleanup."
  (let* ((timeout 30) timer done
         (inherit-process-coding-system t)
         (proc-buffer
          (url-retrieve
           url (lambda (status)
                 (setq done t)
                 (when timer (cancel-timer timer))
                 (if-let* ((err (plist-get status :error)))
                     (funcall tool-cb
                              (format "Error: %s failed with error: %S" failed-msg err))
                   (apply url-cb tool-cb args))
                 (kill-buffer (current-buffer)))
           args 'silent)))
    (setq timer
          (run-at-time
           timeout nil
           (lambda (buf cb)
             (unless done
               (setq done t)
               (let ((kill-buffer-query-functions)) (kill-buffer buf))
               (funcall
                cb (format "Error: %s timed out after %d seconds."
                           failed-msg timeout))))
           proc-buffer tool-cb))
    proc-buffer))

;;;; Web search
(defun gptel-agent--shr-next-link ()
  "Jump to the next SHR link in the buffer.  Return jump position."
  (let ((current-prop (get-char-property (point) 'shr-url))
        (next-pos (point)))
    (while (and (not (eobp))
                (setq next-pos
                      (or (next-single-property-change (point) 'shr-url)
                          (point-max)))
                (let ((next-prop (get-char-property next-pos 'shr-url)))
                  (or (equal next-prop current-prop)
                      (equal next-prop nil))))
      (goto-char next-pos))
    (goto-char next-pos)))

(defvar gptel-agent--web-search-active nil)

(defun gptel-agent--web-search-eww (tool-cb query &optional count)
  "Search the web using eww's default search engine (usually DuckDuckGo).

Call TOOL-CB with the results as a string.  QUERY is the search string.
COUNT is the number of results to return (default 5)."
  ;; No more than two active searches at one time
  (setq gptel-agent--web-search-active
        (cl-delete-if-not
         (lambda (buf) (and (buffer-live-p buf)
                       (process-live-p (get-buffer-process buf))))
         gptel-agent--web-search-active))
  (if (>= (length gptel-agent--web-search-active) 2)
      (progn (message "Web search: waiting for turn")
             (run-at-time 5 nil #'gptel-agent--web-search-eww
                          tool-cb query count))
    (push (gptel-agent--fetch-with-timeout
           (concat eww-search-prefix (url-hexify-string query))
           #'gptel-agent--web-search-eww-callback
           tool-cb (format "Web search for \"%s\"" query))
          gptel-agent--web-search-active)))

(defun gptel-agent--web-fix-unreadable ()
  "Replace invalid characters from point to end in current buffer."
  (while (and (skip-chars-forward "\0-\x3fff7f")
              (not (eobp)))
    (display-warning
     '(gptel gptel-agent-tools)
     (format "Invalid character in buffer \"%s\"" (buffer-name)))
    (delete-char 1) (insert "?")))

(defun gptel-agent--web-search-eww-callback (cb)
  "Extract website text and run callback CB with it."
  (let* ((count 5) (results))
    (goto-char (point-min))
    (goto-char url-http-end-of-headers)
    ;; (gptel-agent--web-fix-unreadable)
    (let* ((dom (libxml-parse-html-region (point) (point-max)))
           (result-count 0))
      (eww-score-readability dom)
      ;; (erase-buffer) (buffer-disable-undo)
      (with-temp-buffer
        (shr-insert-document (eww-highest-readability dom))
        (goto-char (point-min))
        (while (and (not (eobp)) (< result-count count))
          (let ((pos (point))
                (url (get-char-property (point) 'shr-url))
                (next-pos (gptel-agent--shr-next-link)))
            (when-let* (((stringp url))
                        (idx (string-search "http" url))
                        (url-fmt (url-unhex-string (substring url idx))))
              (cl-incf result-count)
              (push (concat url-fmt "\n\n"
                            (string-trim
                             (buffer-substring-no-properties pos next-pos))
                            "\n\n----\n")
                    results))))))
    (funcall cb (apply #'concat (nreverse results)))))

;;;; Read URLs
(defun gptel-agent--read-url (tool-cb url)
  "Fetch URL text and call TOOL-CB with it."
  (gptel-agent--fetch-with-timeout
   url
   (lambda (cb)
     (goto-char (point-min)) (forward-paragraph)
     (condition-case errdata
         (let ((dom (libxml-parse-html-region (point) (point-max))))
           (with-temp-buffer
             (eww-score-readability dom)
             (shr-insert-document (eww-highest-readability dom))
             (decode-coding-region (point-min) (point-max) 'utf-8)
             (funcall
              cb (buffer-substring-no-properties
                  (point-min) (point-max)))))
       (error (funcall cb (format "Error: Request failed with error data:\n%S"
                                  errdata)))))
   tool-cb (format "Fetch for \"%s\"" url)))

;;;; Fetch youtube transcript
(defun gptel-agent--yt-parse-captions (xml-string)
  "Parse YouTube caption XML-STRING and return DOM."
  (with-temp-buffer
    (insert xml-string)
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    ;; Clean up the XML
    (dolist (reps '(("\n" . " ")
                    ("&amp;" . "&")
                    ("&quot;" . "\"")
                    ("&#39;" . "'")
                    ("&lt;" . "<")
                    ("&gt;" . ">")))
      (save-excursion
        (while (search-forward (car reps) nil t)
          (replace-match (cdr reps) nil t))))
    (libxml-parse-xml-region (point-min) (point-max))))

(defun gptel-agent--yt-format-captions (caption-dom &optional chunk-time)
  "Format CAPTION-DOM as paragraphs with timestamps.

CHUNK-TIME is the number of seconds per paragraph (default 30)."
  (when (and (listp caption-dom)
             (eq (car-safe caption-dom) 'transcript))
    (let ((chunk-time (or chunk-time 30))
          (result "")
          (current-para "")
          (para-start-time 0))
      (dolist (elem (cddr caption-dom)) ;; Process each text element
        (when (and (listp elem) (eq (car elem) 'text))
          (let* ((attrs (cadr elem))
                 (text (caddr elem))
                 (start (string-to-number (cdr (assoc 'start attrs))))
                 ;; Check if we've crossed into a new chunk-time boundary
                 (should-chunk (and (> (abs (- start para-start-time)) 3)
                                    (not (= (floor para-start-time chunk-time)
                                            (floor start chunk-time))))))
            (when (and should-chunk (> (length current-para) 0))
              ;; Add completed paragraph
              (setq result (concat result
                                   (format "[%d:%02d]\n%s\n\n"
                                           (floor para-start-time 60)
                                           (mod para-start-time 60)
                                           (string-trim current-para))))
              (setq current-para "")
              (setq para-start-time start))

            (when text
              (setq current-para (concat current-para " " text))))))

      ;; Add final paragraph
      (when (> (length current-para) 0)
        (setq result (concat result
                             (format "[%d:%02d]\n%s\n\n"
                                     (floor para-start-time 60)
                                     (mod para-start-time 60)
                                     (string-trim current-para)))))
      result)))

(defun gptel-agent--yt-fetch-watch-page (callback video-id)
  "Step 1: Fetch YouTube watch page for VIDEO-ID.

Call CALLBACK with error or proceeds to fetch InnerTube data."
  (url-retrieve
   (format "https://youtube.com/watch?v=%s" video-id)
   (lambda (status callback video-id)
     (if-let ((error (plist-get status :error)))
         (funcall callback (format "Error fetching page: %s" error))
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (let* ((html (buffer-substring (point) (point-max)))
              (api-key (and (string-match
                             "\"INNERTUBE_API_KEY\":\"\\([a-zA-Z0-9_-]+\\)"
                             html)
                            (match-string 1 html))))
         (if api-key
             (gptel-agent--yt--fetch-innertube callback video-id api-key)
           (funcall callback "Error: Could not extract API key")))))
   (list callback video-id)))

(defun gptel-agent--yt--fetch-innertube (callback video-id api-key)
  "Step 2: Fetch VIDEO-ID metadata from YouTube InnerTube API.

Call CALLBACK with error or proceeds to fetch captions."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")
           ("Accept-Language" . "en-US")))
        (url-request-data
         (encode-coding-string
          (json-encode
           `((context . ((client . ((clientName . "ANDROID")
                                    (clientVersion . "20.10.38")))))
             (videoId . ,video-id)))
          'utf-8)))
    (url-retrieve
     (format "https://www.youtube.com/youtubei/v1/player?key=%s" api-key)
     (lambda (status callback)
       (if-let ((error (plist-get status :error)))
           (funcall callback (format "Error fetching metadata: %s" error))
         (goto-char (point-min))
         (search-forward "\n\n" nil t)
         (let* ((json-data (ignore-errors
                             (json-parse-buffer :object-type 'plist)))
                (video-details (plist-get json-data :videoDetails))
                (description (plist-get video-details :shortDescription))
                (caption-tracks (map-nested-elt
                                 json-data
                                 '(:captions
                                   :playerCaptionsTracklistRenderer
                                   :captionTracks))))
           (gptel-agent--yt-fetch-captions callback description caption-tracks))))
     (list callback))))

(defun gptel-agent--yt-fetch-captions (callback description caption-tracks)
  "Step 3: Find and fetch English captions for CAPTION-TRACKS.

Call CALLBACK with formatted result containing DESCRIPTION and transcript."
  (if (not caption-tracks)
      (funcall callback
               (format "# Description\n\n%s\n\n# Transcript\n\nNo transcript available."
                       (or description "No description available.")))
    (let ((en-caption
           (cl-find-if
            (lambda (track)
              (string-match-p "^en" (or (plist-get track :languageCode) "")))
            caption-tracks)))
      (if (not en-caption)
          (funcall callback
                   (format "# Description\n\n%s\n\n# Transcript\n\nNo English transcript available."
                           (or description "No description available.")))
        (let ((base-url (replace-regexp-in-string
                         "&fmt=srv3" ""
                         (plist-get en-caption :baseUrl))))
          (url-retrieve
           base-url
           (lambda (status callback description)
             (if-let ((error (plist-get status :error)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\nError fetching transcript: %s"
                                  (or description "No description available.")
                                  error))
               (goto-char (point-min))
               (search-forward "\n\n" nil t)
               (let* ((xml-string (buffer-substring (point) (point-max)))
                      (caption-dom (gptel-agent--yt-parse-captions xml-string))
                      (formatted-transcript
                       (gptel-agent--yt-format-captions caption-dom 30)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\n%s"
                                  (or description "No description available.")
                                  (or formatted-transcript "Error parsing transcript."))))))
           (list callback description)))))))

(defun gptel-agent--yt-read-url (callback url)
  "Fetch YouTube metadata and transcript for URL, calling CALLBACK with result.
CALLBACK is called with a markdown-formatted string containing the video
description and transcript formatted as timestamped paragraphs."
  (if-let* ((video-id
             (and (string-match
                   (rx bol (opt "http" (opt "s") "://")
                       (opt "www.") "youtu" (or ".be" "be.com") "/"
                       (opt "watch?v=")
                       (group (one-or-more (not (any "?&")))))
                   url)
                  (match-string 1 url))))
      (gptel-agent--yt-fetch-watch-page callback video-id)
    (funcall callback "Error: Invalid YouTube URL")))

;;; Code tools
;;;; Diagnostics from flymake
(declare-function flymake--project-diagnostics "flymake")
(declare-function flymake--diag-beg "flymake")
(declare-function flymake--diag-type "flymake")
(declare-function flymake--diag-text "flymake")
(declare-function flymake-diagnostic-buffer "flymake")

(defun gptel-agent--flymake-diagnostics (&optional all)
  "Collect flymake errors across all open buffers in the current project.

Errors with low severity are not collected.  With ALL, collect all
diagnostics."
  (let ((project (project-current)))
    (unless project
      (error "Not in a project.  Cannot collect flymake diagnostics"))
    (require 'flymake)
    (let ((results '()))
      (dolist (diag (flymake--project-diagnostics project))
        (let ((severity (flymake--diag-type diag)))
          (when (memq severity `(:error :warning ,@(and all '(:note))))
            (with-current-buffer (flymake-diagnostic-buffer diag)
              (let* ((beg (flymake--diag-beg diag))
                     (line-num (line-number-at-pos beg))
                     (line-text (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))
                (push (format "File: %s:%d\nSeverity: %s\nMessage: %s\n---\n%s"
                              (buffer-file-name)
                              line-num
                              severity
                              (flymake--diag-text diag)
                              line-text)
                      results))))))
      (string-join (nreverse results) "\n\n"))))

;;; Filesystem tools
;;;; Make directories
;;;; Writing to files
(defun gptel-agent--edit-files-preview-setup (arg-values _info)
  "Insert tool call preview for ARG-VALUES for \"Edit\" tool."
  (pcase-let ((from (point)) (files-affected) (description)
              (`(,path ,old-str ,new-str-or-diff ,diffp) arg-values))

    (if (and diffp (not (eq diffp :json-false)))
        (progn                          ;Patch
          (insert new-str-or-diff)
          (save-excursion
            (while (re-search-backward "^\\+\\+\\+ \\(.*\\)$" from t)
              (push (match-string 1) files-affected))
            (goto-char from)
            (when (looking-at "^ *```\\(diff\\|patch\\)\\s-*\n")
              (delete-region (match-beginning 0) (match-end 0))))
          (skip-chars-backward " \t\r\n") (forward-line 0)
          (when (looking-at-p "^ *```\\s-*\\'")
            (delete-region (line-beginning-position) (line-end-position)))
          (setq description "Patch")
          (require 'diff-mode)
          (gptel-agent--fontify-block 'diff-mode from (point)))
      (when old-str                     ;Text replacement
        (push path files-affected)
        (setq description "ReplaceIn")
        (insert
         (propertize old-str 'font-lock-face 'diff-removed
                     'line-prefix (propertize "-" 'face 'diff-removed))
         "\n" (propertize new-str-or-diff 'font-lock-face 'diff-added
                          'line-prefix (propertize "+" 'face 'diff-added))
         "\n")))
    (insert "\n")
    (font-lock-append-text-property
     from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (when (derived-mode-p 'org-mode)
      (org-escape-code-in-region from (1- (point))))
    (save-excursion
      (goto-char from)
      (insert
       "(" (propertize description 'font-lock-face 'font-lock-keyword-face)
       " " (mapconcat (lambda (f) (propertize (concat "\"" f "\"")
                                         'font-lock-face 'font-lock-constant-face))
                      files-affected " ")
       ")\n"))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--fix-patch-headers ()
  "Fix line numbers in hunks in diff at point."
  ;; Find and process each hunk header
  (while (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) +\\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
    (let ((hunk-start (line-beginning-position))
          (orig-line (string-to-number (match-string 1)))
          (new-line (string-to-number (match-string 3)))
          (orig-count 0)
          (new-count 0))

      ;; Count lines in this hunk until we hit the next @@ or EOF
      (goto-char hunk-start)
      (forward-line 1)
      (save-match-data
        (while (and (not (eobp))
                    (not (looking-at-p "^@@")))
          (cond
           ;; Removed lines (not ---)
           ((looking-at-p "^-[^-]")
            (cl-incf orig-count))
           ;; Added lines (not +++)
           ((looking-at-p "^\\+[^+]")
            (cl-incf new-count))
           ;; Context lines (space at start)
           ((looking-at-p "^ ")
            (cl-incf orig-count)
            (cl-incf new-count)))
          (forward-line 1)))

      ;; Replace the hunk header with corrected counts
      (goto-char hunk-start)
      (delete-line)
      (insert (format "@@ -%d,%d +%d,%d @@\n"
                      orig-line orig-count new-line new-count)))))

;;;; Create a directory
(defun gptel-agent--make-directory (parent name)
  "Create a directory NAME in PARENT directory.

Creates the directory and any missing parent directories.  If the
directory already exists, this is a no-op and returns success.

PARENT is the parent directory path,NAME is the name of the new
directory to create."
  (condition-case errdata
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error "Error creating directory %s in %s:\n%S" name parent errdata)))

(defun gptel-agent--edit-files (path &optional old-str new-str-or-diff diffp)
  "Replace text in file(s) at PATH using either string matching or unified diff.

This function supports two distinct modes of operation:

1. STRING REPLACEMENT MODE (DIFFP is nil or :json-false):
   - Searches for OLD-STR in the file at PATH
   - Replaces it with NEW-STR-OR-DIFF
   - Requires OLD-STR to match exactly once (uniquely) in the file
   - Only works on single files, not directories

2. DIFF/PATCH MODE (when DIFFP is non-nil and not :json-false):
   - Applies NEW-STR-OR-DIFF as a unified diff using the `patch` command
   - Works on both single files and directories
   - OLD-STR is ignored in this mode
   - NEW-STR-OR-DIFF can contain the diff in fenced code blocks
     (=diff or =patch)
   - Uses the -N (--forward) option to ignore already-applied patches

PATH - File or directory path to modify (must be readable)
OLD-STR - (String mode only) Exact text to find and replace
NEW-STR-OR-DIFF - Replacement text (string mode) or unified diff (diff mode)
DIFFP - If non-nil (and not :json-false), use diff/patch mode

Error Conditions:
  - PATH not readable
  - (String mode) PATH is a directory
  - (String mode) OLD-STR not found in file
  - (String mode) OLD-STR matches multiple times (ambiguous)
  - (Diff mode) patch command fails (exit status non-zero)

Returns:
  Success message string describing what was changed

Signals:
  error - On any failure condition (caught and displayed by gptel)"
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))

  (unless new-str-or-diff
    (error "Required argument `new_str' missing"))

  (if (or (eq diffp :json-false) old-str)
      ;; Replacement by Text
      (progn
        (when (file-directory-p path)
          (error "Error: String replacement is intended for single files, not directories (%s)"
                 path))
        (with-temp-buffer
          (insert-file-contents path)
          (if (search-forward old-str nil t)
              (if (save-excursion (search-forward old-str nil t))
                  (error "Error: Match is not unique.\
Consider providing more context for the replacement, or a unified diff")
                ;; TODO: More robust backspace escaping
                (replace-match (string-replace  "\\" "\\\\" new-str-or-diff))
                (write-region nil nil path)
                (format "Successfully replaced %s (truncated) with %s (truncated)"
                        (truncate-string-to-width old-str 20 nil nil t)
                        (truncate-string-to-width new-str-or-diff 20 nil nil t)))
            (error "Error: Could not find old_str \"%s\" in file %s"
                   (truncate-string-to-width old-str 20) path))))
    ;; Replacement by Diff
    (unless (executable-find "patch")
      (error "Error: Command \"patch\" not available, cannot apply diffs.\
Use string replacement instead"))
    (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
           ;; (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
           (target-file (expand-file-name path))
           (exit-status -1)             ; Initialize to a known non-zero value
           (result-output "")
           ;; (result-error "")
           )
      (unwind-protect
          (let ((default-directory (file-name-directory (expand-file-name path)))
                (patch-options    '("--forward" "--verbose")))

            (with-temp-message
                (format "Applying diff to: `%s` with options: %s"
                        target-file patch-options)
              (with-temp-buffer
                (insert new-str-or-diff)
                ;; Insert trailing newline, required by patch
                (unless (eq (char-before (point-max)) ?\n)
                  (goto-char (point-max))
                  (insert "\n"))
                (goto-char (point-min))
                ;; Remove code fences, if present
                (when (looking-at-p "^ *```diff\n")
                  (save-excursion
                    (delete-line)
                    (goto-char (point-max))
                    (forward-line -1)   ;guaranteed to be at a blank newline
                    (when (looking-at-p "^ *```") (delete-line))))
                ;; Fix line numbers in hunk headers
                (gptel-agent--fix-patch-headers)

                (setq exit-status
                      (apply #'call-process-region (point-min) (point-max)
                             "patch" nil (list out-buf-name t) ; stdout/stderr buffer names
                             nil patch-options))))

            ;; Retrieve content from buffers using their names
            (when-let* ((stdout-buf (get-buffer out-buf-name)))
              (when (buffer-live-p stdout-buf)
                (with-current-buffer stdout-buf
                  (setq result-output (buffer-string)))))

            (if (= exit-status 0)
                (format "Diff successfully applied to %s.
Patch command options: %s
Patch STDOUT:\n%s"
                        target-file patch-options result-output)
              ;; Signal an Elisp error, which gptel will catch and display.
              ;; The arguments to 'error' become the error message.
              (error "Error: Failed to apply diff to %s (exit status %s).
Patch command options: %s
Patch STDOUT:\n%s"
                     target-file exit-status patch-options
                     result-output)))
        (let ((stdout-buf-obj (get-buffer out-buf-name))) ;Clean up
          (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj)))))))

(defun gptel-agent--insert-in-file-preview-setup (arg-values _info)
  "Preview setup for Insert.
INFO is the tool call info plist.
ARG-VALUES is a list: (path line-number new-str)"
  (let ((from (point)) (line-offset)
        (face-bg (gptel-agent--block-bg))
        (cb (current-buffer)))
    (pcase-let ((`(,path ,line-number ,new-str) arg-values))
      (insert "("
              (propertize "insert_into_file " 'font-lock-face 'font-lock-keyword-face)
              (propertize (concat "\"" path "\"")
                          'font-lock-face 'font-lock-constant-face)
              ")\n")
      (if (file-readable-p path)
          (insert
           (with-temp-buffer       ;NEW-STR with context lines, styled as a diff
             (insert-file-contents path)
             (pcase line-number
               (-1 (goto-char (point-max)))
               (_ (forward-line line-number)))
             (save-excursion
               (forward-line -6)
               (setq line-offset (line-number-at-pos))
               (delete-region (point-min) (point))
               (dotimes (_ 12)
                 (put-text-property
                  (line-beginning-position) (line-end-position)
                  'line-prefix (propertize (format "%4d " line-offset) 'face
                                           `(:inherit ,face-bg :inherit line-number)))
                 (forward-line 1) (when (eolp) (insert " "))
                 (cl-incf line-offset)))
             (insert (propertize new-str 'font-lock-face 'diff-added
                                 'fontified t 'font-lock-multiline t
                                 'line-prefix (propertize "   + " 'face 'diff-added)))
             (save-excursion
               (forward-line 6)
               (delete-region (point) (point-max)))
             (font-lock-append-text-property
              (point-min) (point-max) 'font-lock-face face-bg)
             (when (provided-mode-derived-p
                    (buffer-local-value 'major-mode cb) 'org-mode)
               (org-escape-code-in-region (point-min) (point-max)))
             (buffer-string)) "\n")
        (insert (propertize "[File not readable]\n\n" 'font-lock-face 'error)))
      (gptel-agent--confirm-overlay from (point)))))

(defun gptel-agent--insert-in-file (path line-number new-str)
  "Insert NEW-STR at LINE-NUMBER in file at PATH.

LINE-NUMBER conventions:
- 0 inserts at the beginning of the file
- -1 inserts at the end of the file
- N > 1 inserts before line N"
  (unless (file-readable-p path)
    (error "Error: File %s is not readable" path))

  (when (file-directory-p path)
    (error "Error: Cannot insert into directory %s" path))

  (with-temp-buffer
    (insert-file-contents path)

    (pcase line-number
      (0 (goto-char (point-min)))       ; Insert at the beginning
      (-1 (goto-char (point-max)))      ; Insert at the end
      (_ (goto-char (point-min))
         (forward-line line-number)))   ; Insert before line N

    ;; Insert the new string
    (insert new-str)

    ;; Ensure there's a newline after the inserted text if not already present
    (unless (or (string-suffix-p "\n" new-str) (eobp))
      (insert "\n"))

    ;; Write the modified content back to the file
    (write-region nil nil path)

    (format "Successfully inserted text at line %d in %s" line-number path)))

(defun gptel-agent--write-file-preview-setup (arg-values _info)
  "Setup preview overlay for Write file tool call.

ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,path ,filename ,content) arg-values))
    (insert
     "(" (propertize "Write " 'font-lock-face 'font-lock-keyword-face)
     (propertize (prin1-to-string path) 'font-lock-face 'font-lock-constant-face) " "
     (propertize (prin1-to-string filename) 'font-lock-face 'font-lock-constant-face)
     ")\n")
    (let ((inner-from (point)))
      (insert content)
      (gptel-agent--fontify-block filename inner-from (point))
      (insert "\n\n")
      (font-lock-append-text-property
       inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
      (when (derived-mode-p 'org-mode)
        (org-escape-code-in-region inner-from (1- (point)))))
    (gptel-agent--confirm-overlay from (point))))

;;;; Write content to a file
(defun gptel-agent--write-file (path filename content)
  "Write CONTENT to FILENAME in PATH.

PATH and FILENAME are expanded to create the full path.  CONTENT is
written to the file.  Returns a success message string, or signals an
error if writing fails.

PATH, FILENAME, and CONTENT must all be strings."
  (unless (and (stringp path) (stringp filename) (stringp content))
    (error "PATH, FILENAME or CONTENT is not a string, cancelling action"))
  (let ((full-path (expand-file-name filename path)))
    (condition-case errdata
        (with-temp-buffer
          (insert content)
          (write-file full-path)
          (format "Created file %s in %s" filename path))
      (error "Error: Could not write file %s:\n%S" path errdata))))

;;;; Find files using regexes
(defun gptel-agent--glob (pattern &optional path depth)
  "Find files matching PATTERN using the `tree' command.

PATTERN is a case-insensitive regex pattern to match filenames against.
PATH is the optional directory to search (defaults to current directory).
DEPTH limits recursion depth when provided (non-negative integer).

Returns a string listing matching files with full paths, sorted by
modification time.  If the output is too large (>20000 chars), it writes
the full results to a temporary file and returns a truncated version with
instructions to use `Read' for the full contents.

Raises an error if PATTERN is empty, PATH is not readable, or the
`tree' executable is not found."
  (when (string-empty-p pattern)
    (error "Error: pattern must not be empty"))
  (if path
      (unless (and (file-readable-p path) (file-directory-p path))
        (error "Error: path %s is not readable" path))
    (setq path "."))
  (unless (executable-find "tree")
    (error "Error: Executable `tree` not found.  This tool cannot be used"))
  (let ((full-path (expand-file-name path)))
    (with-temp-buffer
      (let* ((args (list "-l" "-f" "-i" "-I" ".git"
                         "--sort=mtime" "--ignore-case"
                         "--prune" "-P" pattern full-path))
             (args (if (natnump depth)
                       (nconc args (list "-L" (number-to-string depth)))
                     args))
             (exit-code (apply #'call-process "tree" nil t nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Glob failed with exit code %d\n.STDOUT:\n\n"
                          exit-code))))
      (when (> (buffer-size) 20000)
        ;; Too large - save to temp file and return truncated info
        (let* ((temp-dir (expand-file-name "gptel-agent-temp"
                                           (temporary-file-directory)))
               (temp-file (expand-file-name
                           (format "glob-%s-%s.txt"
                                   (format-time-string "%Y%m%d-%H%M%S")
                                   (random 10000))
                           temp-dir)))
          (unless (file-directory-p temp-dir) (make-directory temp-dir t))
          (write-region nil nil temp-file)
          (let ((max-lines 50)
                (orig-size (buffer-size))
                (orig-lines (line-number-at-pos (point-max))))
            ;; Insert header
            (goto-char (point-min))
            (insert (format "Glob results too large (%d chars, %d lines)\
 for context window.\nStored in: %s\n\nFirst %d lines:\n\n"
                            orig-size orig-lines temp-file max-lines))
            ;; Truncate to first max-lines lines
            (forward-line max-lines)
            (delete-region (point) (point-max))
            ;; Insert footer
            (goto-char (point-max))
            (insert (format "\n\n[Use Read tool with file_path=\"%s\" to view full results]"
                            temp-file)))))
      (buffer-string))))

;;;; Read files or directories
(defun gptel-agent--read-file-lines (filename start-line end-line)
  "Return lines START-LINE to END-LINE fom FILENAME."
  (unless (file-readable-p filename)
    (error "Error: File %s is not readable" filename))

  (when (file-directory-p filename)
    (error "Error: Cannot read directory %s as file" filename))

  (when (file-symlink-p filename)
    (setq filename (file-truename filename)))

  (if (and (not start-line) (not end-line)) ;read full file
      (if (> (file-attribute-size (file-attributes filename))
             (* 512 1024))
          (error "Error: File is too large (> 512 KB).\
Please specify a line range to read")
        (with-temp-buffer
          (insert-file-contents filename)
          (buffer-string)))
    ;; TODO: Handle nil start-line OR nil end-line
    (cl-decf start-line)
    (let* ((file-size (nth 7 (file-attributes filename)))
           (chunk-size (min file-size (* 512 1024)))
           (byte-offset 0) (line-offset (- end-line start-line)))
      (with-temp-buffer
        ;; Go to start-line
        (while (and (> start-line 0)
                    (< byte-offset file-size))
          (insert-file-contents
           filename nil byte-offset (+ byte-offset chunk-size))
          (setq byte-offset (+ byte-offset chunk-size))
          (setq start-line (forward-line start-line))
          (when (eobp)
            (if (/= (line-beginning-position) (line-end-position))
                ;; forward-line counted 1 extra line
                (cl-incf start-line))
            (delete-region (point-min) (line-beginning-position))))

        (delete-region (point-min) (point))

        ;; Go to end-line, forward by line-offset
        (cl-block nil
          (while (> line-offset 0)
            (setq line-offset (forward-line line-offset))
            (when (and (eobp) (/= (line-beginning-position) (line-end-position)))
              ;; forward-line counted 1 extra line
              (cl-incf line-offset))
            (if (= line-offset 0)
                (delete-region (point) (point-max))
              (if (>= byte-offset file-size)
                  (cl-return)
                (insert-file-contents
                 filename nil byte-offset (+ byte-offset chunk-size))
                (setq byte-offset (+ byte-offset chunk-size))))))

        (buffer-string)))))

(defun gptel-agent--grep (regex path &optional glob context-lines)
  "Search for REGEX in file or directory at PATH using ripgrep.

REGEX is a PCRE-format regular expression to search for.
PATH can be a file or directory to search in.

Optional arguments:
GLOB restricts the search to files matching the glob pattern.
  Examples: \"*.el\", \"*.md\", \"*.rs\"
CONTEXT-LINES specifies the number of lines of context to show
  around each match (0-15 inclusive, defaults to 0).

Returns a string containing matches grouped by file, with line numbers
and optional context.  Results are limited to 1000 or fewer matches per
file.  Results are sorted by modification time."
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))
  (let ((grepper (or (executable-find "rg") (executable-find "grep"))))
    (unless grepper
      (error "Error: ripgrep/grep not available, this tool cannot be used"))
    (with-temp-buffer
      (let* ((cmd (file-name-sans-extension (file-name-nondirectory grepper)))
             (args
              (cond
               ((string= "rg" cmd)
                (delq nil (list "--sort=modified"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--glob=%s" glob))
                                ;; "--files-with-matches"
                                "--max-count=1000"
                                "--heading" "--line-number" "-e" regex
                                (expand-file-name (substitute-in-file-name path)))))
               ((string= "grep" cmd)
                (delq nil (list "--recursive"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--include=%s" glob))
                                "--max-count=1000"
                                "--line-number" "--regexp" regex
                                (expand-file-name (substitute-in-file-name path)))))
               (t (error "Error: failed to identify grepper"))))
             (exit-code (apply #'call-process grepper nil '(t t) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
        (buffer-string)))))

;;; Todo-write tool (task tracking)
(defvar-local gptel-agent--todos nil)

(defun gptel-agent-toggle-todos ()
  "Toggle the display of the gptel agent todo list."
  (interactive)
  (pcase-let ((`(,prop-value . ,ov)
               (or (get-char-property-and-overlay (point) 'gptel-agent--todos)
                   (get-char-property-and-overlay
                    (previous-single-char-property-change
                     (point) 'gptel-agent--todos nil (point-min))
                    'gptel-agent--todos))))
    (if-let* ((fmt (overlay-get ov 'after-string)))
        (progn (overlay-put ov 'gptel-agent--todos fmt)
               (overlay-put ov 'after-string nil))
      (overlay-put ov 'after-string
                   (and (stringp prop-value) prop-value))
      (overlay-put ov 'gptel-agent--todos t))))

(defun gptel-agent--write-todo (todos)
  "Display a formatted task list in the buffer.

TODOS is a list of plists with keys :content, :activeForm, and :status.
Completed items are displayed with strikethrough and shadow face.
Exactly one item should have status \"in_progress\"."
  (setq gptel-agent--todos todos)
  ;; Update overlay
  (let* ((info (gptel-fsm-info gptel--fsm-last))
         (where-from
          (previous-single-property-change
           (plist-get info :position) 'gptel nil (point-min)))
         (where-to (plist-get info :position)))
    (unless (= where-from where-to)
      (pcase-let ((`(,_ . ,todo-ov)
                   (get-char-property-and-overlay where-from 'gptel-agent--todos)))
        (if todo-ov
            ;; Move if reusing an old overlay and the text has changed.
            (move-overlay todo-ov where-from where-to)
          (setq todo-ov (make-overlay where-from where-to nil t))
          (overlay-put todo-ov 'gptel-agent--todos t)
          (overlay-put todo-ov 'evaporate t)
          (overlay-put todo-ov 'priority -40)
          (overlay-put todo-ov 'keymap (define-keymap
                                         "<tab>" #'gptel-agent-toggle-todos
                                         "TAB"   #'gptel-agent-toggle-todos))
          (plist-put
           info :post              ; Don't use push, see note in gptel-anthropic
           (cons (lambda (&rest _)      ; Clean up header line after tasks are done
                   (when (and gptel-mode gptel-use-header-line header-line-format)
                     (setf (nth 2 header-line-format) gptel--header-line-info)))
                 (plist-get info :post))))
        (let* ((formatted-todos         ; Format the todo list
                (mapconcat
                 (lambda (todo)
                   (pcase (plist-get todo :status)
                     ("completed"
                      (concat "✓ " (propertize (plist-get todo :content)
                                               'face '(:inherit shadow :strike-through t))))
                     ("in_progress"
                      (concat "● " (propertize (plist-get todo :activeForm)
                                               'face '(:inherit bold :inherit warning))))
                     (_ (concat "○ " (plist-get todo :content)))))
                 todos "\n"))
               (in-progress
                (cl-loop for todo across todos
                         when (equal (plist-get todo :status) "in_progress")
                         return (plist-get todo :activeForm)))
               (todo-display
                (concat
                 (unless (= (char-before (overlay-end todo-ov)) 10) "\n")
                 gptel-agent--hrule
                 (propertize "Task list: [ "
                             'face '(:inherit font-lock-comment-face :inherit bold))
                 (save-excursion
                   (goto-char (1- (overlay-end todo-ov)))
                   (propertize (substitute-command-keys "\\[gptel-agent-toggle-todos]")
                               'face 'help-key-binding))
                 (propertize " to toggle display ]\n" 'face 'font-lock-comment-face)
                 formatted-todos "\n"
                 gptel-agent--hrule)))
          (overlay-put todo-ov 'after-string todo-display)
          (when (and gptel-mode gptel-use-header-line in-progress header-line-format)
            (setf (nth 2 header-line-format)
                  (concat (propertize
                           " " 'display
                           `(space :align-to (- right ,(+ 5 (length in-progress)))))
                          (propertize (concat "Task: " in-progress)
                                      'face 'font-lock-escape-face))))))))
  t)

;;; Skill tool
(defun gptel-agent--get-skill (skill &optional _args)
  "Return the details of the SKILL.

This loads the body of the corresponding SKILL.  When using this as a
tool in gptel, make sure the known skills are added to the context
window.  `gptel-agent--skills-system-message' can be used to generate
the known skills as string ready to be included to the context."
  (let ((skill-dir
         (car-safe
          (alist-get skill gptel-agent--skills nil nil #'string-equal))))
    (if (not skill-dir)
        (format "Error: skill %s not found." skill)
      (let* ((skill-dir-expanded (expand-file-name skill-dir))
             (skill-files
              (mapcar
               (lambda (full-path)
                 (cons (file-relative-name full-path skill-dir-expanded)
                       full-path))
               (directory-files-recursively skill-dir-expanded ".*")))
             (body (plist-get
                    (cdr (gptel-agent-read-file
                          (expand-file-name "SKILL.md" skill-dir)))
                    :system)))
        (if body
            (let (start)
              (with-temp-buffer
                (insert "## Skill: " skill
                        "\n- base dir: " skill-dir-expanded "\n")
                (setq start (point))
                (insert body)
                (pcase-dolist (`(,rel-path . ,full-path) skill-files)
                  (unless (string-match-p "SKILL\\.md" rel-path)
                    (goto-char start)
                    (while (search-forward-regexp (regexp-quote rel-path) nil t)
                      (replace-match full-path t t))))
                (buffer-string)))
          (format "Could not load body of skill %s" skill))))))

;;; Register tool call preview functions

(pcase-dolist (`(,tool-name . ,setup-fn)
               `(("Write"     ,#'gptel-agent--write-file-preview-setup)
                 ("Eval"     ,#'gptel-agent--eval-elisp-preview-setup)
                 ("Bash"   ,#'gptel-agent--execute-bash-preview-setup)
                 ("Edit"     ,#'gptel-agent--edit-files-preview-setup)
                 ("Insert" ,#'gptel-agent--insert-in-file-preview-setup)
                 ("Agent"     ,#'gptel-agent--task-preview-setup)))
  (setf (alist-get tool-name gptel--tool-preview-alist
                   nil nil #'equal)
        setup-fn))

;;; All tool declarations

(gptel-make-tool
 :name "Bash"
 :function #'gptel-agent--execute-bash
 :description "Execute Bash commands.

This tool provides access to a Bash shell with GNU coreutils (or equivalents) available.
Use this to inspect system state, run builds, tests or other development or system administration tasks.

Do NOT use this for file operations, finding, reading or editing files.
Use the provided file tools instead: `Read`, `Write`, `Edit`, \
`Glob`, `Grep`

- Quote file paths with spaces using double quotes.
- Chain dependent commands with && (or ; if failures are OK)
- Use absolute paths instead of cd when possible
- For parallel commands, make multiple `Bash` calls in one message
- Run tests, check your work or otherwise close the loop to verify changes you make.

EXAMPLES:
- List files with details: 'ls -lah /path/to/dir'
- Find recent errors: 'grep -i error /var/log/app.log | tail -20'
- Check file type: 'file document.pdf'
- Count lines: 'wc -l *.txt'

The command will be executed in the current working directory.  Output is
returned as a string.  Long outputs should be filtered/limited using pipes."
 :args '(( :name "command"
           :type string
           :description "The Bash command to execute.  \
Can include pipes and standard shell operators.
Example: 'ls -la | head -20' or 'grep -i error app.log | tail -50'"))
 :category "gptel-agent"
 :confirm t
 :include t
 :async t)

(gptel-make-tool
 :name "Eval"
 :function
 (lambda (expression)
   (let ((standard-output (generate-new-buffer " *gptel-agent-eval-elisp*"))
         (result nil) (output nil))
     (unwind-protect
         (condition-case err
             (progn
               (setq result (eval (read expression) t))
               (when (> (buffer-size standard-output) 0)
                 (setq output (with-current-buffer standard-output (buffer-string))))
               (concat
                (format "Result:\n%S" result)
                (and output (format "\n\nSTDOUT:\n%s" output))))
           ((error user-error)
            (concat
             (format "Error: eval failed with error %S: %S"
                     (car err) (cdr err))
             (and output (format "\n\nSTDOUT:\n%s" output)))))
       (kill-buffer standard-output))))
 :description "Evaluate Elisp EXPRESSION and return result and any printed output.

EXPRESSION can be anything to evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

Output from `print`, `prin1`, and `princ` is captured and returned as STDOUT.
Use `print` for diagnostic output, not `message` (which goes to *Messages* buffer
and is not captured).

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user."
 :args '(( :name "expression"
           :type string
           :description "A single elisp sexp to evaluate."))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "WebSearch"
 :function 'gptel-agent--web-search-eww
 :description "Search the web for the first five results to a query.  The query can be an arbitrary string.  Returns the top five results from the search engine as a list of plists.  Each object has the keys `:url` and `:excerpt` for the corresponding search result.

This tool uses the Emacs web browser (eww) with its default search engine (typically DuckDuckGo) to perform searches. No API key is required.

If required, consider using the url as the input to the `Read` tool to get the contents of the url.  Note that this might not work as the `Read` tool does not handle javascript-enabled pages."
 :args '((:name "query"
                :type string
                :description "The natural language search query, can be multiple words.")
         (:name "count"
                :type integer
                :description "Number of results to return (default 5)"
                :optional t))
 :include t
 :async t
 :category "gptel-agent")

(gptel-make-tool
 :function #'gptel-agent--read-url
 :name "WebFetch"
 :description "Fetch and read the contents of a URL.

- Returns the text of the URL (not HTML) formatted for reading.
- Request times out after 30 seconds."
 :args '(( :name "url"
           :type "string"
           :description "The URL to read"))
 :async t
 :include t
 :category "gptel-agent")

(gptel-make-tool
 :name "YouTube"
 :function #'gptel-agent--yt-read-url
 :description "Find the description and video transcript for a youtube video.  Returns a markdown formatted string containing two sections:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
                :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
                :type "string"))
 :category "gptel-agent"
 :async t
 :include t)

(gptel-make-tool
 :name "Diagnostics"
 :description "Collect all code diagnostics with severity high/medium \
across all open buffers in the current project.

With optional argument `all`, collect notes and low-severity diagnostics
too."
 :function #'gptel-agent--flymake-diagnostics
 :args (list '( :name "all"
                :type boolean
                :description
                "Whether low-severity diagnostics (notes) should also be collected."
                :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Mkdir"
 :description "Create a new directory with the given name in the specified parent directory"
 :function #'gptel-agent--make-directory
 :args (list '( :name "parent"
                :type "string"
                :description "The parent directory where the new directory should be created, e.g. /tmp")
             '( :name "name"
                :type "string"
                :description "The name of the new directory to create, e.g. testdir"))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Edit"
 :description
 "Replace text in one or more files.

To edit a single file, provide the file `path`.

For the replacement, there are two methods:
- Short replacements: Provide both `old_str` and `new_str`, in which case `old_str` \
needs to exactly match one unique section of the original file, including any whitespace.  \
Make sure to include enough context that the match is not ambiguous.  \
The entire original string will be replaced with `new str`.
- Long or involved replacements: set the `diff` parameter to true and provide a unified diff \
in `new_str`. `old_str` can be ignored.

To edit multiple files,
- provide the directory path,
- set the `diff` parameter to true
- and provide a unified diff in `new_str`.

Diff instructions:

- The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format.
- The LLM should generate the diff such that the file paths within the diff \
  (e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'path'.

To simply insert text at some line, use the \"Insert\" instead."
 :function #'gptel-agent--edit-files
 :args '(( :name "path"
           :description "File path or directory to edit"
           :type string)
         ( :name "old_str"
           :description "Original string to replace.  If providing a unified diff, this should be false"
           :type string
           :optional t)
         ( :name "new_str"
           :description "Replacement string OR unified diff text"
           :type string)
         ( :name "diff"
           :description "Whether the replacement is a string or a diff.  `true` for a diff, `false` otherwise."
           :type boolean))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Insert"
 :description "Insert `new_str` after `line_number` in file at `path`.

Use this tool for purely additive actions: adding text to a file at a \
specific location with no changes to the surrounding context."
 :function #'gptel-agent--insert-in-file
 :args '(( :name "path"
           :description "Path of file to edit."
           :type string)
         ( :name "line_number"
           :description "The line number at which to insert `new_str`, with
- 0 to insert at the beginning, and
- -1 to insert at the end."
           :type integer)
         ( :name "new_str"
           :description "String to insert at `line_number`."
           :type string))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Write"
 :description "Create a new file with the specified content.
Overwrites an existing file, so use with care!
Consider using the more granular tools \"Insert\" or \"Edit\" first."
 :function #'gptel-agent--write-file
 :args (list '( :name "path"
                :type "string"
                :description "The directory where to create the file, \".\" is the current directory.")
             '( :name "filename"
                :type "string"
                :description "The name of the file to create.")
             '( :name "content"
                :type "string"
                :description "The content to write to the file"))
 :category "gptel-agent"
 :confirm t)

(gptel-make-tool
 :name "Glob"
 :description "Recursively find files matching a provided glob pattern.

- Supports glob patterns like \"*.md\" or \"*test*.py\".
  The glob applies to the basename of the file (with extension).
- Does not support double wildcard \"**/*\".
- Returns matching file paths at all depths sorted by modification time.
  Limit the depth of the search by providing the `depth` argument.
- When you are doing an open ended search that may require multiple rounds
  of globbing and grepping, use the \"task\" tool instead
- You can call multiple tools in a single response.  It is always better to
  speculatively perform multiple searches in parallel if they are potentially useful."
 :function #'gptel-agent--glob
 :args '(( :name "pattern"
           :type string
           :description "Glob pattern to match, for example \"*.el\". Must not be empty.
Use \"*\" to list all files in a directory.")
         ( :name "path"
           :type string
           :description "Directory to search in.  Supports relative paths and defaults to \".\""
           :optional t)
         ( :name "depth"
           :description "Limit directory depth of search, 1 or higher. Defaults to no limit."
           :type integer
           :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Read"
 :description "Read file contents between specified line numbers `start_line` and `end_line`,
with both ends included.

Consider using the \"Grep\" tool to find the right range to read first.

Reads the whole file if the line range is not provided.

Files over 512 KB in size can only be read by specifying a line range."
 :function #'gptel-agent--read-file-lines
 :args '(( :name "file_path"
           :type string
           :description "The path to the file to be read.")
         ( :name "start_line"
           :type integer
           :description "The line to start reading from, defaults to the start of the file"
           :optional t)
         ( :name "end_line"
           :type integer
           :description "The line up to which to read, defaults to the end of the file."
           :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Grep"
 :description "Search for text in file(s) at `path`.

Use this tool to find relevant parts of files to read.

Returns a list of matches prefixed by the line number, and grouped by file.  Can search an individual file (if providing a file path) or a directory.  Consider using this tool to find the right line range for the \"Read\" tool.

When searching directories, optionally restrict the types of files in the search with a `glob`.  Can request context lines around each match using the `context_lines` parameters."
 :function #'gptel-agent--grep
 :args '(( :name "regex"
           :description "Regular expression to search for in file contents."
           :type string)
         ( :name "path"
           :description "File or directory to search in."
           :type string)
         ( :name "glob"
           :description "Optional glob to restrict file types to search for.
Only required when path is a directory.
Examples: *.md, *.rs"
           :type string
           :optional t)
         ( :name "context_lines"
           :description "Number of lines of context to retrieve around each match (0-15 inclusive).
Optional, defaults to 0."
           :optional t
           :type integer
           :maximum 15))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "TodoWrite"
 :description "Create and manage a structured task list for your current session.  \
Helps track progress and organize complex tasks. Use proactively for multi-step work.

Only one todo can be `in_progress` at a time."
 :function #'gptel-agent--write-todo
 :args
 '(( :name "todos"
     :type array
     :items
     ( :type object
       :properties
       (:content
        ( :type string :minLength 1
          :description "Imperative form describing what needs to be done (e.g., 'Run tests')")
        :status
        ( :type string
          :enum ["pending" "in_progress" "completed"]
          :description "Task status: pending, in_progress (exactly one), or completed")
        :activeForm
        ( :type string :minLength 1
          :description "Present continuous form shown during execution (e.g., 'Running tests')")))))
 :category "gptel-agent")

(gptel-make-tool
 :name "Skill"
 :description "Load a skill into the current conversation.

Each skill provides guidance on how to execute a specific task.
You can invoke a skill with optional args, the args are for your future reference only.

When to use:
- When a skill is relevant, you must invoke this tool IMMEDIATELY
- This is a BLOCKING REQUIREMENT: invoke the relevant Skill tool before generating any other response about the task
- Only use skills listed in your prompt
- Do not invoke a skill that is already loaded.

How to use:
- Invoke with the skill name and optional args.  The args are for your reference only
- Examples:
    - `skill: \"pdf\"` - invoke the pdf skill
    - `skill: \"commit\", args: \"-m 'Fix bug'\"` - invoke with arguments
    - `skill: \"review-pr\", args: \"123\"` - invoke with arguments"
 :function #'gptel-agent--get-skill
 :args '(( :name "skill"
           :type string
           :description "Name of the skill, chosen from the list of available skills")
         ( :name "args"
           :type string
           :optional t
           :description "Args relevant to the skill, for your future reference"))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Agent"
 :description "Launch a specialized agent to handle complex, multi-step tasks autonomously.  \
Agents run independently in their own buffer and MUST call AgentFinish when done.  \
Use for open-ended searches, complex research, or when uncertain about finding results in first few tries."
 :function #'gptel-agent--task
 :args '(( :name "subagent_type"
           :type string
           :enum ["researcher" "introspector"]
           :description "The type of specialized agent to use for this task")
         ( :name "description"
           :type string
           :description "A short (3-5 word) description of the task")
         ( :name "prompt"
           :type "string"
           :description "The detailed task for the agent to perform autonomously.  \
Should include exactly what information the agent should return."))
 :category "gptel-agent"
 :async t
 :confirm t
 :include t)

;;; Sub-agent control tools (AgentFinish, AskUser)

(gptel-make-tool
 :name "AgentFinish"
 :description "Signal that you have completed your task and return results to the delegating agent.

CRITICAL: You MUST call this tool when your task is complete.
Without calling AgentFinish, your results will NOT be delivered to the delegating agent.

Use status 'success' when the task was completed successfully.
Use status 'error' when the task could not be completed due to an error.
Use status 'partial' when you gathered some results but couldn't fully complete the task."
 :function #'gptel-agent--finish
 :args '((:name "status"
          :type string
          :enum ["success" "error" "partial"]
          :description "The completion status of the task")
         (:name "result"
          :type string
          :description "The result or findings to return to the delegating agent. \
For errors, include what went wrong and any partial results.")
         (:name "summary"
          :type string
          :optional t
          :description "Optional brief one-line summary of the result"))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "AskUser"
 :description "Request input or clarification from the user.

Use this when you need user input to proceed with your task.
The user will be prompted in the main conversation buffer and your execution will pause until they respond.

Examples of when to use:
- Need clarification on ambiguous requirements
- Need to choose between multiple valid approaches  
- Need confirmation before a potentially risky action
- Need additional information not available in context

Note: Only use this when truly necessary. Try to make reasonable assumptions first."
 :function #'gptel-agent--ask-user
 :args '((:name "question"
          :type string
          :description "The question or prompt to show the user")
         (:name "options"
          :type array
          :items (:type string)
          :optional t
          :description "Optional list of suggested options/choices for the user")
         (:name "context"
          :type string
          :optional t
          :description "Additional context to help the user understand the question")
         (:name "default"
          :type string
          :optional t
          :description "Suggested default answer if user just presses enter"))
 :category "gptel-agent"
 :include t
 :async t
 :confirm nil)

(provide 'gptel-agent-tools)
;;; gptel-agent-tools.el ends here

;; Local Variables:
;; elisp-flymake-byte-compile-load-path: ("~/.local/share/git/elpaca/repos/gptel/" "~/.local/share/git/elpaca/repos/transient/lisp" "~/.local/share/git/elpaca/repos/compat/")
;; End:
