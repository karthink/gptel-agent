;;; gptel-agent.el --- Agentic LLM use for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025 Karthik Chikmagalur

;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (compat "30.1.0.0") (gptel "0.9.9") (yaml "1.2.0") (orderless "1.1"))
;; Keywords: comm
;; URL: https://github.com/karthink/gptel-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is a collection of tools and prompts to use gptel "agentically" with any
;; LLM, to autonomously perform tasks.
;;
;; It has access to
;; - the web (via basic web search, URL fetching and YouTube video metadata),
;; - local files (read/write/edit),
;; - the state of Emacs (documentation and Elisp evaluation),
;; - and Bash, if you are in a POSIX-y environment.
;;
;; To use gptel-agent in a dedicated buffer:
;;
;; - Set `gptel-model' and `gptel-backend' to use your preferred LLM.
;;
;; - Run M-x `gptel-agent'.  This will open a buffer with the agent preset
;;   loaded.
;;
;; - Use gptel as usual, calling `gptel-send' etc.
;;
;; - If you change the system prompt, tools or other settings in this buffer, you
;;   can reset the agent state by (re)applying the "gptel-agent" preset from
;;   gptel's menu.
;;
;; To use gptel-agent anywhere in Emacs:
;;
;; - As with gptel, you can use gptel-agent in any buffer.  Just apply the
;;   "gptel-agent" preset in the buffer, or include "@gptel-agent" in a prompt.
;;
;; gptel-agent can delegate tasks to "sub-agents".  Sub-agents can be specified
;; in Markdown or Org files in a directory.  To see how to specify agents,
;; examine the "agents" directory in this package.  You can add your directory
;; of agents to `gptel-agent-dirs', which see.
;;
;; Please note: gptel-agent uses significantly more tokens than the average
;; gptel LLM interaction.
;;
;;; Code:

(require 'compat)
(require 'gptel)
(require 'gptel-agent-tools)
(eval-when-compile (require 'cl-lib))

(declare-function yaml-parse-string "yaml")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-root "project")
(declare-function org-get-property-block "org")
(declare-function org-entry-properties "org")
(declare-function gptel-agent--confirm-overlay "gptel-agent-tools")
(declare-function gptel-agent--handle-tool-use-with-confirmation "gptel-agent-tools")
(declare-function gptel-curl--stream-insert-response "gptel-curl")
(defvar org-inhibit-startup)
(defvar project-prompter)

;;; User options
(defcustom gptel-agent-dirs
  (list (expand-file-name
         "./agents/" (file-name-directory
                      (or load-file-name (buffer-file-name)))))
  "Agent definition directories for `gptel-agent'.

Markdown (.md) and Org (.org) files in these directories will be scanned
for gptel sub-agent definitions by `gptel-agent'."
  :type '(repeat directory)
  :group 'gptel-agent)

(defcustom gptel-agent-skill-dirs '("~/.claude/skills/"
                                    ".claude/skills/"
                                    "~/.agents/skills" ;; codex
                                    ".agents/skills"   ;; codex
                                    "~/.opencode/skill/"
                                    ".opencode/skill/"
                                    "~/.gemini/skills/"
                                    ".gemini/skills/")
  "Agent skill definition directories.

Each directory listed here should contain agent skills.  An agent skill
is a directory with at least one file named \"SKILL.md\".

Relative paths are resolved against the current directory and also
against the project root when searching for skills.

Relative directory locations will be take precedence over absolute
locations. If multiple skills share the same name, the one in the
directory listed earlier takes precedence.

See https://agentskills.io for more details on agentskills."
  :type '(repeat directory)
  :group 'gptel-agent)

;;; State update
(defvar gptel-agent--agents nil
  "Known gptel agents.

Alist mapping agent names to a plist of agent properties.")

(defvar gptel-agent--skills nil
  "Known skills alist.

The key is the name.  The value is a cons (LOCATION . SKILL-PLIST).
LOCATION is path to the skill's directory.  SKILL-PLIST is the header
of the corresponding SKILL.md as a plist.")

;;; Sub-agent state tracking variables

(defvar gptel-agent-kill-finished-buffers nil
  "If non-nil, kill sub-agent buffers when they finish.
When nil (the default), sub-agent buffers are kept for debugging and inspection.")

;; Parent buffer variables (track active sub-agents)
(defvar-local gptel-agent--active-subagents nil
  "List of active sub-agent buffers spawned from this buffer.
Each entry is a plist with :buffer, :type, :description, :status.")

(defvar-local gptel-agent--user-request-queue nil
  "Queue of pending user requests from sub-agents.
Each entry is a plist with :agent-buffer, :request-id, :callback, etc.")

;; Sub-agent buffer variables (store parent context)
(defvar-local gptel-agent--parent-buffer nil
  "Reference to the parent buffer that spawned this sub-agent.")

(defvar-local gptel-agent--parent-callback nil
  "Callback to invoke when this sub-agent finishes (via AgentFinish).")

(defvar-local gptel-agent--agent-type nil
  "The type of this sub-agent (e.g., \"researcher\", \"executor\").")

(defvar-local gptel-agent--task-description nil
  "Short description of the task this sub-agent is performing.")

(defvar-local gptel-agent--pending-user-requests nil
  "List of pending AskUser requests in this sub-agent buffer.
Each entry is a plist: (:id ID :callback CB :question Q :timestamp TS).")

(defvar-local gptel-agent--auto-confirm-tools nil
  "When non-nil, automatically approve all tool calls in this sub-agent.
Set to t when user selects \"Yes to all\" during tool confirmation.")

(defvar gptel-agent--request-id-counter 0
  "Counter for generating unique request IDs.")

(defvar-local gptel-agent--parent-overlay nil
  "Overlay in parent buffer showing this sub-agent's task status.
Used to update parent buffer with tool call progress.")

(defun gptel-agent--generate-request-id ()
  "Generate a unique request ID for AskUser requests."
  (format "req-%d-%s"
          (cl-incf gptel-agent--request-id-counter)
          (format-time-string "%H%M%S")))

;;; Sub-agent buffer management

(defun gptel-agent--setup-header-line (&optional agent-preset)
  "Set up the agent header line with preset display.
AGENT-PRESET is the initial agent type symbol to display (e.g., 'researcher, 'spec-workflow).
The header line dynamically reads from `gptel--preset' to reflect changes made via gptel-menu."
  (when gptel-use-header-line
    ;; Set gptel--preset if an initial preset is provided
    (when agent-preset
      (setq-local gptel--preset agent-preset))
    (setcar header-line-format
            `(:eval (concat
                     (propertize " " 'display '(space :align-to 0))
                     (format "%s" (gptel-backend-name gptel-backend))
                     (propertize 
                      (buttonize
                       (format "[%s]" 
                               (capitalize 
                                (replace-regexp-in-string 
                                 "-" " " 
                                 (symbol-name (or gptel--preset 'gptel-agent)))))
                       (lambda (_button) 
                         (require 'gptel-transient)
                         (call-interactively #'gptel--preset))
                       nil
                       "Choose gptel preset")
                      'face 'font-lock-keyword-face))))))

(defun gptel-agent--update-subagent-status (status &optional face)
  "Update the status indicator in a sub-agent buffer's header-line.
STATUS is the status text to display (e.g., \" Ready\", \" Working...\").
FACE is the optional face to use (defaults based on STATUS)."
  (when gptel-use-header-line
    (let ((status-text (if (stringp status) status (format " %s" status)))
          (status-face (or face
                          (pcase status
                            ((or " Ready" 'ready) 'success)
                            ((or " Working..." 'working) 'mode-line-emphasis)
                            ((or " Waiting..." 'waiting) 'warning)
                            ((or " Error" 'error) 'error)
                            (_ 'default)))))
      (when (consp header-line-format)
        (setf (nth 1 header-line-format)
              (propertize status-text 'face status-face))
        (force-mode-line-update)))))

(defun gptel-agent--create-subagent-buffer (agent-type description parent-buffer agent-preset)
  "Create a dedicated buffer for a sub-agent session.
AGENT-TYPE is the type of agent (e.g., \"researcher\").
DESCRIPTION is a short task description.
PARENT-BUFFER is the buffer that spawned this sub-agent.
AGENT-PRESET is the preset plist to apply to the buffer.
Returns the new buffer."
  (let* ((buf-name (format "*gptel-agent:%s:%s*"
                           agent-type
                           (truncate-string-to-width description 30 nil nil "…")))
         (buf (generate-new-buffer buf-name)))
    (with-current-buffer buf
      ;; Inherit major mode from parent buffer (major mode change kills buffer-local variables)
      (funcall (buffer-local-value 'major-mode parent-buffer))
      ;; Store parent context AFTER major mode is set
      (setq-local gptel-agent--parent-buffer parent-buffer)
      (setq-local gptel-agent--parent-callback nil) ; set later by gptel-agent--task
      (setq-local gptel-agent--agent-type agent-type)
      (setq-local gptel-agent--task-description description)
      (setq-local gptel-agent--pending-user-requests nil)
      (setq-local gptel-agent--auto-confirm-tools nil)
      ;; Copy relevant settings from parent
      (setq-local default-directory (buffer-local-value 'default-directory parent-buffer))
      (setq-local gptel-backend (buffer-local-value 'gptel-backend parent-buffer))
      ;; Only copy model/temperature if agent preset didn't specify them
      (unless (plist-member agent-preset :model)
        (setq-local gptel-model (buffer-local-value 'gptel-model parent-buffer)))
      (unless (plist-member agent-preset :temperature)
        (setq-local gptel-temperature (buffer-local-value 'gptel-temperature parent-buffer)))
      (setq-local gptel-max-tokens (buffer-local-value 'gptel-max-tokens parent-buffer))
      (setq-local gptel-stream (buffer-local-value 'gptel-stream parent-buffer))
      (setq-local gptel-use-curl (buffer-local-value 'gptel-use-curl parent-buffer))
      ;; Enable gptel-mode for proper conversation tracking
      (gptel-mode 1)
      ;; Apply the agent preset AFTER enabling gptel-mode
      (when agent-preset
        (gptel--apply-preset
         (append
          ;; Agent preset takes priority (may include :include-reasoning)
          agent-preset
          ;; Defaults for keys not in preset
          (list :use-tools t
                :use-context nil)
          ;; Inherit reasoning from parent if not specified in preset
          (unless (plist-member agent-preset :include-reasoning)
            (list :include-reasoning
                  (buffer-local-value 'gptel-include-reasoning parent-buffer))))
         (lambda (sym val) (set (make-local-variable sym) val))))
      ;; Set up header line to show agent type
      (gptel-agent--setup-header-line (intern agent-type)))
    buf))

(defun gptel-agent--register-subagent (parent-buffer agent-buffer type description)
  "Register a new sub-agent AGENT-BUFFER in PARENT-BUFFER's tracking list.
TYPE is the agent type, DESCRIPTION is the task description."
  (with-current-buffer parent-buffer
    (push (list :buffer agent-buffer
                :type type
                :description description
                :status 'running
                :start-time (current-time))
          gptel-agent--active-subagents)))

(defun gptel-agent--unregister-subagent (parent-buffer agent-buffer)
  "Remove AGENT-BUFFER from PARENT-BUFFER's tracking list."
  (when (buffer-live-p parent-buffer)
    (with-current-buffer parent-buffer
      (setq gptel-agent--active-subagents
            (cl-remove agent-buffer gptel-agent--active-subagents
                       :key (lambda (s) (plist-get s :buffer)))))))

(defun gptel-agent--cleanup-subagent (agent-buffer &optional status)
  "Clean up AGENT-BUFFER after it finishes.
STATUS is the completion status (for updating parent's tracking)."
  (when (buffer-live-p agent-buffer)
    (with-current-buffer agent-buffer
      ;; Update status indicator
      (pcase status
        ('success (gptel-agent--update-subagent-status " Ready" 'success))
        ('error   (gptel-agent--update-subagent-status " Error" 'error))
        ('aborted (gptel-agent--update-subagent-status " Aborted" 'error))
        (_        (gptel-agent--update-subagent-status " Done" 'success)))
      
      (when-let* ((parent-buffer gptel-agent--parent-buffer))
        ;; Update status in parent's tracking
        (when (buffer-live-p parent-buffer)
          (with-current-buffer parent-buffer
            (when-let* ((entry (cl-find agent-buffer gptel-agent--active-subagents
                                        :key (lambda (s) (plist-get s :buffer)))))
              (plist-put entry :status (or status 'finished)))))
        ;; Unregister from parent
        (gptel-agent--unregister-subagent parent-buffer agent-buffer))
      ;; Optionally kill the buffer
      (when gptel-agent-kill-finished-buffers
        (kill-buffer agent-buffer)))))

;;; Sub-agent FSM handlers

(defun gptel-agent--handle-subagent-done (fsm)
  "Handle successful completion of sub-agent request FSM.

Sets `gptel--fsm-last' for inspection, runs :post cleanup, and
auto-notifies the parent if AgentFinish was never called."
  (let ((info (gptel-fsm-info fsm)))
    ;; Set fsm-last in the sub-agent buffer for inspection/diagnostics
    (when-let* ((buf (plist-get info :buffer))
                ((buffer-live-p buf)))
      (with-current-buffer buf
        (setq gptel--fsm-last fsm)))
    ;; Run :post cleanup callbacks
    (gptel--handle-post fsm)
    ;; If AgentFinish was never called, notify the parent so it doesn't hang
    (when-let* ((buf (plist-get info :buffer))
                ((buffer-live-p buf)))
      (with-current-buffer buf
        (when gptel-agent--parent-callback
          ;; AgentFinish was NOT called — extract buffer text as fallback result
          (let* ((result (buffer-substring-no-properties (point-min) (point-max)))
                 (truncated (truncate-string-to-width result 4000 nil nil t))
                 (parent-cb gptel-agent--parent-callback)
                 (parent-buf gptel-agent--parent-buffer)
                 (agent-type gptel-agent--agent-type)
                 (description gptel-agent--task-description))
            (setq gptel-agent--parent-callback nil)
            (when (buffer-live-p parent-buf)
              (with-current-buffer parent-buf
                ;; Delete the task overlay in parent buffer
                (dolist (ov (overlays-in (point-min) (point-max)))
                  (when (and (overlay-get ov 'gptel-agent)
                             (equal (overlay-get ov 'gptel-agent-buffer) buf))
                    (delete-overlay ov)))
                ;; Notify parent with partial status (not error - agent did produce output)
                (funcall parent-cb
                         (format "%s result for task: %s\n\nStatus: partial\nSummary: Agent completed without calling AgentFinish\n\n%s"
                                 (capitalize (or agent-type "Agent"))
                                 (or description "unknown task")
                                 truncated))))
            (gptel-agent--cleanup-subagent buf 'finished)))))))

(defun gptel-agent--handle-subagent-error (fsm)
  "Handle error in sub-agent request FSM.

Captures error from INFO, notifies the parent, and cleans up."
  (let ((info (gptel-fsm-info fsm)))
    ;; Set fsm-last for inspection
    (when-let* ((buf (plist-get info :buffer))
                ((buffer-live-p buf)))
      (with-current-buffer buf
        (setq gptel--fsm-last fsm)))
    ;; Run :post cleanup
    (gptel--handle-post fsm)
    ;; Notify parent of the error
    (when-let* ((buf (plist-get info :buffer))
                ((buffer-live-p buf)))
      (with-current-buffer buf
        (when gptel-agent--parent-callback
          (let* ((error-data (plist-get info :error))
                 (http-msg (plist-get info :status))
                 (parent-cb gptel-agent--parent-callback)
                 (parent-buf gptel-agent--parent-buffer)
                 (agent-type gptel-agent--agent-type)
                 (description gptel-agent--task-description))
            (setq gptel-agent--parent-callback nil)
            (when (buffer-live-p parent-buf)
              (with-current-buffer parent-buf
                ;; Delete the task overlay in parent buffer
                (dolist (ov (overlays-in (point-min) (point-max)))
                  (when (and (overlay-get ov 'gptel-agent)
                             (equal (overlay-get ov 'gptel-agent-buffer) buf))
                    (delete-overlay ov)))
                ;; Notify parent of error
                (funcall parent-cb
                         (format "%s result for task: %s\n\nStatus: error\nHTTP: %s\nError: %S"
                                 (capitalize (or agent-type "Agent"))
                                 (or description "unknown task")
                                 (or http-msg "unknown")
                                 error-data))))
            (gptel-agent--cleanup-subagent buf 'error)))))))

(defun gptel-agent--handle-subagent-abort (fsm)
  "Handle abort of sub-agent request FSM.

Notifies the parent that the sub-agent was aborted."
  (let ((info (gptel-fsm-info fsm)))
    ;; Set fsm-last for inspection
    (when-let* ((buf (plist-get info :buffer))
                ((buffer-live-p buf)))
      (with-current-buffer buf
        (setq gptel--fsm-last fsm)))
    ;; Notify parent of the abort
    (when-let* ((buf (plist-get info :buffer))
                ((buffer-live-p buf)))
      (with-current-buffer buf
        (when gptel-agent--parent-callback
          (let ((parent-cb gptel-agent--parent-callback)
                (parent-buf gptel-agent--parent-buffer)
                (description gptel-agent--task-description))
            (setq gptel-agent--parent-callback nil)
            (when (buffer-live-p parent-buf)
              (with-current-buffer parent-buf
                ;; Delete the task overlay in parent buffer
                (dolist (ov (overlays-in (point-min) (point-max)))
                  (when (and (overlay-get ov 'gptel-agent)
                             (equal (overlay-get ov 'gptel-agent-buffer) buf))
                    (delete-overlay ov)))
                ;; Notify parent of abort
                (funcall parent-cb
                         (format "Error: Task \"%s\" was aborted."
                                 (or description "unknown task")))))
            (gptel-agent--cleanup-subagent buf 'aborted)))))))

;;; Sub-agent task orchestration

(defconst gptel-agent--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))

(defvar gptel-agent-request--handlers
  `((WAIT ,#'gptel-agent--indicate-wait
          ,#'gptel--handle-wait)
    (TOOL ,#'gptel-agent--indicate-tool-call
          ,#'gptel-agent--handle-tool-use-with-confirmation)
    (DONE ,#'gptel-agent--handle-subagent-done)
    (ERRS ,#'gptel-agent--handle-subagent-error)
    (ABRT ,#'gptel-agent--handle-subagent-abort))
  "See `gptel-request--handlers'.
Uses custom tool handling that routes confirmations through AskUser.")

(defun gptel-agent--task-preview-setup (arg-values _info)
  "Preview setup for Agent.
INFO is the tool call info plist.
ARG-VALUES is a list: (type description prompt)"
  (pcase-let ((from (point))
              (`(,type ,desc ,prompt) arg-values))
    (insert "("
            (propertize "Agent " 'font-lock-face 'font-lock-keyword-face)
            (propertize (prin1-to-string type)
                        'font-lock-face 'font-lock-escape-face)
            " " (propertize (prin1-to-string desc)
                            'font-lock-face
                            '(:inherit font-lock-constant-face :inherit bold))
            "\n" (propertize (prin1-to-string prompt)
                             'line-prefix "  "
                             'wrap-prefix "  "
                             'font-lock-face 'font-lock-constant-face)
            ")\n\n")
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--indicate-wait (fsm)
  "Display waiting indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (info-ov (plist-get info :context))
              (count (overlay-get info-ov 'count)))
    (run-at-time
     1.5 nil
     (lambda (ov count)
       (when (and (overlay-buffer ov)
                  (eql (overlay-get ov 'count) count))
         (let* ((task-msg (overlay-get ov 'msg))
                (new-info-msg
                 (concat task-msg
                         (concat
                          (propertize "Waiting... " 'face 'warning) "\n"
                          (propertize "\n" 'face
                                      '(:inherit shadow :underline t :extend t))))))
           (overlay-put ov 'after-string new-info-msg))))
     info-ov count)))

(defun gptel-agent--indicate-tool-call (fsm)
  "Display tool call indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (tool-use (plist-get info :tool-use)))
    ;; Update parent buffer overlay if this is a sub-agent
    (when (and gptel-agent--parent-buffer
               gptel-agent--parent-overlay
               (overlay-buffer gptel-agent--parent-overlay))
      (let* ((task-msg (overlay-get gptel-agent--parent-overlay 'msg))
             (info-count (overlay-get gptel-agent--parent-overlay 'count))
             (new-info-msg))
        (setq new-info-msg
              (concat task-msg
                      (concat
                       (propertize "Calling Tools... " 'face 'mode-line-emphasis)
                       (if (= info-count 0) "\n" (format "(+%d)\n" info-count))
                       (mapconcat (lambda (call)
                                    (gptel--format-tool-call
                                     (plist-get call :name)
                                     (map-values (plist-get call :args))))
                                  tool-use)
                       "\n" gptel-agent--hrule)))
        (overlay-put gptel-agent--parent-overlay 'count 
                     (+ info-count (length tool-use)))
        (overlay-put gptel-agent--parent-overlay 'after-string new-info-msg)))))

(defun gptel-agent--task-overlay (where &optional agent-type description)
  "Create overlay for agent task at WHERE with AGENT-TYPE and DESCRIPTION."
  (let* ((bounds                  ;where to place the overlay, handle edge cases
          (save-excursion
            (goto-char where)
            (when (bobp) (insert "\n"))
            (if (and (bolp) (eolp))
                (cons (1- (point)) (point))
              (cons (line-beginning-position) (line-end-position)))))
         (ov (make-overlay (car bounds) (cdr bounds) nil t))
         (msg (concat
               (unless (eq (char-after (car bounds)) 10) "\n")
               "\n" gptel-agent--hrule
               (propertize (concat (capitalize agent-type) " Task: ")
                           'face 'font-lock-escape-face)
               (propertize description 'face 'font-lock-doc-face) "\n")))
    (prog1 ov
      (overlay-put ov 'gptel-agent t)
      (overlay-put ov 'count 0)
      (overlay-put ov 'msg msg)
      (overlay-put ov 'line-prefix "")
      (overlay-put
       ov 'after-string
       (concat msg (propertize "Waiting..." 'face 'warning) "\n"
               gptel-agent--hrule)))))

(defun gptel-agent--task (main-cb agent-type description prompt)
  "Call a gptel agent in a dedicated buffer.

MAIN-CB is the callback to return results to the parent agent.
AGENT-TYPE is the name of the agent preset (e.g., \"researcher\").
DESCRIPTION is a short description of the task.
PROMPT is the detailed prompt instructing the agent on what to do.

The sub-agent runs in its own buffer with its own conversation context.
It MUST call AgentFinish when done to deliver results back to the parent.
It MAY call AskUser to request user input during execution."
  (let* ((parent-buffer (current-buffer))
         (parent-info (gptel-fsm-info gptel--fsm-last))
         (where (or (plist-get parent-info :tracking-marker)
                    (plist-get parent-info :position)))
         (agent-preset (cdr (assoc agent-type gptel-agent--agents)))
         (agent-buffer (gptel-agent--create-subagent-buffer
                        agent-type description parent-buffer agent-preset)))

    ;; Register the sub-agent in parent's tracking
    (gptel-agent--register-subagent parent-buffer agent-buffer agent-type description)

    ;; Create status overlay in parent buffer
    (let ((ov (gptel-agent--task-overlay where agent-type description)))
      ;; Store reference to agent buffer in overlay for cleanup
      (overlay-put ov 'gptel-agent-buffer agent-buffer)
      ;; Store parent overlay in sub-agent buffer for tool call updates
      (with-current-buffer agent-buffer
        (setq-local gptel-agent--parent-overlay ov)))

    (gptel--update-status " Calling Agent..." 'font-lock-escape-face)

    ;; Set up and run the request in the sub-agent buffer
    (with-current-buffer agent-buffer
      ;; Update status to Working
      (gptel-agent--update-subagent-status " Working..." 'mode-line-emphasis)
      
      ;; Store the parent callback - AgentFinish will use this
      (setq gptel-agent--parent-callback main-cb)

      ;; Insert the initial prompt
      (insert prompt "\n")

      (gptel-request nil  ; Use buffer contents as prompt
        :stream t
        :fsm (gptel-make-fsm :handlers gptel-agent-request--handlers)
        :callback #'gptel-agent--subagent-callback))))

(defun gptel-agent--subagent-callback (response info)
  "Callback for sub-agent responses.
RESPONSE is the LLM response, INFO is the request info plist."
  (pcase response
    ('nil
     ;; Error case — insert into sub-agent buffer for visibility.
     ;; The ERRS FSM handler will notify the parent.
     (let ((error-msg (plist-get info :error)))
       (message "Sub-agent error: %S" error-msg)
       (when-let* ((buf (plist-get info :buffer))
                   ((buffer-live-p buf)))
         (with-current-buffer buf
           (goto-char (point-max))
           (insert (propertize (format "\n[Error: %S]\n" error-msg)
                               'face 'error))))))

    (`(tool-call . ,calls)
     ;; Tool calls in sub-agent buffers are handled by the FSM
     ;; The FSM will display them and auto-confirm via gptel-agent--auto-confirm-tools
     nil)

    ((pred stringp)
     ;; Insert response in sub-agent buffer
     (if (plist-get info :stream)
         (gptel-curl--stream-insert-response response info)
       (gptel--insert-response response info)))

    ('t
     ;; Stream finished successfully.
     ;; The DONE FSM handler will check if AgentFinish was called
     ;; and notify the parent if not.
     nil)

    ('abort
     ;; User aborted — the ABRT FSM handler will notify the parent.
     nil)))

;;;###autoload
(defun gptel-agent-read-file (agent-file &optional templates metadata-only)
  "Read a preset/agent from AGENT-FILE.

If TEMPLATES is non-nil, read the system-prompt with templates applied
to them.  TEMPLATES should be an alist of (VAR-NAME . VAR-VALUE) for
template expansion.  Template variables in the format {{VAR-NAME}} in
the markdown body will be replaced with VAR-VALUE.

If METADATA-ONLY is non-nil, only the header/metadata of the
preset/agent will be returned.  If TEMPLATES and METADATA-ONLY are
both non-nil, TEMPLATES will be ignored."
  (if (not (and (file-readable-p agent-file)
                (file-regular-p agent-file)))
      (prog1 nil
        (message "gptel-agent: File %s is not parseable" agent-file))
    (let* ((agent-plist
            (pcase (file-name-extension agent-file)
              ("org" (gptel-agent-parse-org-properties
                      agent-file nil templates metadata-only))
              ("md" (gptel-agent-parse-markdown-frontmatter
                     agent-file nil templates metadata-only))))
           (name (or (plist-get agent-plist :name)
                     (let ((filename (file-name-base agent-file)))
                       (replace-regexp-in-string " " "-" filename)))))
      (cl-remf agent-plist :name)
      (cons name agent-plist))))

(defun gptel-agent--update-agents ()
  "Update agent definitions from `gptel-agent-dirs'.
Returns an alist of (agent-name . file-path)."
  (setq gptel-agent--agents nil)
  (let ((agent-files nil))               ; Alist of (agent-name . file-path)
    (mapc (lambda (dir)
            (dolist (agent-file (cl-delete-if-not #'file-regular-p
                                                  (directory-files dir 'full)))
              (pcase-let ((`(,name . ,agent-plist) ;loading only metadata
                           (gptel-agent-read-file agent-file nil t)))
                (setf (alist-get name gptel-agent--agents nil t #'equal)
                      agent-plist)
                (push (cons name agent-file) agent-files))))
          gptel-agent-dirs)
    agent-files))

(defun gptel-agent--update-skills ()
  "Update the known skills list from `gptel-agent-skill-dirs'."
  (setq gptel-agent--skills nil)
  (mapc (lambda (dir)
          (when (file-directory-p dir)
            (dolist (skill-file (directory-files-recursively dir "SKILL\\.md"))
              (pcase-let ((`(,name . ,skill-plist) ;loading only metadata
                           (gptel-agent-read-file skill-file nil t)))
                ;; validating skill definition
                (if (plist-get skill-plist :description)
                    (setf (alist-get name gptel-agent--skills nil nil #'string-equal)
                          (cons (file-name-directory skill-file) skill-plist))
                  (warn "Skill %s (at %s) does not have a description. Ignoring %s skill."
                        name skill-file name))))))
        ;; To preserve precedence, the list should be reversed and resolved
        ;; relative names should be at the end.
        (cl-loop for dir in gptel-agent-skill-dirs
                 with project-root = (and-let* ((proj (project-current))
                                                (root (project-root proj))
                                                (_ (not (equal root default-directory))))
                                       root)
                 if (file-name-absolute-p dir)
                 collect dir into absolute-dirs
                 else
                 collect (expand-file-name dir) into relative-dirs
                 and when project-root
                 collect (expand-file-name dir project-root) into relative-dirs
                 finally return (nconc (nreverse absolute-dirs) (nreverse relative-dirs))))
  gptel-agent--skills)

(defun gptel-agent--skills-system-message (agent-skills)
  "Parse AGENT-SKILLS and return the message describing known skills.

Meant to be used as a template (see `gptel-agent-read-file').

AGENT-SKILLS is a alist of skill names and associated plist as value
 (See `gptel-agent--skills').  The plist is expected to have
:description as a key."
  ;; Copied from opencode
  ;; (https://github.com/anomalyco/opencode/blob/dev/packages/opencode/src/tool/skill.ts)
  (concat "Load a skill to get detailed instructions for a specific task."
          "Skills provide specialized knowledge and step-by-step guidance."
          "Use this when a task matches an available skill's description."
          "\n<available_skills>\n"
          (mapconcat (lambda (skill-def)
                       (format "  <skill>
    <name>%s</name>
    <description>%s</description>
  </skill>"
                               (car skill-def)
                               (plist-get (cddr skill-def) :description)))
                     agent-skills "\n")
          "\n</available_skills>"))

;;;###autoload
(defun gptel-agent-update ()
  "Update agents."
  (let ((agent-files (gptel-agent--update-agents))
        ;; Load skills to be included in the system message
        (skills-str (gptel-agent--skills-system-message (gptel-agent--update-skills))))
    ;; reload agents with template expansion
    (dolist (agent-entry gptel-agent--agents)
      (let* ((name (car agent-entry))
             (agent-file (cdr (assoc name agent-files)))
             ;; Format the agent list for template substitution
             (agents-list-str
              (cl-loop for entry in gptel-agent--agents
                       unless (or (string= (car entry) name)
                                  (string= (car entry) "gptel-agent")
                                  (string= (car entry) "gptel-plan"))
                       collect (format "`%s`: %s\n"
                                       (car entry) (plist-get (cdr entry) :description))
                       into agent-list
                       finally return (apply #'concat agent-list)))
             ;; Create templates alist
             (templates (list
                         (cons "AGENTS" agents-list-str)
                         (cons "SKILLS" skills-str))))
        (when agent-file                ; Parse the agent file with templates
          (setf (alist-get name gptel-agent--agents nil t #'equal)
                (cdr (gptel-agent-read-file agent-file templates)))))))

  ;; Update the enum for Agent tool
  (setf (plist-get (car (gptel-tool-args (gptel-get-tool "Agent"))) :enum)
        (vconcat (delete "gptel-agent" (mapcar #'car gptel-agent--agents))))

  ;; Apply gptel-agent preset if it exists
  (when-let* ((gptel-agent-plist (assoc-default "gptel-agent" gptel-agent--agents nil nil)))
    (apply #'gptel-make-preset 'gptel-agent gptel-agent-plist))
  (when-let* ((gptel-plan-plist (assoc-default "gptel-plan" gptel-agent--agents nil nil)))
    (apply #'gptel-make-preset 'gptel-plan gptel-plan-plist))
  gptel-agent--agents)

;;; Sub-agent definition parsers for Markdown and Org

(defalias 'gptel-agent-validator-default #'always)

(defun gptel-agent--expand-templates (start templates)
  "Expand template variables in the current buffer from START to point-max.

START is the buffer position where to start expanding.
TEMPLATES is an alist of (VAR-NAME . VAR-VALUE) pairs.

Template variables in the format {{VAR-NAME}} are replaced with VAR-VALUE.
Substitution happens in-place in the buffer."
  (dolist (template templates)
    (let ((var-name (car template))
          (var-value (cdr template)))
      (goto-char start)
      (while (search-forward (format "{{%s}}" var-name) nil t)
        (replace-match var-value t t)))))

;; Parsing utilities for gptel subagent definition files, from
;; - Markdown files with YAML frontmatter
;; - Org files with PROPERTIES blocks

(defun gptel-agent-parse-markdown-frontmatter (file-path &optional validator templates metadata-only)
  "Parse a markdown file with optional YAML frontmatter.

FILE-PATH is the path to a markdown file.

VALIDATOR is an optional predicate function that takes a keyword symbol
and returns t if the key is allowed, nil otherwise.  If not provided,
defaults to `gptel-agent-validator-default'.

TEMPLATES is an optional alist of (VAR-NAME . VAR-VALUE) for template
expansion.  Template variables in the format {{VAR-NAME}} in the markdown
body will be replaced with VAR-VALUE.

If METADATA-ONLY is non-nil, only the header/metadata of the
preset/agent will be returned.  If TEMPLATES and METADATA-ONLY are
both non-nil, TEMPLATES will be ignored.


Returns a plist with:
- All YAML frontmatter keys as keywords
- When metadata-only is nil, :system containing the markdown body text
  after frontmatter (with templates expanded)

Signals an error if:
- The frontmatter block is malformed (opening without closing delimiter)
- A key in the frontmatter is not allowed by the validator"
  (unless validator
    (setq validator #'gptel-agent-validator-default))
  (require 'yaml)

  (with-temp-buffer
    (insert-file-contents file-path)

    ;; Check if file starts with frontmatter delimiter
    (if (not (looking-at-p "^---[ \t]*$"))
        ;; No frontmatter
        (if metadata-only
            nil  ; Requested only metadata but none exists -> return empty plist
          ;; Return plist with :system key containing entire file content
          (when templates               ;Apply template substitutions
            (gptel-agent--expand-templates (point-min) templates))
          (list :system (buffer-substring-no-properties
                         (point-min) (point-max))))
      ;; Move past opening delimiter
      (forward-line 1)
      (let ((frontmatter-start (point)))

        ;; Search for closing delimiter
        (unless (re-search-forward "^---[ \t]*$" nil t)
          (error "Malformed frontmatter: opening delimiter '---' found but no closing delimiter"))

        ;; Extract frontmatter text (from start to beginning of closing delimiter)
        (let* ((frontmatter-end (match-beginning 0))
               (frontmatter-str (buffer-substring-no-properties
                                 frontmatter-start frontmatter-end))
               (body-start (1+ (match-end 0))))

          ;; Parse YAML frontmatter
          (let ((parsed-yaml (yaml-parse-string
                              frontmatter-str
                              :object-type 'plist
                              :object-key-type 'keyword
                              :sequence-type 'list)))
            (let ((tail parsed-yaml))
              (while tail
                (let ((key (pop tail))
                      (val (pop tail)))
                  (pcase key
                    ((or :pre :post) (plist-put parsed-yaml key (eval (read val) t)))
                    (:parents (plist-put parsed-yaml key
                                         (mapcar #'intern (ensure-list (read val)))))
                    ;; Convert model string to symbol (gptel expects a symbol)
                    (:model (plist-put parsed-yaml key (intern val)))
                    ;; Convert YAML boolean strings to elisp booleans
                    ((or :confirm-tool-calls :include-reasoning)
                     (plist-put parsed-yaml key
                                (pcase val
                                  ((or "no" "false" :json-false :false) nil)
                                  ((or "yes" "true" t :json-true :true) t)
                                  ("auto" 'auto)
                                  (_ val))))))))

            ;; Validate all keys in the parsed YAML
            (let ((current-plist parsed-yaml))
              (while current-plist
                (let ((key (car current-plist)))
                  (unless (funcall validator key)
                    (error "Invalid frontmatter key: %s" key)))
                (setq current-plist (cddr current-plist))))

            (if metadata-only
                parsed-yaml
              (when templates
                ;; Apply template substitutions in place, then extract body text
                (gptel-agent--expand-templates body-start templates))
              ;; Extract the expanded body text
              (let ((expanded-body (buffer-substring-no-properties body-start (point-max))))
                (plist-put parsed-yaml :system expanded-body)))))))))

(defun gptel-agent-parse-org-properties (file-path &optional validator templates metadata-only)
  "Parse an Org file with properties in a :PROPERTIES: drawer.

FILE-PATH is the path to an Org file.

VALIDATOR is an optional predicate function that takes a keyword
symbol and returns t if the key is allowed, nil otherwise.
If not provided, defaults to `gptel-agent-validator-default'.

TEMPLATES is an optional alist of (VAR-NAME . VAR-VALUE) for template
expansion.  Template variables in the format {{VAR-NAME}} in the Org body
will be replaced with VAR-VALUE.

If METADATA-ONLY is non-nil, only the header/metadata of the
preset/agent will be returned.  If TEMPLATES and METADATA-ONLY are
both non-nil, TEMPLATES will be ignored.

The function expects a :PROPERTIES: block at the top of the file
 (before any headlines), with keys like name, description, tools,
backend, model, etc. Property names are case-insensitive and will
be converted to lowercase keyword symbols.

Returns a plist with:
- All properties from the :PROPERTIES: drawer as keywords
- When metadata-only is nil, :system containing the Org file body text
  after the property block (with templates expanded)

Signals an error if:
- A key in the property block is not allowed by the validator."
  (unless validator
    (setq validator #'gptel-agent-validator-default))

  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))

    ;; Try to get the property block at this position
    (let ((prop-range (org-get-property-block)))
      (if (not prop-range)
          ;; No property block
          (if metadata-only
              nil ; Requested only metadata but none exists -> return empty plist (nil)
            ;; Return body as :system, applying templates only when metadata-only is nil
            (when templates             ;Apply template substitutions
              (gptel-agent--expand-templates (point-min) templates))
            (list :system (buffer-substring-no-properties
                           (point-min) (point-max))))
        ;; Extract properties as an alist
        (let* ((props-alist (org-entry-properties (point-min) 'standard))
               (props-plist nil)
               (body-start (save-excursion
                             (goto-char (cdr prop-range))
                             (forward-line 1) ; Move past the :END: line
                             (while (looking-at-p "^\\s-*$") (forward-line 1))
                             (point))))

          ;; Process each property
          (dolist (pair props-alist)
            (let* ((key-str (downcase (car pair)))
                   (key-sym (intern (concat ":" key-str)))
                   (value (cdr pair)))

              (pcase key-sym
                (:context (setq value (split-string value)))
                (:tools (setq value (split-string value)))
                ;; Convert model string to symbol (gptel expects a symbol)
                (:model (setq value (intern value)))
                ;; Convert boolean strings to elisp booleans
                ((or :confirm-tool-calls :include-reasoning)
                 (setq value (pcase value
                               ((or "no" "false" "nil") nil)
                               ((or "yes" "true" "t") t)
                               ("auto" 'auto)
                               (_ value)))))

              ;; Skip CATEGORY property (added automatically by Org)
              (unless (string-equal key-str "category")
                ;; Validate the key
                (unless (funcall validator key-sym)
                  (error "Invalid property key: %s" key-sym))

                ;; Add to plist
                (setq props-plist (plist-put props-plist key-sym value)))))

          (let ((tail props-plist))
            (while tail
              (let ((key (pop tail))
                    (val (pop tail)))
                (pcase key
                  ((or :pre :post) (plist-put props-plist key (eval (read val) t)))
                  (:parents (plist-put props-plist key
                                       (mapcar #'intern (ensure-list (read val)))))))))

          ;; If only metadata requested, return the props plist (ignore templates)
          (if metadata-only
              props-plist
            (when templates
              ;; Apply template substitutions in place, then extract body text
              (gptel-agent--expand-templates body-start templates))
            ;; Extract the expanded body text
            (let ((body-text (buffer-substring-no-properties
                                  body-start (point-max))))
              (plist-put props-plist :system body-text))))))))

;;; Commands

;;;###autoload
(defun gptel-agent (&optional project-dir agent-preset)
  "Start a `gptel-agent' session in the current project.

With optional prefix arg, query for PROJECT-DIR.  Load AGENT-PRESET in
this session, which defaults to the default `gptel-agent'."
  (interactive
   (list (if current-prefix-arg
             (funcall project-prompter)
           (if-let ((proj (project-current)))
               (project-root proj)
             default-directory))
         'gptel-agent))
  (let ((gptel-buf
         (gptel (generate-new-buffer-name
                 (format "*gptel-agent:%s*"
                         (cadr (nreverse (file-name-split project-dir)))))
                nil
                (and (use-region-p)
                     (buffer-substring (region-beginning)
                                       (region-end)))
                'interactive))
        (preset-to-use (or agent-preset 'gptel-agent)))
    (with-current-buffer gptel-buf
      (setq default-directory project-dir)
      (gptel-agent-update)              ;Update all agent definitions
      (gptel--apply-preset              ;Apply the gptel-agent preset
       preset-to-use
       (lambda (sym val) (set (make-local-variable sym) val)))
      (unless gptel-max-tokens          ;Agent tasks typically need
        (setq gptel-max-tokens 8192))   ;a higher than usual value
      ;; gptel--apply-preset sets gptel--preset, so header line will pick it up
      (gptel-agent--setup-header-line preset-to-use))))

(provide 'gptel-agent)

;;; gptel-agent.el ends here
