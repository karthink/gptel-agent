;;; gptel-agent-tools-compile.el --- Compile-backed command tools for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

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

;; Compile-backed command session support for gptel-agent.
;; Provides the RunCommand and ManageCommand tools.

;;; Code:

(require 'compile)
(require 'gptel)
(require 'subr-x)
(eval-when-compile (require 'cl-lib))

(defcustom gptel-agent-command-run-wait-ms 1500
  "Milliseconds `RunCommand' waits before returning its first snapshot.

This is an asynchronous grace period intended to capture commands that start and
finish quickly, reducing unnecessary follow-up `ManageCommand' calls."
  :type 'integer
  :group 'gptel-agent)

(defvar gptel-agent--command-sessions nil
  "Alist of active and completed command sessions.

Each entry is (SESSION-ID . FSM), where FSM is the command-session FSM for
that session.")

(defvar-local gptel-agent--command-session-id nil
  "Session identifier associated with the current compilation buffer.")

(defun gptel-agent--command-session-get (session-id)
  "Return command FSM for SESSION-ID or nil if none exists."
  (cdr (assoc session-id gptel-agent--command-sessions)))

(defun gptel-agent--command-session-put (session-id fsm)
  "Store FSM for SESSION-ID and return FSM."
  (setf (alist-get session-id gptel-agent--command-sessions nil nil #'equal) fsm))

(defun gptel-agent--command-session-info (session-id)
  "Return FSM info plist for SESSION-ID."
  (when-let ((fsm (gptel-agent--command-session-get session-id)))
    (gptel-fsm-info fsm)))

(defun gptel-agent--run-command-finish (callback session-id)
  "Call CALLBACK with the current snapshot for SESSION-ID."
  (funcall callback (gptel-agent--command-session-snapshot session-id 4096)))

(defun gptel-agent--command-session-delete (session-id)
  "Delete SESSION-ID from the session table."
  (setq gptel-agent--command-sessions
        (assoc-delete-all session-id gptel-agent--command-sessions #'equal)))

(defun gptel-agent--command-status-symbol (proc)
  "Return normalized session status symbol for PROC."
  (pcase (process-status proc)
    ('run 'running)
    ('open 'running)
    ('listen 'running)
    ('stop 'running)
    ('exit 'exited)
    ('signal 'killed)
    (_ 'failed_to_start)))

(defun gptel-agent--command-collect-delta (buffer read-marker max-output-bytes)
  "Return and consume output delta from BUFFER beginning at READ-MARKER.

The return value is a plist with key :output.  OUTPUT is truncated to
MAX-OUTPUT-BYTES when non-nil and positive.  READ-MARKER is advanced to the end
of the consumed output."
  (with-current-buffer buffer
    (let* ((start (marker-position read-marker))
           (safe-start (min (max (or start 1) (point-min)) (point-max)))
           (end (point-max))
           (truncated (and (natnump max-output-bytes)
                           (> max-output-bytes 0)
                           (> (- end safe-start) max-output-bytes)))
           (read-end (if truncated (+ safe-start max-output-bytes) end))
           (raw (buffer-substring-no-properties safe-start read-end)))
      (move-marker read-marker read-end)
      (list :output raw :truncated truncated))))

(defun gptel-agent--command-session-snapshot (session-id &optional max-output-bytes)
  "Return a JSON string describing SESSION-ID.

When MAX-OUTPUT-BYTES is non-nil, limit returned output delta size."
  (let* ((fsm (or (gptel-agent--command-session-get session-id)
                  (error "Unknown session_id %s" session-id)))
         (info (gptel-fsm-info fsm))
         (proc (plist-get info :proc))
         (buffer (and (buffer-live-p (plist-get info :buffer))
                      (plist-get info :buffer)))
         (stored-status (plist-get info :status))
         (status (cond
                  ((process-live-p proc) (gptel-agent--command-status-symbol proc))
                  ((processp proc) (gptel-agent--command-status-symbol proc))
                  (t stored-status)))
         (delta (and buffer
                     (gptel-agent--command-collect-delta
                      buffer (plist-get info :read-marker) max-output-bytes)))
         (exit-code (cond
                     ((and (processp proc) (memq (process-status proc) '(exit signal)))
                      (process-exit-status proc))
                     (t (plist-get info :exit-code))))
         (signal (plist-get info :signal))
         (normalized-status (if (not (processp proc)) (or stored-status 'exited) status)))
    (setf (plist-get info :status) normalized-status)
    (when exit-code
      (setf (plist-get info :exit-code) exit-code))
    (json-serialize
     (list :session_id session-id
           :status (symbol-name normalized-status)
           :buffer (and buffer (buffer-name buffer))
           :command (plist-get info :command)
           :cwd (plist-get info :cwd)
           :started_at (plist-get info :started-at)
           :stdout_delta (plist-get delta :output)
           :output_truncated (plist-get delta :truncated)
           :exit_code exit-code
           :signal signal)
     :null-object nil :false-object :json-false)))

(defun gptel-agent--command-finish-hook (buffer message)
  "Update session state when compilation BUFFER finishes with MESSAGE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((session-id gptel-agent--command-session-id)
                  (fsm (gptel-agent--command-session-get session-id))
                  (info (gptel-fsm-info fsm))
                  (proc (plist-get info :proc)))
        (let* ((message-downcased (downcase message))
               (status (cond
                        ((processp proc) (gptel-agent--command-status-symbol proc))
                        ((string-match-p "interrupt\\|killed\\|terminated\\|signal"
                                         message-downcased)
                         'killed)
                        (t 'exited)))
               (exit-code (cond
                           ((processp proc) (process-exit-status proc))
                           ((string-match-p "finished" message-downcased) 0)
                           (t nil)))
               (signal (cond
                        ((and (processp proc) (eq (process-status proc) 'signal)) message)
                        ((string-match-p "interrupt\\|killed\\|terminated\\|signal"
                                         message-downcased)
                         (string-trim message))
                        (t nil))))
          (setf (plist-get info :buffer) buffer
                (plist-get info :status) status
                (plist-get info :finished-at) (format-time-string "%FT%TZ" (current-time))
                (plist-get info :message) (string-trim message))
          (when exit-code
            (setf (plist-get info :exit-code) exit-code))
          (when signal
            (setf (plist-get info :signal) signal))
          (gptel--fsm-transition fsm))))))

(defvar gptel-agent--command-fsm-table
  '((CMD-INIT    . ((t . CMD-SPAWN)))
    (CMD-SPAWN   . ((gptel-agent--command-info-started-p . CMD-RUNNING)
                    (t . CMD-ERR)))
    (CMD-RUNNING . ((gptel-agent--command-info-pending-signal-p . CMD-ABRT)
                    (gptel-agent--command-info-exited-p . CMD-DONE)
                    (t . CMD-RUNNING))))
  "Command-session FSM transition table.")

(defvar gptel-agent--command-fsm-handlers
  '((CMD-SPAWN gptel-agent--command-fsm-handle-spawn)
    (CMD-RUNNING gptel-agent--command-fsm-handle-running)
    (CMD-DONE gptel-agent--command-fsm-handle-done)
    (CMD-ERR gptel-agent--command-fsm-handle-error)
    (CMD-ABRT gptel-agent--command-fsm-handle-abort))
  "Command-session FSM handlers.")

(defun gptel-agent--command-info-started-p (info)
  "Non-nil when command INFO has a live process."
  (process-live-p (plist-get info :proc)))

(defun gptel-agent--command-info-exited-p (info)
  "Non-nil when command INFO process has exited."
  (when-let ((proc (plist-get info :proc)))
    (memq (process-status proc) '(exit signal))))

(defun gptel-agent--command-info-pending-signal-p (info)
  "Non-nil when command INFO has a pending signal."
  (plist-get info :signal))

(defun gptel-agent--command-fsm-handle-spawn (fsm)
  "Handle the CMD-SPAWN state for FSM."
  (let* ((info (gptel-fsm-info fsm))
         (proc (plist-get info :proc)))
    (setf (plist-get info :status)
          (if (process-live-p proc) 'running 'failed_to_start))
    (gptel--fsm-transition fsm)))

(defun gptel-agent--command-fsm-handle-running (fsm)
  "Handle the CMD-RUNNING state for FSM."
  (let* ((info (gptel-fsm-info fsm))
         (proc (plist-get info :proc)))
    (when (process-live-p proc)
      (when (and (stringp (plist-get info :stdin))
                 (> (length (plist-get info :stdin)) 0))
        (process-send-string proc (plist-get info :stdin))
        (setf (plist-get info :stdin) nil))
      (when (plist-get info :close-stdin)
        (ignore-errors (process-send-eof proc))
        (setf (plist-get info :close-stdin) nil)))))

(defun gptel-agent--command-fsm-handle-done (fsm)
  "Handle the CMD-DONE state for FSM."
  (let* ((info (gptel-fsm-info fsm))
         (proc (plist-get info :proc)))
    (setf (plist-get info :status) 'exited
          (plist-get info :exit-code)
          (and (processp proc) (process-exit-status proc)))))

(defun gptel-agent--command-fsm-handle-error (fsm)
  "Handle the CMD-ERR state for FSM."
  (setf (plist-get (gptel-fsm-info fsm) :status) 'failed_to_start))

(defun gptel-agent--command-fsm-handle-abort (fsm)
  "Handle the CMD-ABRT state for FSM."
  (let* ((info (gptel-fsm-info fsm))
         (proc (plist-get info :proc))
         (kill-signal (plist-get info :signal)))
    (when (process-live-p proc)
      (pcase kill-signal
        ("SIGINT" (ignore-errors (interrupt-process proc)))
        ("SIGTERM" (ignore-errors (signal-process proc 'SIGTERM)))
        ("SIGKILL" (ignore-errors (kill-process proc)))))
    (setf (plist-get info :status) 'killed
          (plist-get info :signal) nil)))

(defun gptel-agent--run-command (callback command &optional cwd env timeout-ms pty stdin-mode)
  "Start COMMAND using `compilation-start' and return a session description.

CALLBACK is invoked asynchronously with the initial session snapshot.
CWD is the working directory.  ENV is an optional alist or plist of environment
overrides.  TIMEOUT-MS is currently accepted for API compatibility and recorded
in the session metadata, but timeout enforcement is not yet implemented.  PTY
selects comint-backed execution when non-nil.  STDIN-MODE is recorded as part of
the session metadata."
  (unless (stringp command)
    (error "COMMAND must be a string"))
  (let* ((default-directory (expand-file-name (or cwd default-directory)))
         (process-environment
          (append
           (mapcar (lambda (entry)
                     (format "%s=%s" (car entry) (cdr entry)))
                   (cond
                    ((null env) nil)
                    ((and (listp env) (keywordp (car env)))
                     (cl-loop for (key val) on env by #'cddr
                              collect (cons (substring (symbol-name key) 1) val)))
                    ((listp env) env)
                    (t (error "ENV must be an alist or plist"))))
           process-environment))
         (session-id (format "cmd_%s_%06d"
                             (format-time-string "%Y%m%d%H%M%S")
                             (random 1000000)))
         (buffer-name-function
          (lambda (_mode)
            (format "*gptel-agent-command:%s*" session-id)))
         (buffer (compilation-start command (and pty t) buffer-name-function))
         (proc (ignore-errors (get-buffer-process buffer)))
         (started-at (format-time-string "%FT%TZ" (current-time)))
         (fsm (gptel-make-fsm
               :state 'CMD-INIT
               :table gptel-agent--command-fsm-table
               :handlers gptel-agent--command-fsm-handlers
               :info (list :session-id session-id
                           :command command
                           :cwd default-directory
                           :env env
                           :timeout-ms timeout-ms
                           :pty (and pty t)
                           :stdin-mode stdin-mode
                           :started-at started-at
                           :buffer (and (buffer-live-p buffer) buffer)
                           :read-marker (with-current-buffer buffer
                                          (copy-marker (point-min) nil))
                           :proc proc
                           :status (if (process-live-p proc) 'running 'failed_to_start)
                           :stdin nil
                           :close-stdin nil
                           :signal nil))))
    (unless (buffer-live-p buffer)
      (error "Failed to create compilation buffer for command"))
    (with-current-buffer buffer
      (setq-local gptel-agent--command-session-id session-id)
      (add-hook 'compilation-finish-functions #'gptel-agent--command-finish-hook nil t)
      (gptel--fsm-transition fsm))
    (gptel-agent--command-session-put session-id fsm)
    (run-at-time (/ gptel-agent-command-run-wait-ms 1000.0) nil
                 #'gptel-agent--run-command-finish callback session-id)
    nil))

(defun gptel-agent--manage-command (session-id &optional wait-ms stdin close-stdin signal max-output-bytes)
  "Manage or inspect a command session identified by SESSION-ID.

WAIT-MS waits briefly for process state changes or additional output.
STDIN is sent to the session process when supplied.  CLOSE-STDIN sends
EOF.  SIGNAL may be one of SIGINT, SIGTERM or SIGKILL.  MAX-OUTPUT-BYTES
limits the returned output delta."
  (let* ((fsm (or (gptel-agent--command-session-get session-id)
                  (error "Unknown session_id %s" session-id)))
         (info (gptel-fsm-info fsm))
         (proc (plist-get info :proc)))
    (when (and stdin (not (process-live-p proc)))
      (error "Cannot send stdin to finished session %s" session-id))
    (when (and signal (not (process-live-p proc)))
      (error "Cannot signal finished session %s" session-id))
    (when (and (stringp stdin) (> (length stdin) 0))
      (plist-put info :stdin stdin))
    (when close-stdin (plist-put info :close-stdin t))
    (when signal (plist-put info :signal signal))
    (when (and (process-live-p proc) (natnump wait-ms) (> wait-ms 0))
      (accept-process-output proc (/ wait-ms 1000.0)))
    (when (process-live-p proc) (plist-put info :status 'running))
    (gptel--fsm-transition fsm)
    (gptel-agent--command-session-snapshot session-id max-output-bytes)))

;;; Compile tool declarations
(gptel-make-tool
 :name "RunCommand"
 :async t
 :function #'gptel-agent--run-command
 :description "Start a long-running command session.

Use this for commands that may produce output over time, need to keep running
across multiple interactions, or may require later input, waiting, or signal
handling.

Prefer this over a one-shot command when you need to inspect incremental output
or control the process after launch.

Returns a JSON object containing the session_id, status, buffer and any
initial output captured so far."
 :args '(( :name "command"
           :type string
           :description "Command line to run.")
         ( :name "cwd"
           :type string
           :description "Working directory for the process; defaults to the current directory."
           :optional t)
         ( :name "env"
           :type object
           :description "Environment overrides as a plist or alist of KEY=VALUE pairs."
           :optional t)
         ( :name "timeout_ms"
           :type integer
           :description "Reserved timeout budget in milliseconds; recorded but not enforced yet."
           :optional t)
         ( :name "pty"
           :type boolean
           :description "Use a pty-backed process when non-nil."
           :optional t)
         ( :name "stdin_mode"
           :type string
           :description "Hint for how stdin will be used, such as none, buffered, or interactive."
           :optional t))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "ManageCommand"
 :function #'gptel-agent--manage-command
 :description "Inspect or control an existing command session started by `RunCommand`.

Use this to wait for more output, send input, close stdin, or signal the
running process. It returns a JSON object containing the current status and the
output delta since the last `ManageCommand` or `RunCommand` call for that
session. Leave `stdin` unset or empty when you only want to poll."
 :args '(( :name "session_id"
           :type string
           :description "Session identifier returned by `RunCommand`.")
         ( :name "wait_ms"
           :type integer
           :description "Milliseconds to wait for new output or process state changes before returning."
           :optional t)
         ( :name "stdin"
           :type string
           :description "Text to send to the process's standard input. Omit or pass an empty string to only poll."
           :optional t)
         ( :name "close_stdin"
           :type boolean
           :description "Send EOF to the process after any stdin text."
           :optional t)
         ( :name "signal"
           :type string
           :enum ["SIGINT" "SIGTERM" "SIGKILL"]
           :description "Signal to send to the running process."
           :optional t)
         ( :name "max_output_bytes"
           :type integer
           :description "Maximum output bytes to include in the returned delta."
           :optional t))
 :category "gptel-agent"
 :confirm t
 :include t)

(provide 'gptel-agent-tools-compile)
;;; gptel-agent-tools-compile.el ends here
