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
(require 'gptel-agent-loop)
(eval-when-compile (require 'cl-lib))

(declare-function yaml-parse-string "yaml")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-root "project")
(declare-function org-get-property-block "org")
(declare-function org-entry-properties "org")
(defvar org-inhibit-startup)
(defvar project-prompter)
(defvar gptel-org-branching-context)

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
                                    ".gemini/skills/"
                                    "~/.copilot/skills/"
                                    ".github/skills/")

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

(defcustom gptel-agent-compact-prompt
  "Purpose: To create a comprehensive record that ensures no important details or context are lost between sessions.
This process prioritizes thoroughness over brevity to retain all critical information.

The contents of the summary depends on the type of the conversation:

## Coding or technical sessions working towards specific goals

- Overall purpose and goals of the interaction
- Important progress made in the current session
- Mistakes and dead-ends that should be avoided in subsequent steps
- Technical details that need to be preserved
- Key decisions and architectural changes
- Unfinished tasks and actionable next steps

Provide as much detail as necessary, and err on the side of providing too much information.  Be thorough.

## Exploratory conversations or non-technical sessions

Summarize the chat in a way that allows any LLM to continue the conversation based on the summary.

- Emphasize topics covered in the conversation
- In the order in which they were covered.  Retain the narrative flow of the conversation in your summary.
- Include points of disagreement with the user
- Be sure to include any explicit instructions provided by the user in their turns.
  You are expected to continue to follow these

Provide as much detail as necessary, and err on the side of providing too much information.  Be thorough."
  "System prompt for session compaction used by gptel agent.

Can be a string or a function that returns a string."
  :type '(choice (string :tag "Custom prompt string")
                 (function :tag "Function that returns prompt string"))
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
            (dolist (skill-file (directory-files-recursively
                                 dir "SKILL\\.md$" nil nil t))
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
          (error "Malformed frontmatter in \"%s\" : \
opening delimiter '---' found but no closing delimiter" file-path))

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
            ;; Normalize property types and handle special keys.
            (let ((tail parsed-yaml))
              (while tail
                (let ((key (pop tail))
                      (val (pop tail)))
                  (pcase key
                    ;; :pre and :post are Elisp expressions as strings.
                    ((or :pre :post) (plist-put parsed-yaml key (eval (read val) t)))
                    ;; :parents is a YAML list of strings read as a list.
                    (:parents (plist-put parsed-yaml key
                                         (mapcar #'intern (ensure-list val))))
                    ;; :context and :tools: If a string (single-line YAML), split
                    ;; by spaces. If already a list (YAML sequence), keep as-is.
                    ((or :context :tools)
                     (plist-put parsed-yaml key
                                (if (listp val) val (split-string val))))
                    ;; Numeric properties: YAML already handles these correctly
                    ;; (numbers become numbers), so no conversion needed.
                    ;; Boolean properties: YAML true -> t, YAML false -> :false.
                    ((or :stream :track-media :track-response
                         :org-convert-response)
                     (plist-put parsed-yaml key (unless (eq val :false) val)))
                    ;; Include-reasoning can be true/t, false/nil, "ignore",
                    ;; or a string (buffer name).
                    (:include-reasoning
                     (plist-put parsed-yaml key (pcase val
                                                  (:false nil)
                                                  ("ignore" 'ignore)
                                                  (_ val))))
                    ;; Symbol properties (always a symbol or nil)
                    ((or :use-context :include-tool-results :confirm-tool-calls
                         :use-tools :cache :model)
                     (plist-put parsed-yaml key
                                (pcase val
                                  (:false nil)
                                  ((pred stringp) (intern val))
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
              (if-let* ((expanded-body (buffer-substring-no-properties
                                        body-start (point-max)))
                        ((not (string-blank-p expanded-body))))
                  (plist-put parsed-yaml :system expanded-body)
                parsed-yaml))))))))

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
                             (while (and (not (eobp)) (looking-at-p "^\\s-*$"))
                               (forward-line 1))
                             (point))))

          ;; Normalize property types and handle special keys.
          (dolist (pair props-alist)
            (let* ((key-str (downcase (car pair)))
                   (key-sym (intern (concat ":" key-str)))
                   (value (cdr pair)))

              (pcase key-sym
                ;; Properties that should remain as lists
                ((or :context :tools)
                 (setq value (split-string value)))
                ;; Numeric properties
                ((or :temperature :max-tokens :num-messages-to-send)
                 (setq value (unless (string-equal value "nil")
                               (string-to-number value))))
                ;; Include-reasoning can also be a string (buffer name)
                (:include-reasoning (setq value (pcase value
                                                  ("nil" nil)
                                                  ("t" t)
                                                  ("ignore" 'ignore)
                                                  (_ value))))
                ;; Boolean properties
                ((or :stream :track-media :track-response :org-convert-response)
                 (setq value (pcase value
                               ("nil" nil)
                               ("t" t)
                               (_ value))))
                ;; Symbol properties
                ((or :use-context :include-tool-results :confirm-tool-calls
                     :use-tools :cache :model)
                 (setq value (intern value))))

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
            (if-let* ((body-text (buffer-substring-no-properties
                                  body-start (point-max)))
                      ((not (string-blank-p body-text))))
                (plist-put props-plist :system body-text)
              props-plist)))))))

;;; Commands

(defun gptel-agent-compact--callback (resp info)
  "Callback for `gptel-agent-compact'.

See `gptel-request' for the meanings of RESP and INFO."
  (let ((buf (plist-get info :buffer))
        (pos (plist-get info :position)))
    (cond
     ((not (buffer-live-p buf))
      (user-error "Session buffer \"%s\" is no longer available"
                  (buffer-name buf)))
     ((null resp)                       ;error
      (with-current-buffer buf
        (gptel--update-status
         (format " Error: %s" (gptel--to-string (plist-get info :status))) 'error))
      (message
       "Prompt compaction failed with error %S, see *Messages* buffer for details"
       (plist-get info :status))
      (let ((inhibit-message t))
        (message "Error details:\n%S" (plist-get info :error))))
     ((stringp resp)
      (with-current-buffer buf
        (goto-char pos)
        (save-restriction
          (if-let* ((region-markers (plist-get info :context)))
              (apply #'narrow-to-region region-markers)
            (narrow-to-region (point-min) pos))
          (if (or buffer-read-only
                  (get-char-property (point) 'read-only)
                  (/= (previous-single-char-property-change (point) 'read-only)
                      (point-min)))
              (user-error "Cannot compact session: read-only text in buffer")
            ;; Replace chat text
            (delete-region (point-min) (point-max))
            (insert resp)
            (unless (eq (char-before) 10) (insert "\n"))))
        (gptel--update-status " Ready" 'success)))
     ((and (consp resp) (eq (car resp) 'reasoning)) nil)
     (t (with-current-buffer buf        ;Tool called -- should not happen!
          (plist-put info :error "Compaction failed")
          (gptel--update-status " Error: Compaction failed" 'error)
          (message "Prompt compaction stalled or failed"))))))

(defun gptel-agent-compact (&optional extra post-func confirm)
  "Compact contents of buffer up to point, or region if active.

This will replace buffer text with an LLM generated summary.  For prompt
compaction instructions see `gptel-agent-compact-prompt'.

With prefix argument, prompt for EXTRA instructions.

POST-FUNC is a function to run after the compaction is complete.  It is
called with one argument, the INFO plist of the compaction request.
This plist contains request details, including the :error key which is
non-nil if the compaction request failed.  See `gptel-request' for more
information about INFO.

When CONFIRM is non-nil (the default in interactive use), seek
confirmation before proceeding."
  (interactive
   (list (and current-prefix-arg
              (read-string "Extra compaction instructions: "))
         nil t))
  (when confirm
    (unless (y-or-n-p
             (concat "Prompt compaction will replace all buffer text "
                     (if (use-region-p) "in the region." "before point.")
                     "  Proceed?"))
      (user-error "Prompt compaction canceled")))
  (gptel--update-status " Compacting..." 'warning)
  (let* ((gptel-include-reasoning)
         (gptel-use-tools)
         (gptel-org-branching-context)
         (fsm (gptel-request nil
                :system
                (if extra
                    (concat (gptel--parse-directive gptel-agent-compact-prompt t)
                            "\n\nAdditional instructions:\n\n" extra)
                  gptel-agent-compact-prompt)
                :transforms (list 'gptel--transform-add-context)
                :context (when (use-region-p)
                           (pcase-let ((`(,from . ,to) (car (region-bounds))))
                             (list (set-marker (make-marker) from)
                                   (set-marker (make-marker) to))))
                :callback #'gptel-agent-compact--callback)))
    (prog1 fsm
      (when (functionp post-func)
        (let ((info (gptel-fsm-info fsm)))
          (plist-put info :post (cons post-func (plist-get info :post))))))))

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
                'interactive)))
    (with-current-buffer gptel-buf
      (setq default-directory project-dir)
      (gptel-agent-update)              ;Update all agent definitions
      (gptel--apply-preset              ;Apply the gptel-agent preset
       (or agent-preset 'gptel-agent)
       (lambda (sym val) (set (make-local-variable sym) val)))
      (unless gptel-max-tokens              ;Agent tasks typically need
        (setq-local gptel-max-tokens 8192)) ;a higher than usual value
      (when gptel-use-header-line
        (let* ((agent-mode t)
               (switch-mode
                (lambda (&rest _)
                  (gptel--apply-preset
                   (if agent-mode 'gptel-plan 'gptel-agent)
                   (lambda (sym val) (set (make-local-variable sym) val)))
                  (setq agent-mode (not agent-mode))
                  (force-mode-line-update)))
               (display-mode
                (lambda () (concat
                       (propertize " " 'display '(space :align-to 0))
                       (format "%s" (gptel-backend-name gptel-backend))
                       (if agent-mode
                           (propertize (buttonize "[Agent]" switch-mode nil
                                                  "Switch to planning preset")
                                       'face 'font-lock-keyword-face)
                         (propertize (buttonize "[Plan]" switch-mode nil
                                                "Switch to agent preset")
                                     'face 'font-lock-doc-face))))))
          (setcar header-line-format
                  `(:eval (funcall ,display-mode))))))))

(provide 'gptel-agent)

;;; gptel-agent.el ends here
