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

Each directory listed here should contain agent skills. An agent
skill is a directory with at least one file named \"SKILL.md\".

Relative paths are resolved against the current directory and
also against the project root when searching for skills.

Relative directory locations will be take precedence over absolute
locations. If multiple skills share the same name, the one in the
directory listed earlier takes precedence.

See https://agentskills.io for more details on agentskills."
  :type '(repeat directory)
  :group 'gptel-agent)

;;; State update
(defvar gptel-agent--agents nil)

;;;###autoload
(defun gptel-agent-read-file (agent-file &optional templates metadata-only)
  "Read a preset/agent from AGENT-FILE.

If TEMPLATES is non-nil, read the system-prompt with templates applied
to them.  TEMPLATES should be an alist of (VAR-NAME . VAR-VALUE) for
template expansion. Template variables in the format {{VAR-NAME}} in
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
              (pcase-let ((`(,name . ,agent-plist) ;call without templates
                           (gptel-agent-read-file agent-file)))
                (setf (alist-get name gptel-agent--agents nil t #'equal)
                      agent-plist)
                (push (cons name agent-file) agent-files))))
          gptel-agent-dirs)
    agent-files))

;;;###autoload
(defun gptel-agent-update ()
  "Update agents."
  ;; Load skills to be included in the system message
  (gptel-agent--update-skills)

  (let ((agent-files (gptel-agent--update-agents))
        (skills-str (gptel-agent--skills-system-message)))
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
            ;; Requested only metadata but none exists -> return empty plist (nil)
            nil
          ;; Return plist with :system key containing entire file content
          (let ((file-content (buffer-string)))
            (if templates
                (progn                    ;Apply template substitutions
                  (gptel-agent--expand-templates (point-min) templates)
                  ;; Extract the expanded body text
                  (list :system (buffer-substring-no-properties (point-min) (point-max))))
              ;; Return plist with system content as-is
              (list :system file-content))))

      ;; Move past opening delimiter
      (forward-line 1)
      (let ((frontmatter-start (point)))

        ;; Search for closing delimiter
        (unless (re-search-forward "^---[ \t]*$" nil t)
          (error "Malformed frontmatter: opening delimiter '---' found but no closing delimiter"))

        ;; Extract frontmatter text (from start to beginning of closing delimiter)
        (let* ((frontmatter-end (match-beginning 0))
               (frontmatter-str (buffer-substring-no-properties frontmatter-start frontmatter-end))
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
                                         (mapcar #'intern (ensure-list (read val)))))))))

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
- A key in the property block is not allowed by the validator"
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
              ;; Requested only metadata but none exists -> return empty plist (nil)
              nil
            ;; Return body as :system, applying templates only when metadata-only is nil
            (let ((file-content (buffer-string)))
              (if templates
                  (progn                  ;Apply template substitutions
                    (gptel-agent--expand-templates (point-min) templates)
                    ;; Extract the expanded body text
                    (list :system (buffer-substring-no-properties (point-min) (point-max))))
                ;; Return plist with system content as-is
                (list :system file-content))))

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
                (:tools (setq value (split-string value))))

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

;;; Agentskill loading and processing

(defvar gptel-agent--skills nil
  "Known skills alist.

The key is the name. The value is a cons (LOCATION . SKILL-PLIST).
LOCATION is path to the skill's directory. SKILL-PLIST is the header
of the corresponding SKILL.md as a plist.")

(defun gptel-agent--update-skills ()
  "Update the known skills list from `gptel-agent-skill-dirs'."
  (setq gptel-agent--skills nil)
  (mapcar (lambda (dir)
            (when (file-exists-p dir)
             (dolist (skill-file (directory-files-recursively dir "SKILL\\.md"))
              (pcase-let ((`(,name . ,skill-plist)
                           (gptel-agent-read-file skill-file)))
                ;; validating skill definition
                (if (plist-get skill-plist :description)
                    (setf (alist-get name gptel-agent--skills nil nil #'string-equal)
                          (cons (file-name-directory skill-file) skill-plist))
                  (warn "Skill %s (at %s) does not have a description. Ignoring %s skill."
                        name skill-file name))))))
          ;; To preserve precedence, the list should be reversed and resolved
          ;; relative names should be at the end.
          (cl-loop for dir in gptel-agent-skill-dirs
                with project-root = (when-let* ((proj (project-current))
                                                (root (project-root proj))
                                                (_ (not (equal root default-directory))))
                                      root)
                if (file-name-absolute-p dir)
                  collect dir into absolute-dirs
                else
                  collect (expand-file-name dir) into relative-dirs
                  and
                  when project-root
                    collect (expand-file-name dir project-root) into relative-dirs
                finally return (append (reverse absolute-dirs) (reverse relative-dirs))))
  gptel-agent--skills)

(defun gptel-agent--skills-system-message ()
  "Returns the message describing the list of known skills."
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
                     gptel-agent--skills "\n")
          "\n</available_skills>"))

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
                'interactive)))
    (with-current-buffer gptel-buf
      (setq default-directory project-dir)
      (gptel-agent-update)              ;Update all agent definitions
      (gptel--apply-preset              ;Apply the gptel-agent preset
       (or agent-preset 'gptel-agent)
       (lambda (sym val) (set (make-local-variable sym) val)))
      (unless gptel-max-tokens          ;Agent tasks typically need
        (setq gptel-max-tokens 8192))   ;a higher than usual value
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
