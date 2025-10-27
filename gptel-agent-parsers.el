;;; gptel-agent-parsers.el --- Parse markdown files with YAML frontmatter -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Parsing utilities for markdown files with YAML frontmatter, specifically
;; for gptel subagent definition files.

;;; Code:

(defvar gptel-agent-allowed-keys '(:name :description :tools :backend :model)
  "Default list of allowed keys in subagent frontmatter.")

(defun gptel-agent-default-validator (key)
  "Default validator for frontmatter keys.

KEY is a keyword symbol to validate.

Returns t if KEY is in `gptel-agent-allowed-keys', nil otherwise."
  (memq key gptel-agent-allowed-keys))

(defun gptel-agent-parse-frontmatter (file-path &optional validator)
  "Parse a markdown file with optional YAML frontmatter.

FILE-PATH is the path to a markdown file.

VALIDATOR is an optional predicate function that takes a keyword
symbol and returns t if the key is allowed, nil otherwise.
If not provided, defaults to `gptel-agent-default-validator'.

Returns a plist with:
- All YAML frontmatter keys as keywords
- :system containing the markdown body text after frontmatter

If no frontmatter block exists, returns nil.

Signals an error if:
- The frontmatter block is malformed (opening without closing delimiter)
- A key in the frontmatter is not allowed by the validator"
  (unless validator
    (setq validator #'gptel-agent-default-validator))
  (require 'yaml)

  (with-temp-buffer
    (insert-file-contents file-path)

    ;; Check if file starts with frontmatter delimiter
    (if (not (looking-at-p "^---[ \t]*$"))
        nil                             ; No frontmatter

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

            ;; Validate all keys in the parsed YAML
            (let ((current-plist parsed-yaml))
              (while current-plist
                (let ((key (car current-plist)))
                  (unless (funcall validator key)
                    (error "Invalid frontmatter key: %s" key)))
                (setq current-plist (cddr current-plist))))

            ;; Extract body text
            (let ((body-str (buffer-substring-no-properties
                             body-start (if (eq (char-before (point-max)) 10)
                                            (1- (point-max)) (point-max)))))
              ;; Add the markdown body as :system key
              (plist-put parsed-yaml :system body-str))))))))

(defun gptel-agent-parse-org-properties (file-path &optional validator)
  "Parse an Org file with properties in a :PROPERTIES: drawer.

FILE-PATH is the path to an Org file.

VALIDATOR is an optional predicate function that takes a keyword
symbol and returns t if the key is allowed, nil otherwise.
If not provided, defaults to `gptel-agent-default-validator'.

The function expects a :PROPERTIES: block at the top of the file
(before any headlines), with keys like name, description, tools,
backend, model, etc. Property names are case-insensitive and will
be converted to lowercase keyword symbols.

Returns a plist with:
- All properties from the :PROPERTIES: drawer as keywords
- :system containing the Org file body text after the property block

If no property block exists, returns nil.

Signals an error if:
- A key in the property block is not allowed by the validator"
  (unless validator
    (setq validator #'gptel-agent-default-validator))

  (require 'org)

  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))

    ;; Try to get the property block at this position
    (let ((prop-range (org-get-property-block)))
      (if (not prop-range)
          nil                           ; No property block found

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
                (:tools (setq value (split-string value))))

              ;; Skip CATEGORY property (added automatically by Org)
              (unless (string-equal key-str "category")
                ;; Validate the key
                (unless (funcall validator key-sym)
                  (error "Invalid property key: %s" key-sym))

                ;; Add to plist
                (setq props-plist (plist-put props-plist key-sym value)))))

          ;; Extract body text (everything after the property block)
          (let ((body-str (buffer-substring-no-properties
                           body-start (if (eq (char-before (point-max)) 10)
                                          (1- (point-max)) (point-max)))))
            ;; Add the body as :system key
            (plist-put props-plist :system body-str)))))))

(provide 'gptel-agent-parsers)

;;; gptel-agent-parsers.el ends here
