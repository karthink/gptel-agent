;;; gptel-agent-test-parsers.el --- Tests for gptel-agent-parsers -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; ERT tests for gptel-agent-parsers module.

;;; Code:

(require 'ert)
(require 'gptel-agent)

;;; Default validator test

(ert-deftest gptel-agent-test-default-validator-accepts-all-keys ()
  "Test that the default validator accepts any key.
The set of allowed keys is not closed, so the default validator
accepts everything."
  (should (gptel-agent-validator-default :name))
  (should (gptel-agent-validator-default :description))
  (should (gptel-agent-validator-default :tools))
  (should (gptel-agent-validator-default :backend))
  (should (gptel-agent-validator-default :model))
  (should (gptel-agent-validator-default :invalid))
  (should (gptel-agent-validator-default :unknown)))

(ert-deftest gptel-agent-test-valid-file-with-default-validator ()
  "Test parsing a valid agent file with default validator."
  (let ((result (gptel-agent-parse-markdown-frontmatter
                 "./data/test-agent-with-frontmatter.md")))
    (should result)
    (should (equal (plist-get result :name) "code-reviewer"))
    (should (equal (plist-get result :description) "Expert code review specialist"))
    (should (equal (plist-get result :tools) '("Read" "Grep" "Glob" "Bash")))
    (should (eq (plist-get result :model) 'inherit))
    (should (plist-get result :system))
    (should (stringp (plist-get result :system)))))

;;; Custom validator test

(ert-deftest gptel-agent-test-custom-validator ()
  "Test that custom validators restrict allowed keys."
  (let ((custom-validator (lambda (key) (memq key '(:name :description :system)))))
    ;; Should fail with tools key present
    (should-error
     (gptel-agent-parse-markdown-frontmatter
      "./data/test-agent-with-frontmatter.md" custom-validator)
     :type 'error)
    ;; Should succeed with custom validator for file without extra keys
    (let ((result (gptel-agent-parse-markdown-frontmatter
                   "./data/test-agent-custom-validator.md" custom-validator)))
      (should result)
      (should (equal (plist-get result :name) "custom-agent"))
      (should (equal (plist-get result :description)
                     "This agent only has name and description")))))

;;;; Markdown parser test
(ert-deftest gptel-agent-test-markdown-no-frontmatter-return-system ()
  "Test that markdown files without frontmatter return plist with system prompt."
  (let ((result (gptel-agent-parse-markdown-frontmatter
                 "./data/test-agent-no-frontmatter-return-system.md")))
    (should result)
    (should (null (plist-get result :name)))
    (should (plist-get result :system))
    (should (stringp (plist-get result :system)))
    ;; Should contain specific content from the file
    (should (string-match "Simple Agent Without Frontmatter" (plist-get result :system)))
    (should (string-match "entire content of this file" (plist-get result :system)))))

(ert-deftest gptel-agent-test-markdown-no-frontmatter-with-templates ()
  "Test template expansion with markdown files without frontmatter."
  (let ((templates '(("AGENT_NAME" . "TestAgent")
                     ("CAPABILITY" . "testing"))))
    (let ((result (gptel-agent-parse-markdown-frontmatter 
                   "./data/test-agent-no-frontmatter-return-system.md"
                   nil templates)))
      (should result)
      (should (plist-get result :system))
      (should (stringp (plist-get result :system)))
      ;; Templates should be expanded in the system prompt
      (should (not (string-match "{{AGENT_NAME}}" (plist-get result :system))))
      (should (not (string-match "{{CAPABILITY}}" (plist-get result :system)))))))


(ert-deftest gptel-agent-test-multiline-yaml ()
  "Test parsing multiline YAML values."
  (let ((result (gptel-agent-parse-markdown-frontmatter "./data/test-agent-multiline.md")))
    (should result)
    (should (equal (plist-get result :name) "complex-agent"))
    ;; Description should be multiline
    (should (string-match "\n" (plist-get result :description)))
    (should (equal (plist-get result :tools) '("Read" "Edit" "Bash")))))

(ert-deftest gptel-agent-test-malformed-frontmatter ()
  "Test that missing closing delimiter triggers an error."
  (should-error
   (gptel-agent-parse-markdown-frontmatter "./data/test-agent-malformed.md")
   :type 'error)
  (let ((err (should-error
              (gptel-agent-parse-markdown-frontmatter "./data/test-agent-malformed.md"))))
    (should (string-match "no closing delimiter" (error-message-string err)))))

(ert-deftest gptel-agent-test-system-key-in-result ()
  "Test that :system key is always present with body text."
  (let ((result (gptel-agent-parse-markdown-frontmatter "./data/test-agent-with-frontmatter.md")))
    (should (plist-get result :system))
    (should (stringp (plist-get result :system)))
    ;; Body should contain the prompt text
    (should (string-match "code reviewer" (plist-get result :system)))))

(ert-deftest gptel-agent-test-parsing-markdown-property-types ()
  "Test parsing of YAML frontmatter property types from Markdown."
  (let* ((markdown-content "---
name: markdown-agent-demo
description: >
  test agent
temperature: 0.7
max-tokens: 4096
stream: false
track-media: true
track-response: false
org-convert-response: false
include-reasoning: ignore
use-context: system
include-tool-results: auto
confirm-tool-calls: auto
use-tools: force
cache: false
model: claude-sonnet-4-20250514
tools:
  - Read
  - Write
context:
  - ./protocol-epistemic-stance.org
  - ./protocol-output_format.org
---
test agent from markdown
")
         (file-path (make-temp-file "gptel-agent-markdown-test-")))
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert markdown-content))

          (let ((parsed (gptel-agent-parse-markdown-frontmatter file-path)))
            (should (equal (plist-get parsed :temperature) 0.7))
            (should (equal (plist-get parsed :max-tokens) 4096))
            (should (eq (plist-get parsed :stream) nil))
            (should (eq (plist-get parsed :track-media) t))
            (should (eq (plist-get parsed :track-response) nil))
            (should (eq (plist-get parsed :org-convert-response) nil))
            (should (eq (plist-get parsed :include-reasoning) 'ignore))
            (should (eq (plist-get parsed :use-context) 'system))
            (should (eq (plist-get parsed :include-tool-results) 'auto))
            (should (eq (plist-get parsed :confirm-tool-calls) 'auto))
            (should (eq (plist-get parsed :use-tools) 'force))
            (should (eq (plist-get parsed :cache) nil))
            (should (eq (plist-get parsed :model) 'claude-sonnet-4-20250514))
            (should (equal (plist-get parsed :tools) '("Read" "Write")))
            (should (equal (plist-get parsed :context)
                           '("./protocol-epistemic-stance.org"
                             "./protocol-output_format.org")))
            (should (stringp (plist-get parsed :system)))))
      (when (file-exists-p file-path)
        (delete-file file-path)))))

;;;; Org preset parsing
(ert-deftest gptel-agent-test-parsing-org-property-types ()
  "Test parsing of Org property types from an Org file."
  (let* ((org-content ":PROPERTIES:
:NAME: org-context-testing-agent
:DESCRIPTION: agent for testing parsing of context
:TEMPERATURE: 0.7
:MAX-TOKENS: 4096
:STREAM: nil
:TRACK-MEDIA: t
:TRACK-RESPONSE: nil
:ORG-CONVERT-RESPONSE: nil
:INCLUDE-REASONING: ignore
:USE-CONTEXT: system
:INCLUDE-TOOL-RESULTS: auto
:CONFIRM-TOOL-CALLS: auto
:USE-TOOLS: force
:CACHE: nil
:MODEL: claude-sonnet-4-20250514
:TOOLS: ./protocol_output_format.org ./protocol-epistemic-stance.org
:CONTEXT: ./protocol_output_format.org ./protocol-epistemic-stance.org
:END:
           This is the system content after the properties block.")
         (file-path (make-temp-file "gptel-agent-org-test-")))
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert org-content))

          (let ((parsed (gptel-agent-parse-org-properties file-path)))
            (should (equal (plist-get parsed :temperature) 0.7))
            (should (equal (plist-get parsed :max-tokens) 4096))
            (should (eq (plist-get parsed :stream) nil))
            (should (eq (plist-get parsed :track-media) t))
            (should (eq (plist-get parsed :track-response) nil))
            (should (eq (plist-get parsed :org-convert-response) nil))
            (should (eq (plist-get parsed :include-reasoning) 'ignore))
            (should (eq (plist-get parsed :use-context) 'system))
            (should (eq (plist-get parsed :include-tool-results) 'auto))
            (should (eq (plist-get parsed :confirm-tool-calls) 'auto))
            (should (eq (plist-get parsed :use-tools) 'force))
            (should (eq (plist-get parsed :cache) nil))
            (should (eq (plist-get parsed :model) 'claude-sonnet-4-20250514))
            (should (equal (plist-get parsed :tools)
                           '("./protocol_output_format.org"
                             "./protocol-epistemic-stance.org")))
            (should (equal (plist-get parsed :context)
                           '("./protocol_output_format.org"
                             "./protocol-epistemic-stance.org")))
            (should (stringp (plist-get parsed :system)))))
      (when (file-exists-p file-path)
        (delete-file file-path)))))

(ert-deftest gptel-agent-test-org-no-properties-return-system ()
  "Test that org files without properties return plist with system prompt."
  (let ((result (gptel-agent-parse-org-properties
                 "./data/test-agent-no-properties-return-system.org")))
    (should result)
    (should (null (plist-get result :name)))
    (should (plist-get result :system))
    (should (stringp (plist-get result :system)))
    ;; Should contain specific content from the file
    (should (string-match "Simple Org Agent Without Properties" (plist-get result :system)))
    (should (string-match "entire content of this file" (plist-get result :system)))))

(ert-deftest gptel-agent-test-org-no-properties-with-templates ()
  "Test template expansion with org files without properties."
  (let ((templates '(("AGENT_NAME" . "TestAgent")
                     ("CAPABILITY" . "testing"))))
    (let ((result (gptel-agent-parse-org-properties 
                   "./data/test-agent-no-properties-return-system.org"
                   nil templates)))
      (should result)
      (should (plist-get result :system))
      (should (stringp (plist-get result :system)))
      ;; Templates should be expanded in the system prompt
      (should (not (string-match "{{AGENT_NAME}}" (plist-get result :system))))
      (should (not (string-match "{{CAPABILITY}}" (plist-get result :system)))))))


(provide 'gptel-agent-test-parsers)

;;; gptel-agent-test-parsers.el ends here
