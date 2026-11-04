;;; gptel-agent-tools-introspection.el --- Emacs introspection tools for gptel-agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

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

;; Emacs introspection tools for gptel-agent.
;;
;; These tools provide LLMs with access to Emacs' introspection facilities:
;; - Symbol lookup and completion
;; - Documentation retrieval
;; - Source code inspection
;; - Manual/info documentation
;; - Feature and library inspection
;; - Elisp evaluation
;;
;; Adapted from ragmacs.el by Positron Solutions.

;;; Code:

(require 'gptel-request)
(require 'info)
(require 'find-func)
(eval-when-compile (require 'cl-lib))

(declare-function custom-variable-documentation "cus-edit")
(declare-function orderless-filter "orderless")

;;; Helper functions

(defun gptel-agent--introspect-manual-node-contents (manual node)
  "Return contents of NODE in Info MANUAL."
  (condition-case err
      (progn
        (save-window-excursion
          (Info-goto-node (format "(%s)%s" manual node))
          (buffer-substring-no-properties (point-min) (point-max))))
    (user-error
     (error (error-message-string err)))))

(defun gptel-agent--introspect-symbol-in-manual (symbol)
  "Return the Info documentation for SYMBOL, if it exists."
  (when-let* ((symbol (intern-soft symbol)))
    (let* (buf
           (remember (lambda (buffer alist)
                       (display-buffer-no-window (setq buf buffer) alist)))
           (display-buffer-overriding-action
            `(,remember (allow-no-window . t))))
      (info-lookup-symbol symbol #'emacs-lisp-mode)
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun gptel-agent--introspect-library-source (library-name)
  "Return the source code of LIBRARY-NAME as a string."
  (if-let ((library (find-library-name library-name)))
      (with-temp-buffer
        (progn
          (insert-file-contents library)
          (buffer-string)))
    (error "Library not found: %s" library-name)))

(defun gptel-agent--introspect-source (symbol &optional type)
  "Retrieve the source code for SYMBOL of TYPE.
SYMBOL should be a function or variable name, given as a string or symbol.
TYPE can be nil for functions, defvar for variables, or defface for faces.
Returns the source code as a string, or nil if the definition is not found."
  (when-let* ((callable (intern-soft symbol))
              (save-silently t)         ; suppresses message in
                                        ; find-file-noselect
              (vc-follow-symlinks t)     ; don't ask, we're not editing.
              (buffer-point (find-definition-noselect callable type)))
    (with-current-buffer (car buffer-point)
      (goto-char (cdr buffer-point))
      (buffer-substring-no-properties
       (point)
       (progn (if (null type)
                  (end-of-defun)
                (cond ((derived-mode-p 'c-mode)
                       (forward-sexp 2)
                       (forward-char))
                      ((derived-mode-p 'emacs-lisp-mode)
                       (forward-sexp))
                      (t (error "Unexpected file mode"))))
              (point))))))

;;; Tool declarations

(gptel-make-tool
 :function (lambda (name) (intern-soft name))
 :name "symbol_exists"
 :include t
 :category "introspection"
 :args '(( :name "symbol"
           :type string
           :description "A symbol that will be in `obarray' if they \
actually exist"))
 :description "Check if SYMBOL exists in `obarray'. \
Returns the name of a symbol if that symbol has been interned or \"nil\"
if not.  Uses `intern-soft' to perform the check.  This tool is
extremely cheap to call.")

(gptel-make-tool
 :function (lambda () (string-join load-path "\n"))
 :name "load_paths"
 :include t
 :category "introspection"
 :args nil
 :description "Return the users load paths.
This can reveal information about what packages the user has available.
You can also learn about what kind of package management they are using
and which packages are likely shadowed by their Elisp dependency
manager.  The location of default packages can tell you about the user's
Emacs installation.")

(gptel-make-tool
 :function (lambda ()
             (json-serialize (vconcat (info--filter-manual-names
                                       (info--manual-names nil)))))
 :name "manual_names"
 :include t
 :category "introspection"
 :args nil
 :description "Return a list of available manual names.
Call this tool in order to determine if a particular manual is
available.  This can also help determine which packages are available on
the user's Emacs.  This tool is a good starting point for general
questions about Emacs, Elisp, and common built-in packages.

Manuals are usually named the same as the symbol of the package prefix
that they document.  The Common Lisp manual is called \"cl\".  The Emacs
Lisp manual is called \"elisp\".

You will usually follow this call with a subsequent call to
`manual_nodes' in order to see the sections in the manual, which are
somewhat like a summary.  This call is extremely cheap and should be
used liberally.")

(gptel-make-tool
 :function (lambda (name)
             (json-serialize
              (vconcat (mapcar #'car (Info-build-node-completions name)))))
 :name "manual_nodes"
 :include t
 :category "introspection"
 :args '(( :name "manual"
           :type string
           :description "The name of the manual.
Examples include \"cl\", \"elisp\", or \"transient\"."))
 :description "Retrieve a listing of topic nodes within MANUAL.
Return value is a list of all nodes in MANUAL.  The list of topic nodes
provides a good summary of MANUAL.

MANUAL is one of the results returned from `manual_names'.  If you are
sure a manual exists, you may skip first calling `manual_names'.  When
you believe MANUAL exists, this tool is very useful to find places to
broaden your search.

You will usually follow this call with a subsequent call to
`manual_node_contents' to view the actual full contents of a node in the
manual.  This call is extremely cheap and should be used liberally.

In the Elisp manual, you can find more answers about code and
implementations that a programmer can used to deeply customize.  The
Emacs manual contains descriptions about built-in features and behavior
that can be used to understand the context for what is being programmed
in the Elisp manual.")

(gptel-make-tool
 :function #'gptel-agent--introspect-manual-node-contents
 :name "manual_node_contents"
 :include t
 :category "introspection"
 :args '(( :name "manual_name"
           :type string
           :description "The name of MANUAL.
Examples manuals include \"cl\", \"elisp\", or \"transient\".")
         ( :name "node"
           :type string
           :description "The name of the NODE in a MANUAL.
Example nodes from the elisp manual include \"Records\" or \"Sequences
Arrays \ Vectors\"."))
 :description "Retrieve the contents of NODE in MANUAL.
The return value is the full contents of NODE in MANUAL.  Contents
include high-level grouping of related functions and variables.  Hidden
behavior is described.  This tool is awesome!  You should try to call it
all the time.

Pay attention to the entries in the Menu.  You can do recursive look-ups
of more specific manual sections.  Example menu:

* Menu:

* Good Node::
* A Node::

You can recursively look up \"Good Node\" and other relevant menu nodes
in this same MANUAL.  Sometimes there are links, such as, \"*Note
Narrowing::.\".  \"Narrowing\" is a node in this example.  Use this tool
recursively.

If both Elisp and Emacs manuals are available, open both but prefer Elisp manual
style language anc content.")

(gptel-make-tool
 :function (lambda (feature)
             (if-let ((feature-symbol (intern-soft feature)))
                 (when (featurep feature-symbol)
                   feature)
               (find-library-name feature)))
 :name "features"
 :include t
 :category "introspection"
 :args '(( :name "feature"
           :type string
           :description "FEATURE to look for."))
 :description "Check if FEATURE is loaded or available.
Returns non-nil if FEATURE is loaded or available for loading.  Not all
users have all features loaded.  Before recommending the user to try a
particular solution, you might check if the necessary features are
loaded.  If you are using all built-in Emacs libraries, you don't need
to check.  Use this mainly to check for 3rd party packages that the user
would obtain from MELPA and Non-GNU ELPA etc.")

(gptel-make-tool
 :function #'gptel-agent--introspect-library-source
 :name "library_source"
 :include t
 :category "introspection"
 :args '(( :name "library"
           :type string
           :description "LIBRARY to look for."))
 :description "Read the source code for LIBRARY.
LIBRARY can either be a C or Elisp source code library.  Examples would
include \"transient\" or \"fns.c\".  When looking for C libraries, they
must contain the .c suffix.

This tool is a bit expensive, and you can usually find what you want by
looking up symbols in the package first by calling
`function_completions' and `variable_completions' to get a high-level
summary of what definitions might be contained in a library.

Watch for for sub-packages.  Some multi-file packages will have symbols
that are defined in a sub-package.  If you see a common prefix in the
function or variable completions and those symbols are not in the
top-level package, there are likely sub-packages and you should
recursively look them up.")

(gptel-make-tool
 :name "symbol_manual_section"
 :include t
 :function #'gptel-agent--introspect-symbol-in-manual
 :category "introspection"
 :args '(( :name "symbol"
           :type string
           :description "Name of a SYMBOL, such as \
\"find-file-noselect\"."))
 :description "Returns contents of manual node for SYMBOL.
SYMBOL can be a function, macro, defcustom, or defvar.  If symbol is not
known to be in a manual, this functon will return nil.

The returned manual contents are similar to the `manual_node_contents'
tool.  You sould recursively inspect any links or menu entries that look
relevant.  Check the node list for the manual if a link or menu entry
returns nil.

If you can't find anything, you should try looking up its source or
docstring next and finally try to complete the prefix of the symbol .")

(gptel-make-tool
 :name "function_source"
 :include t
 :function (lambda (symbol)
             (when-let ((symbol (intern-soft symbol)))
               (gptel-agent--introspect-source symbol)))
 :category "introspection"
 :args '(( :name "function"
           :type string
           :description "Name of a FUNCTION, such as \
\"find-file-noselect\"."))
 :description "Returns the source code for FUNCTION.
Return the source code for FUNCTION.  FUNCTION can be a function or
macro.  The signature and docstring can supply extremely valuable
information about how to call a function correctly and what behaviors
are controlled by its arguments.  You can understand the side-effects
and what variables a function reacts to by reading its body.

You can use the source code for functions to recursively look up other
functions & variables and make inferences about how implementations work
in order to connect the behaviors and implementation details that the
user will need.

Because the docstring is embedded in the source, you should prefer this
tool over just retrieving the documentation. If the result seems
incomplete, you can try returning the docstring using
`function_documentation' or the entire source for a library feature by
using `library_source'.  This tool is cheap.  Use it liberally.")

(gptel-make-tool
 :name "variable_source"
 :function (lambda (symbol)
             (when-let ((symbol (intern-soft symbol)))
               (gptel-agent--introspect-source symbol 'defvar)))
 :category "introspection"
 :include t
 :args '(( :name "variable"
           :type string
           :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
 :description "Returns the source code for VARIABLE.
Return value is the source code for VARIABLE.  VARIABLE can be a defvar
or defcustom.  The returned source code can be extremely insightful
because nothing is more accurate than looking at the code and the source
code contains the docstring too.

You can use source code for variables to see the forms used in setting
their defaults and make inferences about what forms will be interpreted
correctly.  If the result seems incomplete, you can try returning the
docstring using `variable_documentation' or the entire source for a
library feature by using `library_source'.  This tool is cheap and fast.
Call it liberally.")

(gptel-make-tool
 :name "variable_value"
 :function (lambda (symbol)
             (when-let ((symbol (intern-soft symbol)))
               (default-value symbol)))
 :category "introspection"
 :confirm t
 :include t
 :args '(( :name "variable"
           :type string
           :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
 :description "Returns the global value for VARIABLE.
Return value is the global (not buffer-local) value for VARIABLE.
VARIABLE can be a defvar or defcustom.  Use this when behavior depends
on the state of a variable or you want to infer if a package has indeed
configured a variable.  By observing expected side-effects, you can
build a coherent view of the interaction between functions and settings.

Use of this tool could leak private data, so don't call it for any
VARIABLE that contains initialized authentication data.

If the result is confusing, you can try returning the docstring using
`variable_documentation' to gain insights into the structure of values
contained.")

(gptel-make-tool
 :name "function_documentation"
 :function (lambda (symbol)
             (when-let ((symbol (intern-soft symbol)))
               (documentation symbol)))
 :category "introspection"
 :include t
 :args '(( :name "function"
           :type string
           :description "Name of a FUNCTION, such as \"mapcar\"."))
 :description "Returns the docstring for FUNCTION.
Return value is a docstring for FUNCTION.  FUNCTION can be a function or
macro.  Can be used to infer the purpose or correct forms for arguments
and behavior changes related to those arguments.  This is more reliable
than `function_source', so if `function_source' seems off, try this.
This tool is very cheap and very fast.  Call it very liberally.")

(gptel-make-tool
 :name "variable_documentation"
 :function (lambda (symbol)
             (when-let* ((symbol (intern-soft symbol)))
               (require 'cus-edit)
               (custom-variable-documentation symbol)))
 :category "introspection"
 :include t
 :args '(( :name "variable"
           :type string
           :description "Name of a VARIABLE, such as \
\"cursor-type\"."))
 :description "Returns the docstring VARIABLE.
Return value is a docstring for VARIABLE.  VARIABLE can be a defcustom
or defvar.  Can be used to infer the correct forms for setting a
variable, such as when configuring packages in use-package expressions
or leading the user through diagnosing something.  This tool is very
cheap and very fast.  Call it very liberally.")

(gptel-make-tool
 :name "function_completions"
 :function (lambda (prefix)
             (require 'orderless)
             (string-join (orderless-filter prefix obarray #'functionp) "\n"))
 :category "introspection"
 :include t
 :args '(( :name "function_prefix"
           :type string
           :description "FUNCTION_PREFIX of functions you are searching for."))
 :description "Returns a list of functions beginning with FUNCTION_PREFIX.
Use this to prepare for subsequent calls to `function_source' or
`function_documentation' to look up the source code or docstrings of
multiple functions.  You can also use this tool to verify which
functions and macros can be called.  If you want to search for all
functions defined in foo and its sub-packages, you this tool is a very
good starting point.  This tool is very cheap and very fast.  Call it
very liberally.")

(gptel-make-tool
 :name "command_completions"
 :function (lambda (prefix)
             (require 'orderless)
             (string-join (orderless-filter prefix obarray #'commandp) "\n"))
 :category "introspection"
 :include t
 :args '(( :name "command_prefix"
           :type string
           :description "COMMAND_PREFIX of commands you are searching for."))
 :description "Returns a list of commands beginning with COMMAND_PREFIX.
This tool is very similar to `function_completions' but will only return
commands that can be called interactively.  This can tell you about the
entry points where a user begins interacting with a package.  Because
commands are functions, you will follow up this tool with calls to
`function_source' and `function_documentation'.  This tool is very cheap
and very fast.  Call it very liberally.")

(gptel-make-tool
 :name "variable_completions"
 :function (lambda (prefix)
             (require 'orderless)
             (string-join (orderless-filter prefix obarray #'boundp) "\n"))
 :category "introspection"
 :include t
 :args '(( :name "variable_prefix"
           :type string
           :description "VARIABLE_PREFIX of variables you are searching for."))
 :description "Returns a list of variables beginning with VARIABLE_PREFIX.
The variables returned include defvars and custom variables.  Defvars
tell you what states a package relies on for its implementation.
Defcustom tells you what configuration options the user should know
about when configuring a package, such as if they are working on
use-package expressions.

Use this to prepare for subsequent calls to `variable_source' or
`variable_documentation' to look up the source code or docstrings of
multiple variables.  If you want to search for all variables defined
under a prefix, you this tool is a very good starting point.  This tool
is very cheap and very fast.  Call it very liberally.")

(provide 'gptel-agent-tools-introspection)
;;; gptel-agent-tools-introspection.el ends here
