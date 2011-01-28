;;; yaqi-mode-el -- Major mode for editing YAQi files
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Wed Aug 19 23:24:17 2009 (+0100)
;; Version: 0.5
;; Last-Updated: Tue Sep  1 12:22:02 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 6
;; URL: Not yet available
;; Keywords: YAQi major-mode
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; The major mode for editing YAQi code.
;;
;; Based heavily on lisp-mode and scheme-mode with inspiration from font-lock,
;; python-mode and simula-mode supplied with emacs-23 and a couple of ideas from
;; clojure-mode.
;;
;; All three commenting styles are supported:
;;
;;     #| nested block comments |#
;;     ## single-line comments
;;     #; (statement comments)
;;
;; but the single statement comment starting with `#;' currently only supports
;; s-expressions.
;;
;; -----------------------------------------------------------------------------
;;; Change log:
;;
;; Version 0.1
;; * Initial release.
;; Version 0.2
;; * Added support for s-expression comments.
;; Version 0.3
;; * Added special indentation handling for YAQi infix symbols defined in
;;   `yaqi-special-symbol-indent-regexp' e.g. `->' which if on new line are
;;   indented one level."
;; Version 0.4
;; * Added special indentation handling for forms for which all but the last
;;   specified number of arguments should be indented.  This is useful for the
;;   YAQi `let' in which the last argument is special.
;; * Added indentation of the arguments in a function call form.
;; * Split `define' and 'datatype' font-locking so that the function and type
;;   names can have different fonts.
;; * Changed the default indentation style of `if' to indent the condition and
;;   execution statements the same amount.
;; Version 0.5
;; * Added better support for block-comments:
;;   + Indent the delimiters corresponding to the current indentation level.
;;   + Indent first internal line as required -- not automatically re-indented.
;;   + Further lines follow the indentation level of the first internal line.
;; -----------------------------------------------------------------------------
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; -----------------------------------------------------------------------------
;;; Code:

;; Include support for better commenting/un-commenting
(require 'newcomment)

;; -----------------------------------------------------------------------------
;;;  Customization variables

(defgroup yaqi nil
  "Editing mode for the YAQi programming language."
  :group 'languages
  :version "0.3"
  :link '(emacs-commentary-link "yaqi"))

;;;###autoload
(defcustom yaqi-special-symbol-indent-regexp "->"
  "Regular expression for the YAQi symbols to be indented one additional level."
  :group 'yaqi
  :type 'regexp)

;;;###autoload
(defcustom yaqi-indent-optional-function-alist nil
  "Alist of indentation methods for standard YAQi functions.
Each element is a cons-cell (FUNCTION . INDENTATION-METHOD)."
  :group 'yaqi
  :type
  '(alist
    :key-type symbol
    :value-type
    (choice

     (const :tag "Handle this function like a `def' construct: treat the
second line as the start of a `body'" defun)

     (integer :tag "The first NUMBER arguments of the
function are `distinguished' arguments; the rest are considered
the body of the expression.  A line in the expression is indented
according to whether the first argument on it is distinguished or
not.  If the argument is part of the body, the line is indented
`lisp-body-indent' more columns than the open-parenthesis
starting the containing expression.  If the argument is
distinguished and is either the first or second argument, it is
indented _twice_ that many extra columns.  If the argument is
distinguished and not the first or second argument, the line uses
the standard pattern.")

     (symbol :tag "SYMBOL should be a function name; that
function is called to calculate the indentation of a line within
this expression.  The function receives two arguments:

STATE
    The value returned by `parse-partial-sexp' (a Lisp
    primitive for indentation and nesting computation) when it
    parses up to the beginning of this line.

POS
    The position at which the line being indented begins.

It should return either a number, which is the number of columns
of indentation for that line, or a list whose car is such a
number.  The difference between returning a number and returning
a list is that a number says that all following lines at the same
nesting level should be indented just like this one; a list says
that following lines might call for different indentations.  This
makes a difference when the indentation is being computed by
`C-M-q'; if the value is a number, `C-M-q' need not recalculate
indentation for the following lines until the end of the list."))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.yaqi\\'" . yaqi-mode))

;;;###autoload
(add-to-list 'same-window-buffer-names "*YAQi*")

;; -----------------------------------------------------------------------------
;;;  YAQi mode font-locking

(defconst yaqi-font-lock-keywords-1
  (eval-when-compile
    (list
     '("(\\(define\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(datatype\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-type-face nil t))
     '("\\<\\(error:\\)\\>\\(.*\\)"
       (1 font-lock-warning-face) (2 font-lock-warning-face nil t))
     ))
  "Highlight function declarations, strings, comments and errors.")

(defconst yaqi-font-lock-keywords-2
  (append yaqi-font-lock-keywords-1
   (eval-when-compile
     (list
      ;; Control structures
      (cons
       (concat
        "(" (regexp-opt '("do" "if" "let") t) "\\>")
       '(1 font-lock-builtin-face))
      ;; Control modifiers
      (cons
       (regexp-opt
        '("where" "when" "is" "->" "<-" "-->" ">>" "_" ":") 'words)
       'font-lock-builtin-face)
      ;; Built-in types
      (cons
       (regexp-opt
        '("array" "boolean" "character" "list" "number" "string" "symbol"
          "variable") 'words)
       'font-lock-type-face)
      ;; Built-in constants
      (cons
       (regexp-opt
        '("true" "false" "yes" "no" "fail!") 'words)
       'font-lock-constant-face)
      ;; Tuples
      '("[\(\[]\\(@p\\)\\>" (1 font-lock-type-face))
    )))
  "In addition to level 1, highlight all language keywords, including type names
and named constant values.")

(defconst yaqi-font-lock-keywords-3
  (append yaqi-font-lock-keywords-2
   (eval-when-compile
     (list
      ;; Exported non-system functions
      (cons
       (regexp-opt
        '("&&" "mode" "name" "typecheck" "wff" "verified" "call" "-*-" "-s-"
          "cut" "-*-" "done" "lazy" "in" "out" "fail!" "=!" "profiled")
        'words) 'font-lock-keyword-face)
      ;; System functions
      (cons
       (concat
        "("
        (regexp-opt
         '("<e>" "and" "append" "apply" "assoc" "assoc-type"
           "bind" "boolean?"
           "call" "cd" "character?" "compile" "complex?" "concat" "congruent?"
           "cons?" "cons"
           "datatype" "debug" "declare" "define" "defcc" "delete-file"
           "describe" "destroy" "difference" "document" "documentation" "dump"
           "echo" "element?" "empty?" "error" "eval" "explode"
           "fail-if" "findall" "fix" "float?" "freeze" "fst" "fun"
           "gensym" "get-array" "get-prop"
           "head"
           "identical" "if-with-checking" "if-without-checking" "include"
           "include-all-but" "input" "input+" "integer?" "inferences"
           "intersection"
           "length""lineread" "load"
           "m-prolog" "make-array" "make-string" "map" "mapcan" "maxinferences"
           "multi"
           "newsym" "newvar" "not" "nth" "number?"
           "occurrences" "occurs-check" "or" "opaque" "output"
           "print" "profile" "preclude" "preclude-all-but" "profile-results"
           "prolog?" "ps" "put-array" "put-prop"
           "quit"
           "random" "rational?" "read-char" "read-file-as-charlist" "read-file"
           "real?" "remove" "return" "reverse" "round" "rule"
           "s-prolog" "save" "save-image" "set" "sfun" "snd" "specialise"
           "speed" "spy" "sqrt" "step" "string?" "strong-warning" "sugarlist"
           "sugar" "subst" "symbol?" "synonyms"
           "tail" "tc" "thaw" "time" "track" "transparent" "tuple?" "type"
           "typecheck"
           "unassoc-type" "undebug" "union" "unprofile" "unsugar" "untrack"
           "unspecialise"
           "value" "variable?" "version"
           "warn" "write-to-file"
           "y-or-n?"
           "qi_>" "qi_<" "qi_>=" "qi_<=" "qi_=" "qi_="
           "+" "*" "/" "/." "-" "==") t) "\\>") 'font-lock-keyword-face)
      ;; Global variables with *earmuffs*
      '("\\<\\*\\w+\\*\\>" . font-lock-constant-face)
      ;; Variable names starting with a capital letter
      '("\\<[A-Z]\\w*\\>" . font-lock-variable-name-face)
      ;; Function name in a call e.g. (func-name args)
      '("(\\(\\w*\\)\\>" (1 font-lock-function-name-face))
      )))
   "In addition to level 2, highlight the symbols being defined in functions and
variable declarations, and all builtin function names, wherever they appear.")

(defconst yaqi-font-lock-headings
  (eval-when-compile
    (list
     '("^### [^ ].*" 0 'outline-2 t)
     '("^###  [^ ].*" 0 'outline-3 t)
     '("^###   [^ ].*" 0 'outline-4 t)
     ))
  "Set the fonts for three levels of headings in YAQi mode.")

(defvar yaqi-font-lock-keywords
  (append yaqi-font-lock-keywords-3 yaqi-font-lock-headings)
  "Default expressions to highlight in YAQi mode.")

(defun yaqi-font-lock ()
  "Set up font-locking"
  (interactive)
  ;;(set (make-local-variable 'font-lock-defaults) '(yaqi-font-lock-keywords))
  (set (make-local-variable 'font-lock-defaults)
       '((yaqi-font-lock-keywords)
         nil nil (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 124"))
         beginning-of-defun
         (font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . yaqi-font-lock-syntactic-face-function)
         (parse-sexp-lookup-properties . t)
         (font-lock-extra-managed-props syntax-table)))
  (set (make-local-variable 'font-lock-comment-start-skip) "##+ *"))

;; -----------------------------------------------------------------------------
;;;  YAQi mode syntax table

;; Setup the syntax table based heavily on that for Lisp in lisp-mode.el and
;; adapted for YAQi:
;; [ and ] are open and close delimiter characters.
(defvar yaqi-mode-syntax-table
  (let ((table (make-syntax-table)))
    (let ((i 0))
      (while (< i ?0)
        (modify-syntax-entry i "_   " table)
        (setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
        (modify-syntax-entry i "_   " table)
        (setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
        (modify-syntax-entry i "_   " table)
        (setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
        (modify-syntax-entry i "_   " table)
        (setq i (1+ i)))
      ;; Used in flonum symbols
      (modify-syntax-entry ?. "_    " table)
      (modify-syntax-entry ?\s "    " table)
      ;; Non-break space acts as whitespace.
      (modify-syntax-entry ?\x8a0 " " table)
      (modify-syntax-entry ?\t "    " table)
      (modify-syntax-entry ?\f "    " table)
      (modify-syntax-entry ?` "'    " table)
      (modify-syntax-entry ?' "'    " table)
      (modify-syntax-entry ?, "'    " table)
      (modify-syntax-entry ?@ "'    " table)
      (modify-syntax-entry ?\" "\"  " table)
      (modify-syntax-entry ?\\ "\\  " table)
      (modify-syntax-entry ?\( "()  " table)
      (modify-syntax-entry ?\) ")(  " table)
      (modify-syntax-entry ?\[ "(]  " table)
      (modify-syntax-entry ?\] ")[  " table)
      (modify-syntax-entry ?\{ "(}  " table)
      (modify-syntax-entry ?\} "){  " table)

      ;; Define ## as the start of a line comment
      ;; Define nested comments of the form #| ... #| ... |# ... |#
      ;; And also ; as the second character in #;(...) sexp-comments.
      (modify-syntax-entry ?#  "' 124" table)
      (modify-syntax-entry ?|  ". 23bn" table)
      (modify-syntax-entry ?\n ">   "  table)
      (modify-syntax-entry ?\; "' 2"    table)
      )
    table)
  "Syntax table for YAQi-mode")

(defconst yaqi-statement-comment-syntax-table
  (let ((st (make-syntax-table yaqi-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st)
  "Additional syntax table for statement comments starting with `#;'.")

(defun yaqi-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
             (eq (char-after (nth 8 state)) ?#)
             (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's an statement-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
            (end
             (condition-case err
                 (let ((parse-sexp-lookup-properties nil))
                   (goto-char (+ 2 (nth 8 state)))
                   ;; FIXME: this doesn't handle the case where the sexp
                   ;; itself contains a #; comment.
                   (forward-sexp 1)
                   (point))
               (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2)
                             'syntax-table yaqi-statement-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

;; -----------------------------------------------------------------------------
;;;  YAQi mode indentation

(defun calculate-yaqi-indent (&optional parse-start)
  "Return appropriate indentation for current line as YAQi code.
In the usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression.

Based on `calculate-lisp-indent' but with better handling for the indentation
of the text in #| |# block-comments"
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (if parse-start
          (goto-char parse-start)
        (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (setq paren-depth (elt state 0)) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (= (point) calculate-lisp-indent-last-sexp)
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((integerp (elt state 4))
               ;; Inside a block comment
               ;; Indent start and end according to comment level
               (cond ((looking-at "^[ \t]*#|")
                      (+ (* (elt state 4) lisp-body-indent) normal-indent))
                     ((looking-at "^[ \t]*|#")
                      (+ (* (1- (elt state 4)) lisp-body-indent) normal-indent))
                     (t
                      ;; Otherwise indent following previous comment line
                      (skip-chars-backward " \t\n")
                      (while (and (not (bolp)))
                        (re-search-backward "\\(^[ \t]*#|\\)\\|^"))
                      ;; If the previous line is the beginning of the comment
                      ;; do not change the indentation
                      (if (match-end 1)
                          nil
                        ;; If the previous line is text indent to match
                        (skip-chars-forward " \t\n")
                        (prog1
                            (current-column)
                          (goto-char indent-point))))))
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (and (not (looking-back "^[ \t]*\\|([ \t]+"))
                                   (or (not containing-sexp)
                                       (< (1+ containing-sexp) (point))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp
                           (point)
                           calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (save-excursion (beginning-of-line) (point))
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(defun yaqi-indent-specform (count state indent-point normal-indent)
  "Special-form indentation function based on `lisp-indent-specform'
but with the additional of handling negative COUNT which is treated as
the number of arguments at the end of the form to be excluded from special
indentation."
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; In the case of a negative count add the total number of arguments
    ;; to obtain the number to be specially indented
    (if (< count 0)
        (save-excursion
          (while (and (not (eobp))
                      (condition-case ()
                          (progn
                            (setq count (1+ count))
                            (forward-sexp 1)
                            (not (looking-at "[ \t\n]*\\s)")))
                        (error nil))))
          (setq i count)))
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; lisp-body-indent, else normal indent.  With lisp-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 lisp-body-indent))
                  containing-form-start)
          (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
        normal-indent))))

;; 4. nil if outside a comment, t if inside a non-nestable comment,
;; else an integer (the current comment nesting).

(defun yaqi-indent-line (&optional whole-exp)
  "Indent current line as YAQi code.
With argument, indent any additional lines of the same expression
rigidly along with this one.

Special handling is included for YAQi infix symbols in defined in
`yaqi-special-symbol-indent-regexp' e.g. `->' which if on new
line are indented one level.

Adapted from `lisp-indent-line'."
  (interactive "P")
  (let ((indent (calculate-yaqi-indent))
        shift-amt
        end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "###"))
        ;; Don't alter indentation of a ### comment line
        ;; or a line that starts in a string.
        (goto-char (- (point-max) pos))
      (when (listp indent)
        (setq indent (car indent)))
      ;; Indent special YAQi keywords if lisp-indent-offset is an integer
      (when (and (integerp lisp-indent-offset)
                 (looking-at yaqi-special-symbol-indent-regexp))
        ;;(when (looking-at yaqi-special-symbol-indent-regexp)
        (setq indent (+ indent lisp-body-indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))
    ;; If desired, shift remaining lines of expression the same amount.
    (and whole-exp (not (zerop shift-amt))
         (save-excursion
           (goto-char beg)
           (forward-sexp 1)
           (setq end (point))
           (goto-char beg)
           (forward-line 1)
           (setq beg (point))
           (> end beg))
         (indent-code-rigidly beg end shift-amt))))

(defun yaqi-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a YAQi function
which has a non-nil property `yaqi-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style;
* a positive integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a negative integer N, meaning indent all but the last N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation.

Adapted from `lisp-indent-function'."
  (let ((normal-indent (current-column))
        funform)
    ;; Go to the opening bracket of the form
    (goto-char (elt state 1))
    ;; Assume that if the form is delimited by () it is a function call
    ;; NB in YAQi normal lists are delimited by []
    (setq funform (looking-at "("))
    ;; Now move forward 1 char to the first sexp
    (forward-char)
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\w\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (get (intern-soft function) 'yaqi-indent-function))
        (cond ((eq method 'defun)          ; Definition
               (lisp-indent-defform state indent-point))
              ((integerp method)           ; Special form using standard indent
               (yaqi-indent-specform method state indent-point normal-indent))
              ((and funform (null method)) ; Non-system function call
               (yaqi-indent-specform 0 state indent-point normal-indent))
              (method                      ; Special indentation function
               (funcall method state indent-point normal-indent)))))))

(defun yaqi-put-indent (sym indent)
  "Helper function to specify the indentation style INDENT of the
given symbol SYM."
  (put sym 'yaqi-indent-function indent)
  (put (intern (format "yaqi/%s" (symbol-name sym)))
       'yaqi-indent-function indent))

(defvar yaqi-indent-function-alist
  '((define . defun)
    (datatype . defun)
    (/. . 0)
    (fun . 0)
    (sfun . 0)
    (let . -1)
    (do . 0)
    (if . 0))
  "Alist of indentation methods for standard YAQi functions.")

(defun yaqi-put-indent-function-alist (function-alist)
  (mapcar (lambda (x)
            (yaqi-put-indent (car x) (cdr x))) function-alist))

(yaqi-put-indent-function-alist yaqi-indent-function-alist)

;; -----------------------------------------------------------------------------
;;; Better automatic commenting/un-commenting

;; From http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the
end of the line, then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the
end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; -----------------------------------------------------------------------------
;;;  Key-bindings

(defvar yaqi-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\C-c\C-y" 'yaqi-shell)
    (define-key map "\C-c\C-c" 'comment-dwim-line)
    map)
  "YAQi mode key map which inherits the standard Lisp key map.")

;; -----------------------------------------------------------------------------
;;;  Menu

(defvar yaqi-mode-basic-menu-items
  '(["Run YAQi" yaqi-shell
     :help "Run YAQi shell in separate buffer"
     :active (not (and (boundp inferior-yaqi-buffer)
                       (get-buffer-process inferior-yaqi-buffer)))]
    ["[Un]comment Region/Line" comment-dwim-line
     :help "Comment or uncomment each line in the region"
     :active mark-active]
    ["Indent Region" indent-region
     :help "Indent each nonblank line in the region"
     :active mark-active]
    ["Indent Line" indent-for-tab-command])
  "Basic YAQi formatting menu.")

(easy-menu-define yaqi-menu yaqi-mode-map "YAQi Mode menu"
  (append
   '("YAQi" :help "YAQi-specific Features")
   yaqi-mode-basic-menu-items))

;; -----------------------------------------------------------------------------
;;;  YAQi mode

;;;###autoload
(define-derived-mode yaqi-mode fundamental-mode "YAQi"
  "Major mode for editing YAQi files.

Turns on Font Lock mode.

Supports Eldoc mode (only for functions, using a Python process),
Info-Look and Imenu.  In Outline minor mode, `define' lines count
as headers.

\\{yaqi-mode-map}"
  :group 'yaqi
  (set-syntax-table yaqi-mode-syntax-table)

  (set (make-local-variable 'comment-start) "##")

  ;; Look within the line for a ## following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)##+[ \t]*")

  (set (make-local-variable 'comment-column) 40)

  ;; Set the regexp used by outline-mode to find the headings
  (set (make-local-variable 'outline-regexp) "###[ ]+\\|(......")

  ;; Remove the number of "###" from the outline-level
  (set (make-local-variable 'outline-level) '(lambda () (- (outline-level) 3)))

  ;; Set the heading levels for promotion and demotion
  (setq outline-promotion-headings '("### " "###  " "###   "))

  ;; Remove the number of "###" from the outline-level
  (set (make-local-variable 'outline-level) '(lambda () (- (outline-level) 3)))

  ;; Set up font-locking
  (yaqi-font-lock)

  ;; Register YAQi-specific indentation function
  ;; commented out for Stefan (set (make-local-variable 'lisp-indent-offset) 2)
  (set (make-local-variable 'indent-line-function) 'yaqi-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'yaqi-indent-function)

  ;; The standard lisp comment indentation does not work well with YAQi
  (set (make-local-variable 'comment-indent-function) 'comment-indent-default)

  ;; Process the optional alist of function indentation specifications
  (when yaqi-indent-optional-function-alist
      (yaqi-put-indent-function-alist yaqi-indent-optional-function-alist))
  )

(provide 'yaqi-mode)

;; -----------------------------------------------------------------------------
;;; yaqi-mode.el ends here
