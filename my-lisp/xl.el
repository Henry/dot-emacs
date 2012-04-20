;;; xl.el -- Basic mode for XL
;;
;; Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.
;; Copyright (C) 2011 Henry G. Weller
;;
;; Author: Christophe de Dinechin (christophe@dinechin.org)
;; Maintainer: Henry G. Weller
;;
;; Based on: Python mode by Dave Love <fx@gnu.org>
;;
;; Created: July 2005
;; Version: 0.2
;; Last-Updated: Sat May 14 21:24:05 2011 (+0100)
;;           By: Henry G. Weller
;;     Update #: 9
;; URL:
;; Keywords: languages
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of GNU Emacs.
;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; Major mode for editing programs written in XL.
;;
;; Successive TABs cycle between possible indentations for the line.
;; There is symbol completion using lookup in XL.
;; -----------------------------------------------------------------------------
;;; Change log:
;;
;; Version 0.1
;; * Upgraded to Emacs 23.2.
;; * Changed so that it can be byte-compiled.
;; * Slightly reformatted and cleaned.
;; Version 0.2
;; * Corrected block and function searching code.
;; * Corrected support for outline-mode and added handling of `///' comments.
;; * Improved Imenu support.
;; * Completed list of types for font-locking.
;; -----------------------------------------------------------------------------
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;; -----------------------------------------------------------------------------
;;; Code:

(require 'outline)

(defgroup xl nil
  "Editing mode for XL programs"
  :group 'languages
  :version "0.2"
  :link '(emacs-commentary-link "xl"))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("xl" . xl-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xl\\'" . xl-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xs\\'" . xl-mode))

;;;###autoload
(add-to-list 'same-window-buffer-names "*XL*")

;; -----------------------------------------------------------------------------
;;; Customization variables

(defcustom xl-indent 4
  "*Number of columns for a unit of indentation in XL mode.
See also `\\[xl-guess-indent]'"
  :group 'xl
  :type 'integer)

(defcustom xl-guess-indent t
  "*Non-nil means XL mode guesses `xl-indent' for the buffer."
  :type 'boolean
  :group 'xl)

(defcustom xl-indent-string-contents t
  "*Non-nil means indent contents of multi-line strings together.
This means indent them the same as the preceding non-blank line.
Otherwise indent them to column zero."
  :type '(choice (const :tag "Align with preceding" t)
                 (const :tag "Indent to column 0" nil))
  :group 'xl)

(defcustom xl-honour-comment-indentation nil
  "Non-nil means indent relative to preceding comment line.
Only do this for comments where the leading comment character is followed
by space.  This doesn't apply to comment lines, which are always indented
in lines with preceding comments."
  :type 'boolean
  :group 'xl)

(defcustom xl-continuation-offset 4
  "*Number of columns of additional indentation for continuation lines.
Continuation lines follow a line terminated by an operator other than semicolon"
  :group 'xl
  :type 'integer)

;; -----------------------------------------------------------------------------
;;; Font lock

(eval-when-compile
  (defvar xl-keywords
    '("generic" "is" "import" "include" "using" "use"
      "in" "out" "variable" "constant" "var" "const"
      "return" "other" "require" "ensure" "written" "yield"
      "try" "catch" "retry" "raise" "transform" "into" "include"
      "while" "until" "for" "do" "loop" "exit" "restart"
      "translate" "when" "where" "with" "case"
      "if" "then" "else" "not" "and" "or" "xor" "nil")
    "List of words highlighted as 'keywords' in XL mode")

  (defvar xl-warnings
    '("raise")
    "List of words prefixing a 'warnings' in XL mode")

  (defvar xl-types
    '("integer" "natural" "real" "character" "text" "record" "boolean")
    "List of words highlighted as 'types' in XL mode")

  (defvar xl-functors
    '("function" "procedure" "to" "translation" "iterator")
    "List of words declaring functions in XL mode")

  (defvar xl-type-declarators
    '("type")
    "List of words declaring types in XL mode")

  (defvar xl-module-declarators
    '("module")
    "List of words declaring modules in XL mode")

  (defvar xl-declarators
    (append xl-functors xl-type-declarators xl-module-declarators)
    "List of words declaring modules or records in XL mode")

  (defvar xl-outdent-words
    '("else" "into")
    "List of words declaring modules or records in XL mode")

  (defvar xl-quotes
    '(("<<" ">>"))
    "List of character sequences used as quotes in XL mode")

  (defvar xl-comments
    '(("//")
      ("/*" "*/"))
    "List of character sequences used as comments in XL mode")

  (defun xl-separators-regexp (sep-list)
    "Return a regexp matching the separator list"
    (cons 'or (mapcar '(lambda(x)
                         (if (cadr x)
                             (list 'and
                                   (car x)
                                   '(0+ (or printing "\f" "\n"))
                                   (cadr x))
                           (list 'and
                                 (car x)
                                 '(0+ (or printing))
                                 '(or "\n" "\f"))))
                      sep-list))))

;; Strictly speaking, XL has no keywords, but colorizing special words
;; that have standard uses in normal XL programs still makes sense.
(defvar xl-font-lock-keywords
  (eval-when-compile
    (list
     ;; Strings
     `(,(rx (group (eval (xl-separators-regexp xl-quotes))))
       (1 font-lock-string-face))

     ;; Comments
     `(,(rx (group (eval (xl-separators-regexp xl-comments))))
       (1 font-lock-comment-face))

     ;; Pragmas
     `(,(rx (group (and "{" (0+ blank) (0+ word))))
       (1 font-lock-preprocessor-face))
     `(,(rx (group "}"))
       (1 font-lock-preprocessor-face))

     ;; Keywords
     `(,(rx (group (and word-start
                        (eval (cons 'or xl-keywords))
                        word-end)))
       (1 font-lock-keyword-face))

     ;; Warnings (raise X)
     `(,(rx (and word-start
                 (group (eval (cons 'or xl-warnings)))
                 (1+ space) (group (1+ (or (1+ word)
                                           (eval (xl-separators-regexp xl-quotes))
                                           (1+ (char digit ".eE_")))))))
       (1 font-lock-keyword-face) (2 font-lock-warning-face))

     ;; Constants
     `(,(rx (and word-start
                 (group (1+ (char digit "_"))
                        "#"
                        (1+ word)
                        (optional "." (0+ word))
                        (optional "#")
                        (optional (and (char "eE")
                                       (optional (char "+-"))
                                       (1+ (char digit "_")))))
                 word-end))
       (1 font-lock-constant-face))
     `(,(rx (and word-start
                 (group (1+ (char digit "_"))
                        (optional "." (0+ (char digit "_")))
                        (optional (and (char "eE")
                                       (optional (char "+-"))
                                       (1+ (char digit "_")))))
                 word-end))
       (1 font-lock-constant-face))

     ;; Predefined types
     `(,(rx (and word-start
                 (group (eval (cons 'or xl-types)))
                 word-end))
       (1 font-lock-type-face))

     ;; Executable types (procedures)
     `(,(rx (and word-start
                 (group (eval (cons 'or xl-functors)))
                 (1+ space) (group (and (1+ word)
                                        (0+ (and "." (1+ word)))))))
       (1 font-lock-keyword-face) (2 font-lock-function-name-face))

     ;; Type declarations
     `(,(rx (and word-start
                 (group (eval (cons 'or xl-type-declarators)))
                 (1+ space) (group (and (1+ word)
                                        (0+ (and "." (1+ word)))))))
       (1 font-lock-keyword-face) (2 font-lock-type-face))

     ;; Module declarations (we use the preprocessor face for these)
     `(,(rx (and word-start
                 (group (eval (cons 'or xl-module-declarators)))
                 (1+ space) (group (and (1+ word)
                                        (0+ (and "." (1+ word)))))))
       (1 font-lock-keyword-face) (2 font-lock-preprocessor-face))

     ;; Import directives
     `(,(rx (and word-start
                 (group "import")
                 (1+ space) (group (1+ word))
                 (0+ space) "=" (0+ space)
                 (group (and (1+ word))
                        (0+ (and "." (1+ word))))))
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face)
       (3 font-lock-preprocessor-face))
     `(,(rx (and word-start
                 (group "import")
                 (1+ space)
                 (group (and (1+ word))
                        (0+ (and "." (1+ word))))))
       (1 font-lock-keyword-face)
       (2 font-lock-preprocessor-face))

     ;; Declaration
     `(,(rx (and word-start
                 (group (1+ word))
                 (0+ space) ":" (0+ space)
                 (group (and (1+ word))
                        (0+ (and "." (1+ word))))))
       (1 font-lock-variable-name-face)
       (2 font-lock-type-face))

     ;; Assignment
     `(,(rx (and word-start
                 (group (1+ word))
                 (0+ space) (or ":=" "+=" "-=" "*=" "/=") (0+ space)))
       (1 font-lock-variable-name-face)))))


;; Recognize XL text separators
(defconst xl-font-lock-syntactic-keywords
  (eval-when-compile
    (list
     `(,(rx (and "[[" (0+ anything) "]]")))
     `(,(rx (and (group (syntax string-quote)) anything (backref 1)))))))

;; -----------------------------------------------------------------------------
;;; Keymap and syntax

(defvar xl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177" 'xl-backspace)
    (define-key map "\C-c0" 'xl-unindent-for-tab)
    (define-key map "\C-c<" 'xl-shift-left)
    (define-key map "\C-c>" 'xl-shift-right)

    (define-key map "\C-c1" 'xl-beginning-of-statement)
    (define-key map "\C-c3" 'xl-end-of-statement)
    (define-key map "\C-c4" 'xl-bwc1)
    (define-key map "\C-c7" 'xl-bwc2)
    (define-key map "\C-c6" 'xl-fwc1)
    (define-key map "\C-c9" 'xl-fwc2)

    (define-key map "\C-c\C-k" 'xl-mark-block)
    (define-key map "\C-c\t"   'xl-indent-region)
    (define-key map "\C-c\C-n" 'xl-next-statement)
    (define-key map "\C-c\C-p" 'xl-previous-statement)
    (define-key map "\C-c\C-u" 'xl-beginning-of-block)

    ;; Fixme: Add :help to menu.
    (easy-menu-define xl-menu map "XL Mode menu"
      '("XL"
        :help "XL-specific Features"
        ["Shift region left" xl-shift-left :active mark-active
         :help "Shift by a single indentation step"]
        ["Shift region right" xl-shift-right :active mark-active
         :help "Shift by a single indentation step"]
        "-"
        ["Mark block" xl-mark-block
         :help "Mark innermost block around point"]
        ["Mark procedure/type" mark-defun
         :help "Mark innermost definition around point"]
        "-"
        ["Start of block" xl-beginning-of-block
         :help "Go to start of innermost definition around point"]
        ["End of block" xl-end-of-block
         :help "Go to end of innermost definition around point"]
        ["Start of definition" beginning-of-defun
         :help "Go to start of innermost definition around point"]
        ["End of definition" end-of-defun
         :help "Go to end of innermost definition around point"]
        ["Previous statement" xl-previous-statement]
        ["Next statement" xl-next-statement]))
    map))

(defvar xl-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    (modify-syntax-entry ?:  "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?\|  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?\{ "." table)
    (modify-syntax-entry ?\} "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?\\ "." table)

    ;; a single slash is punctuation, but a double slash starts a comment
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    ;; and \f and \n end a comment
    (modify-syntax-entry ?\f  "> b" table)
    (modify-syntax-entry ?\n "> b"  table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b" table)

    ;; # is a paired delimiter in 16#FFFE#, but we treat it as symbol
    (modify-syntax-entry ?#  "." table)

    ;; define what belongs in Ada symbols
    (modify-syntax-entry ?_ "_" table)

    ;; define parentheses to match
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    table))

;; -----------------------------------------------------------------------------
;;; Support functions

(defsubst xl-in-string/comment ()
  "Return non-nil if point is in a XL literal (a comment or string)."
  (syntax-ppss-context (syntax-ppss)))

(defun xl-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Doesn't move out of comments -- should be outside or at end of line."
  (forward-comment (if backward most-negative-fixnum most-positive-fixnum)))

(defun xl-skip-comments/blanks2 (&optional backward)
  "Same as above"
  (let ((regexp (rx (1+ (or (eval (xl-separators-regexp xl-comments))
                            (1+ (char blank ?\r ?\f)))))))
    (if backward
        (re-search-backward regexp (point-min) t)
      (re-search-forward regexp (point-max) t))))

(defun xl-fwc1 () "" (interactive) (xl-skip-comments/blanks))
(defun xl-bwc1 () "" (interactive) (xl-skip-comments/blanks t))
(defun xl-fwc2 () "" (interactive) (xl-skip-comments/blanks2))
(defun xl-bwc2 () "" (interactive) (xl-skip-comments/blanks2 t))

(defun xl-continuation-line-p ()
  "Return non-nil if current line continues a previous one.
The criteria are that the previous line ends in a punctuation (except semicolon)
or that the bracket/paren nesting depth is nonzero."
  (or (and (save-excursion
             (xl-skip-comments/blanks t)
             (and (re-search-backward (rx graph) (point-min) t)
                  (looking-at (rx (syntax punctuation)))
                  (not (looking-at (rx (or ";" ")" "]" "}"))))))
           (not (syntax-ppss-context (syntax-ppss))))
      (/= 0 (syntax-ppss-depth
             (save-excursion      ; syntax-ppss with arg changes point
               (syntax-ppss (line-beginning-position)))))))

(defun xl-comment-line-p (&optional arg)
  "Return non-nil iff current line has only a comment.
With optional arg, return non-nil iff current line is empty or only a comment."
  (save-excursion
    (end-of-line)
    (when (or arg (eq 'comment (syntax-ppss-context (syntax-ppss))))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start)
                          (eval (xl-separators-regexp xl-comments))
                          line-end))))))

(defun xl-blank-line-p ()
  "Return non-nil if and only if current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun xl-beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun xl-open-block-statement-p ()
  "Return non-nil if statement at point opens a block.
In XL, a statement opens a block if next line is more indented"
  (let ((indent (current-indentation)))
    (if (eobp) nil
      (if (xl-outdent-p) t
        (save-excursion
          (beginning-of-line)
          (forward-line)
          (if (or (xl-comment-line-p) (xl-blank-line-p))
              (xl-skip-comments/blanks))
          (< indent (current-indentation)))))))

(defun xl-close-block-statement-p ()
  "Return non-nil if current line is a statement closing a block.
The criteria is that the previous line has a lower indent"
  (let ((indent (current-indentation)))
    (if (bobp) nil
      (save-excursion
        (forward-line -1)
        (while (and (not (bobp)) (xl-comment-line-p))
          (forward-line -1))
        (< indent (current-indentation))))))

(defun xl-outdent-p ()
  "Return non-nil if current line should outdent a level."
  (save-excursion
    (back-to-indentation)
    (looking-at (rx (eval (cons 'or xl-outdent-words))))))

;; -----------------------------------------------------------------------------
;;; Indentation.

(defun xl-guess-indent ()
  "Guess step for indentation of current buffer.
Set `xl-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done indent)
        (while (and (not done) (not (eobp)))
          (when (and (re-search-forward (rx (and ?: (0+ space)
                                                 (or (syntax comment-start)
                                                     line-end)))
                                        nil 'move)
                     (xl-open-block-statement-p))
            (save-excursion
              (xl-beginning-of-statement)
              (let ((initial (current-indentation)))
                (if (zerop (xl-next-statement))
                    (setq indent (- (current-indentation) initial)))
                (if (and (>= indent 2) (<= indent 8)) ; sanity check
                    (setq done t))))))
        (when done
          (when (/= indent (default-value 'xl-indent))
            (set (make-local-variable 'xl-indent) indent)
            (unless (= tab-width xl-indent)
              (setq indent-tabs-mode nil)))
          indent)))))

(defun xl-calculate-indentation ()
  "Calculate XL indentation for line at point."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss))
          start)
      (cond
       ((eq 'string (syntax-ppss-context syntax)) ; multi-line string
        (if (not xl-indent-string-contents)
            0
          (save-excursion
            ;; Find indentation of preceding non-blank line within string.
            (setq start (nth 8 syntax))
            (forward-line -1)
            (while (and (< start (point)) (looking-at "\\s-*$"))
              (forward-line -1))
            (current-indentation))))
       ((xl-continuation-line-p)
        (let ((point (point))
              (open-start (cadr syntax)))
          (if open-start
              ;; Inside bracketed expression.
              (progn
                (goto-char (1+ open-start))
                ;; Look for first item in list (preceding point) and
                ;; align with it, if found.
                (if (let ((parse-sexp-ignore-comments t))
                      (condition-case ()
                          (progn (forward-sexp)
                                 (backward-sexp)
                                 (< (point) point))
                        (error nil)))
                    (current-column)
                  ;; Otherwise indent relative to statement start, one
                  ;; level per bracketing level.
                  (goto-char (1+ open-start))
                  (xl-beginning-of-statement)
                  (+ (current-indentation) (* (car syntax) xl-indent))))
            ;; Otherwise backslash-continued.
            (forward-line -1)
            (if (xl-continuation-line-p)
                ;; We're past first continuation line.  Align with
                ;; previous line.
                (current-indentation)
              ;; First continuation line.  Indent one step, with an
              ;; extra one if statement opens a block.
              (save-excursion
                (xl-beginning-of-statement)
                (+ (current-indentation) xl-continuation-offset
                   (if (xl-open-block-statement-p)
                       xl-indent
                     0)))))))
       ((bobp)
        0)
       (t (let ((point (point)))
            (if xl-honour-comment-indentation
                ;; Back over whitespace, newlines, non-indentable comments.
                (catch 'done
                  (while t
                    (if (cond ((bobp))
                              ;; not at comment start
                              ((not (forward-comment -1))
                               (xl-beginning-of-statement)
                               t)
                              ;; trailing comment
                              ((/= (current-column) (current-indentation))
                               (xl-beginning-of-statement)
                               t)
                              ;; indentable comment like xl-mode.el
                              ((and (looking-at (rx (and (syntax comment-start)
                                                         (or space line-end))))
                                    (/= 0 (current-column)))))
                        (throw 'done t))))
              ;; Else back over all comments.
              (xl-skip-comments/blanks t)
              (xl-beginning-of-statement))
            ;; don't lose on bogus outdent
            (max 0 (+ (current-indentation)
                      (or (cond ((xl-open-block-statement-p)
                                 xl-indent)
                                ((xl-outdent-p)
                                 xl-indent))
                          (progn (goto-char point)
                                 (if (xl-outdent-p)
                                     (- xl-indent)))
                          0)))))))))

;; -----------------------------------------------------------------------------
;;; Cycling through the possible indentations with TAB
;; These don't need to be buffer-local since they're only relevant during a cycle.

(defvar xl-indent-list nil
  "Alist of possible indentations and start of statement they would close
Internal use.")

(defvar xl-indent-list-length nil
  "Length of the above.
Internal use.")

(defvar xl-indent-index nil
  "Current index into the alist.
Internal use.")

(defun xl-initial-text ()
  "Text of line following indentation and ignoring any trailing comment."
  (buffer-substring (+ (line-beginning-position) (current-indentation))
                    (save-excursion
                      (end-of-line)
                      (forward-comment -1)
                      (point))))

(defun xl-indentation-levels ()
  "Return a list of possible indentations for this line.
Includes the default indentation, one extra indent,
and those which would close all enclosing blocks.
Assumes the line has already been indented per
`xl-indent-line'.  Elements of the list are actually pairs:
\(INDENTATION . TEXT), where TEXT is the initial text of the
corresponding block opening (or nil)."
  (let ((levels (list (cons (current-indentation) nil)))
        (unindents '()))
    ;; Only one possibility if we are in a continuation line.
    (unless (xl-continuation-line-p)
      (progn
        (save-excursion
          (while (xl-beginning-of-block)
            (push (cons (current-indentation) (xl-initial-text)) unindents)))
        (setq levels (append (reverse unindents) levels unindents levels))
        (push (cons (+ (current-indentation) xl-indent) t)
              levels)))
    levels))

(defun xl-indent-line-1 (&optional leave)
  "Subroutine of `xl-indent-line'.
Does non-repeated indentation.  LEAVE non-nil means leave
indentation if it is valid, i.e. one of the positions returned by
`xl-calculate-indentation'.
This is basically what `xl-indent-line' would be if we didn't do the cycling."
  (let ((target (xl-calculate-indentation))
        (pos (- (point-max) (point))))
    (if (or (= target (current-indentation))
            ;; Maybe keep a valid indentation.
            (and leave xl-indent-list
                 (assq (current-indentation) xl-indent-list)))
        (if (< (current-column) (current-indentation))
            (back-to-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target)
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun xl-unindent-for-tab ()
  "Indent for tab with a -1 argument"
  (interactive)
  (indent-for-tab-command))

(defun xl-indent-line ()
  "Indent current line as XL code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  The cycle is broken by a command different
from `indent-for-tab-command', i.e. successive TABs do the cycling."
  (interactive)
  ;; Don't do extra work if invoked via `indent-region', for instance.
  (if (not (or (eq this-command 'indent-for-tab-command)
               (eq this-command 'xl-unindent-for-tab)))
      (xl-indent-line-1)
    (if (or (eq last-command 'indent-for-tab-command)
            (eq last-command 'xl-unindent-for-tab))
        (if (= 1 xl-indent-list-length)
            (message "Sole indentation")
          (progn (setq xl-indent-index
                       (mod
                        (if (eq this-command 'xl-unindent-for-tab)
                            (1- xl-indent-index)
                          (1+ xl-indent-index))
                        xl-indent-list-length))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to (car (nth xl-indent-index xl-indent-list)))
                 (let ((text (cdr (nth xl-indent-index
                                       xl-indent-list))))

                   (if text
                       (cond
                        ((eq text t) (message "Indenting"))
                        ((stringp text) (message "Closes: %s" text)))
                     (message "Current indentation")))))
      (xl-indent-line-1)
      (setq xl-indent-list (xl-indentation-levels)
            xl-indent-list-length (length xl-indent-list)
            xl-indent-index (1- xl-indent-list-length)))))

(defun xl-indent-region (start end)
  "`indent-region-function' for Xl.
Leaves validly-indented lines alone, i.e. doesn't indent to
another valid position."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (xl-indent-line-1 t))
      (forward-line 1))
    (move-marker end nil)))

;; -----------------------------------------------------------------------------
;;; Movement.

(defun xl-beginning-of-defun ()
  "`beginning-of-defun-function' for XL.
Finds beginning of innermost nested type or function definition.
Returns the name of the definition found at the end, or nil if reached
start of buffer."
  (let ((ci (current-indentation))
        (def-re (rx (and line-start (0+ space) (eval (cons 'or xl-declarators))
                         (1+ space)
                         (group (1+ (or word (syntax symbol)))))))
        found lep def-line)
    (if (xl-comment-line-p)
        (setq ci most-positive-fixnum))
    (while (and (not (bobp)) (not found))
      ;; Treat bol at beginning of function as outside function so
      ;; that successive C-M-a makes progress backwards.
      (setq def-line (looking-at def-re))
      (unless (bolp) (end-of-line))
      (setq lep (line-end-position))
      (if (and (re-search-backward def-re nil 'move)
               ;; Must be less indented or matching top level, or
               ;; equally indented if we started on a definition line.
               (let ((in (current-indentation)))
                 (or (and (zerop ci) (zerop in))
                     (= lep (line-end-position)) ; on initial line
                     (and def-line (= in ci))
                     (< in ci)))
               (not (xl-in-string/comment)))
          (setq found t)))
    (back-to-indentation)))

(defun xl-beginning-of-statement ()
  "Go to start of current statement.
Accounts for continuation lines and multi-line strings."
  (interactive)
  (beginning-of-line)
  (xl-beginning-of-string)
  (while (xl-continuation-line-p)
    (beginning-of-line)
    (forward-line -1)))

(defun xl-end-of-statement ()
  "Go to the end of the current statement and return point.
Usually this is the beginning of the next line, but if there is a continuation,
we need to skip additional lines."
  (interactive)
  (end-of-line)
  (xl-skip-comments/blanks)
  (while (xl-continuation-line-p)
    (end-of-line)
    (xl-skip-comments/blanks))
  (point))

(defun xl-previous-statement (&optional count)
  "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (xl-next-statement (- count))
    (xl-beginning-of-statement)
    (while (and (> count 0) (not (bobp)))
      (xl-skip-comments/blanks t)
      (xl-beginning-of-statement)
      (unless (bobp) (setq count (1- count))))
    (back-to-indentation)
    count))

(defun xl-next-statement (&optional count)
  "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (xl-previous-statement (- count))
    (beginning-of-line)
    (while (and (> count 0) (not (eobp)))
      (xl-end-of-statement)
      (xl-skip-comments/blanks)
      (setq count (1- count)))
    count))

(defun xl-beginning-of-block (&optional arg)
  "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`xl-end-of-block' instead.
If point is on the first line of a block, use its outer block.
If current statement is in column zero, don't move and return nil.
Otherwise return non-nil."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((zerop arg))
   ((< arg 0) (xl-end-of-block (- arg)))
   (t
    (let ((point (point)))
      (if (or (xl-comment-line-p) (xl-blank-line-p))
          (xl-skip-comments/blanks t))
      (xl-beginning-of-statement)
      (let ((ci (current-indentation)))
        (if (zerop ci)
            (not (goto-char point))     ; return nil
          ;; Look upwards for less indented statement.
          (if (catch 'done
                (while (zerop (forward-line -1))
                  (when (and (< (current-indentation) ci)
                             (not (xl-comment-line-p t))
                             ;; Move to beginning to save effort in case
                             ;; this is in string.
                             (progn (xl-beginning-of-statement) t)
                             (xl-open-block-statement-p))
                    (beginning-of-line)
                    (throw 'done t)))
                (not (goto-char point))) ; Failed -- return nil
              (xl-beginning-of-block (1- arg)))))))))

(defun xl-end-of-block (&optional arg)
  "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`xl-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block, don't
move and return nil.  Otherwise return t."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (xl-beginning-of-block (- arg)))
  (while (and (> arg 0)
              (let* ((point (point))
                     (_ (if (or (xl-comment-line-p) (xl-blank-line-p))
                            (xl-skip-comments/blanks t)))
                     (ci (current-indentation))
                     (open (xl-open-block-statement-p)))
                (if (and (zerop ci) (not open))
                    (not (goto-char point))
                  (catch 'done
                    (while (zerop (xl-next-statement))
                      (when (or (and open (<= (current-indentation) ci))
                                (< (current-indentation) ci))
                        (xl-skip-comments/blanks t)
                        (beginning-of-line 2)
                        (throw 'done t)))))))
    (setq arg (1- arg)))
  (zerop arg))

;; -----------------------------------------------------------------------------
;;; Imenu

(defvar xl-imenu-generic-expression
  (list
   (list nil
         (purecopy
          (eval-when-compile
            (rx (and line-start (0+ blank)
                     (group (eval (cons 'or xl-functors)))
                     (1+ (or blank "\n"))
                     (group (1+ (or word (syntax symbol))))))))
         2)

   (list (purecopy "Types")
         (purecopy
          (eval-when-compile
            (rx (and line-start (0+ blank)
                     (group (eval (cons 'or xl-type-declarators)))
                     (1+ (or blank "\n"))
                     (group (1+ (or word (syntax symbol))))))))
         2)

   (list (purecopy "Modules")
         (purecopy
          (eval-when-compile
            (rx (and line-start (0+ blank)
                     (group (eval (cons 'or xl-module-declarators)))
                     (1+ (or blank "\n"))
                     (group (1+ (or word (syntax symbol))))))))
         2))
  "Imenu expression for XL-mode.  See `imenu-generic-expression'.")

;; -----------------------------------------------------------------------------
;;; `Electric' commands

(defun xl-backspace (arg)
  "Maybe delete a level of indentation on the current line.
If not at the end of line's indentation, or on a comment line, just call
`backward-delete-char-untabify'.  With ARG, repeat that many times."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (xl-continuation-line-p))
      (backward-delete-char-untabify arg)
    (let ((indent 0))
      (save-excursion
        (while (and (> arg 0) (xl-beginning-of-block))
          (setq arg (1- arg)))
        (when (zerop arg)
          (setq indent (current-indentation))
          (message "Closes %s" (xl-initial-text))))
      (delete-horizontal-space)
      (indent-to indent))))
(put 'xl-backspace 'delete-selection 'supersede)

;; -----------------------------------------------------------------------------
;;; Info-look functionality

(declare-function info-lookup-maybe-add-help "info-look" (&rest arg))

(defun xl-after-info-look ()
  "Set up info-look for XL.
Used with `eval-after-load'."
  (let* ((version "dunno")
         ;; Whether info files have a XL version suffix, e.g. in Debian.
         (versioned
          (with-temp-buffer
            (with-no-warnings (Info-mode))
            (condition-case ()
                ;; Don't use `info' because it would pop-up a *info* buffer.
                (with-no-warnings
                  (Info-goto-node (format "(xl%s-lib)Miscellaneous Index"
                                          version))
                  t)
              (error nil)))))
    (info-lookup-maybe-add-help
     :mode 'xl-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
         ;; The empty prefix just gets us highlighted terms.
         `((,(concat "(xl" version "-ref)Miscellaneous Index") nil "")
           (,(concat "(xl" version "-ref)Module Index" nil ""))
           (,(concat "(xl" version "-ref)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(xl" version "-ref)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(xl" version "-lib)Module Index" nil ""))
           (,(concat "(xl" version "-lib)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(xl" version "-lib)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(xl" version "-lib)Miscellaneous Index" nil "")))
       '(("(xl-ref)Miscellaneous Index" nil "")
         ("(xl-ref)Module Index" nil "")
         ("(xl-ref)Function-Method-Variable Index" nil "")
         ("(xl-ref)Class-Exception-Object Index" nil "")
         ("(xl-lib)Module Index" nil "")
         ("(xl-lib)Class-Exception-Object Index" nil "")
         ("(xl-lib)Function-Method-Variable Index" nil "")
         ("(xl-lib)Miscellaneous Index" nil ""))))))

(eval-after-load "info-look" '(xl-after-info-look))

;; -----------------------------------------------------------------------------
;;; Miscellancy

(defun xl-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling comments and multi-line strings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's
indentation and initial comment characters.  Similarly if the end
of the current line is in or at the end of a multi-line string.
Otherwise, do nothing."
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; The `paragraph-start' and `paragraph-separate' variables
      ;; don't allow us to delimit the last paragraph in a multi-line
      ;; string properly, so narrow to the string and then fill around
      ;; (the end of) the current line.
      (save-excursion
        (end-of-line)
        (let* ((syntax (syntax-ppss))
               (orig (point))
               (start (nth 8 syntax))
               end)
          (cond ((eq t (nth 3 syntax))      ; in fenced string
                 (goto-char (nth 8 syntax)) ; string start
                 (condition-case ()         ; for unbalanced quotes
                     (progn (forward-sexp)
                            (setq end (point)))
                   (error (setq end (point-max)))))
                ((re-search-backward "\\s|\\s-*\\=" nil t) ; end of fenced
                                        ; string
                 (forward-char)
                 (setq end (point))
                 (condition-case ()
                     (progn (backward-sexp)
                            (setq start (point)))
                   (error nil))))
          (when end
            (save-restriction
              (narrow-to-region start end)
              (goto-char orig)
              (fill-paragraph justify))))))
  t)

(defun xl-shift-left (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the left.
COUNT defaults to `xl-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie.  It is an error if any lines in the region are indented less than
COUNT columns."
  (interactive (if (/= (mark) (point))
                   (list (region-beginning) (region-end) current-prefix-arg)
                 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count xl-indent))
  (when (> count 0)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (if (and (< (current-indentation) count)
                 (not (looking-at "[ \t]*$")))
            (error "Can't shift all lines enough"))
        (forward-line))
      (indent-rigidly start end (- count)))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun xl-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `xl-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive (if mark-active
                   (list (region-beginning) (region-end) current-prefix-arg)
                 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count xl-indent))
  (indent-rigidly start end count))

(defun xl-outline-level ()
  "`outline-level' function for XL mode.

For heading starting with /// the outline level is the number of proceeding
spaces.

For heading starting with `xl-indent' steps of indentation followed by ///
the outline level is the number of `xl-indent' steps.

For heading starting with `xl-indent' steps of indentation followed by a
defining keyword the level is the number of `xl-indent' steps of indentation - 1."
  (let (buffer-invisibility-spec)
    (if (string-match-p "^///" (match-string 0))
        (- (outline-level) 3)
      (if (string-match-p "^\\s-*///" (match-string 0))
          (/ (current-indentation) xl-indent)
        (1+ (/ (current-indentation) xl-indent))))))

(defun xl-mark-block ()
  "Mark the block around point.
Uses `xl-beginning-of-block', `xl-end-of-block'."
  (interactive)
  (push-mark)
  (xl-beginning-of-block)
  (push-mark (point) nil t)
  (xl-end-of-block)
  (exchange-point-and-mark))

;; -----------------------------------------------------------------------------
;;; Mode

;;;###autoload
(define-derived-mode xl-mode fundamental-mode "XL"
  "Major mode for editing XL files.

The Emacs commands which work with `defun's,
e.g. \\[beginning-of-defun], deal with nested `function' and
`type' blocks.  They take the innermost one as current without
distinguishing method and type definitions.  Used multiple times,
they move over others at the same indentation level until they
reach the end of definitions at that level, when they move up a
level.

\\<xl-mode-map>

\\[xl-backspace] at the beginning of an indented statement
deletes a level of indentation to close the current block;
otherwise it deletes a charcter backward.  TAB indents the
current line relative to the preceding code.  Successive TABs,
with no intervening command, cycle through the possibilities for
indentation on the basis of enclosing blocks.

\\[fill-paragraph] fills comments and multiline strings appropriately, but has no
effect outside them.

Supports Info-Look and Imenu.

In Outline minor mode, `module', `procedure', `function', `to' and `type'
lines count as headers in addition to lines starting with `///'.

\\{xl-mode-map}"
  :group 'xl

  ;; Set up font-locking
  (set (make-local-variable 'font-lock-defaults)
       '(xl-font-lock-keywords nil nil nil nil ;; ((?_ . "w")) nil
                               (font-lock-syntactic-keywords
                                . xl-font-lock-syntactic-keywords)))

  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")

  ;; Indentation and filling
  (set (make-local-variable 'indent-line-function) #'xl-indent-line)
  (set (make-local-variable 'indent-region-function) #'xl-indent-region)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'xl-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)

  ;; Outline mode
  (set (make-local-variable 'outline-regexp)
       (concat "///[ ]+\\|\\s-*///[ ]+\\|\\s-*" (rx (eval (cons 'or xl-declarators))) "\\>"))
  (set (make-local-variable 'outline-level) #'xl-outline-level)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)

  ;; Function searching
  (set (make-local-variable 'beginning-of-defun-function)
       'xl-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'xl-end-of-block)

  ;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression xl-imenu-generic-expression)

  ;; Initialise indentation
  (when xl-guess-indent (xl-guess-indent)))

;; Add custom options
(custom-add-option 'xl-mode-hook 'imenu-add-menubar-index)
(custom-add-option 'xl-mode-hook
                   '(lambda ()
                      "Turn on Indent Tabs mode."
                      (set (make-local-variable 'indent-tabs-mode) t)))

(provide 'xl)

;; -----------------------------------------------------------------------------
;;; xl.el ends here
