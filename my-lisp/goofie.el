;;; goofie.el -- Basic mode for GOOFIE
;;
;; Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.
;; Copyright (C) 2011 Henry G. Weller
;;
;; Maintainer: Henry G. Weller
;;
;; Based on: XL mode by Christophe de Dinechin (christophe@dinechin.org)
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
;; Major mode for editing programs written in GOOFIE.
;;
;; Successive TABs cycle between possible indentations for the line.
;; There is symbol completion using lookup in GOOFIE.
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

(defgroup goofie nil
  "Editing mode for GOOFIE programs"
  :group 'languages
  :version "0.2"
  :link '(emacs-commentary-link "goofie"))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("g" . goofie-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\'" . goofie-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gi\\'" . goofie-mode))

;;;###autoload
(add-to-list 'same-window-buffer-names "*GOOFIE*")

;; -----------------------------------------------------------------------------
;;; Customization variables

(defcustom goofie-indent 3
  "*Number of columns for a unit of indentation in GOOFIE mode.
See also `\\[goofie-guess-indent]'"
  :group 'goofie
  :type 'integer)

(defcustom goofie-guess-indent t
  "*Non-nil means GOOFIE mode guesses `goofie-indent' for the buffer."
  :type 'boolean
  :group 'goofie)

(defcustom goofie-indent-string-contents t
  "*Non-nil means indent contents of multi-line strings together.
This means indent them the same as the preceding non-blank line.
Otherwise indent them to column zero."
  :type '(choice (const :tag "Align with preceding" t)
                 (const :tag "Indent to column 0" nil))
  :group 'goofie)

(defcustom goofie-honour-comment-indentation nil
  "Non-nil means indent relative to preceding comment line.
Only do this for comments where the leading comment character is followed
by space.  This doesn't apply to comment lines, which are always indented
in lines with preceding comments."
  :type 'boolean
  :group 'goofie)

(defcustom goofie-continuation-offset 6
  "*Number of columns of additional indentation for continuation lines.
Continuation lines follow a line terminated by an operator other than semicolon"
  :group 'goofie
  :type 'integer)

(defcustom goofie-list-offset 3
  "*Number of columns of additional indentation for list continuation lines.
Continuation lines follow a line terminated by a '('"
  :group 'goofie
  :type 'integer)

;; -----------------------------------------------------------------------------
;;; Font lock

(eval-when-compile
  (defvar goofie-keywords
    '("generic" "is" "import" "include" "using" "use"
      "in" "out" "variable" "constant" "var" "const"
      "return" "other" "require" "ensure" "written" "yield"
      "try" "catch" "retry" "raise" "transform" "into" "include"
      "while" "until" "for" "do" "loop" "exit" "restart"
      "translate" "when" "where" "with" "case"
      "if" "then" "else" "not" "and" "or" "xor" "nil")
    "List of words highlighted as 'keywords' in GOOFIE mode")

  (defvar goofie-warnings
    '("raise")
    "List of words prefixing a 'warnings' in GOOFIE mode")

  (defvar goofie-types
    '("integer" "natural" "real" "character" "text" "record" "boolean")
    "List of words highlighted as 'types' in GOOFIE mode")

  (defvar goofie-functors
    '("function" "procedure" "to" "translation" "iterator")
    "List of words declaring functions in GOOFIE mode")

  (defvar goofie-type-declarators
    '("type")
    "List of words declaring types in GOOFIE mode")

  (defvar goofie-module-declarators
    '("module")
    "List of words declaring modules in GOOFIE mode")

  (defvar goofie-declarators
    (append goofie-functors goofie-type-declarators goofie-module-declarators)
    "List of words declaring modules or records in GOOFIE mode")

  (defvar goofie-outdent-words
    '("else" "into")
    "List of words declaring modules or records in GOOFIE mode")

  (defvar goofie-quotes
    '(("<<" ">>"))
    "List of character sequences used as quotes in GOOFIE mode")

  (defvar goofie-comments
    '(("//")
      ("/*" "*/"))
    "List of character sequences used as comments in GOOFIE mode")

  (defun goofie-separators-regexp (sep-list)
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

;; Strictly speaking, GOOFIE has no keywords, but colorizing special words
;; that have standard uses in normal GOOFIE programs still makes sense.
(defvar goofie-font-lock-keywords
  (eval-when-compile
    (list
     ;; Strings
     `(,(rx (group (eval (goofie-separators-regexp goofie-quotes))))
       (1 font-lock-string-face))

     ;; Comments
     `(,(rx (group (eval (goofie-separators-regexp goofie-comments))))
       (1 font-lock-comment-face))

     ;; Pragmas
     `(,(rx (group (and "{" (0+ blank) (0+ word))))
       (1 font-lock-preprocessor-face))
     `(,(rx (group "}"))
       (1 font-lock-preprocessor-face))

     ;; Keywords
     `(,(rx (group (and word-start
                        (eval (cons 'or goofie-keywords))
                        word-end)))
       (1 font-lock-keyword-face))

     ;; Warnings (raise X)
     `(,(rx (and word-start
                 (group (eval (cons 'or goofie-warnings)))
                 (1+ space) (group (1+ (or (1+ word)
                                           (eval (goofie-separators-regexp goofie-quotes))
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
                 (group (eval (cons 'or goofie-types)))
                 word-end))
       (1 font-lock-type-face))

     ;; Executable types (procedures)
     `(,(rx (and word-start
                 (group (eval (cons 'or goofie-functors)))
                 (1+ space) (group (and (1+ word)
                                        (0+ (and "." (1+ word)))))))
       (1 font-lock-keyword-face) (2 font-lock-function-name-face))

     ;; Type declarations
     `(,(rx (and word-start
                 (group (eval (cons 'or goofie-type-declarators)))
                 (1+ space) (group (and (1+ word)
                                        (0+ (and "." (1+ word)))))))
       (1 font-lock-keyword-face) (2 font-lock-type-face))

     ;; Module declarations (we use the preprocessor face for these)
     `(,(rx (and word-start
                 (group (eval (cons 'or goofie-module-declarators)))
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


;; Recognize GOOFIE text separators
(defconst goofie-font-lock-syntactic-keywords
  (eval-when-compile
    (list
     `(,(rx (and "[[" (0+ anything) "]]")))
     `(,(rx (and (group (syntax string-quote)) anything (backref 1)))))))

;; -----------------------------------------------------------------------------
;;; Keymap and syntax

(defvar goofie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177" 'goofie-backspace)
    (define-key map "\C-c0" 'goofie-unindent-for-tab)
    (define-key map "\C-c<" 'goofie-shift-left)
    (define-key map "\C-c>" 'goofie-shift-right)

    (define-key map "\C-c1" 'goofie-beginning-of-statement)
    (define-key map "\C-c3" 'goofie-end-of-statement)
    (define-key map "\C-c4" 'goofie-bwc1)
    (define-key map "\C-c7" 'goofie-bwc2)
    (define-key map "\C-c6" 'goofie-fwc1)
    (define-key map "\C-c9" 'goofie-fwc2)

    (define-key map "\C-c\C-k" 'goofie-mark-block)
    (define-key map "\C-c\t"   'goofie-indent-region)
    (define-key map "\C-c\C-n" 'goofie-next-statement)
    (define-key map "\C-c\C-p" 'goofie-previous-statement)
    (define-key map "\C-c\C-u" 'goofie-beginning-of-block)
    (define-key map "\C-c\C-c" 'comment-region)

    ;; Fixme: Add :help to menu.
    (easy-menu-define goofie-menu map "GOOFIE Mode menu"
      '("GOOFIE"
        :help "GOOFIE-specific Features"
        ["Shift region left" goofie-shift-left :active mark-active
         :help "Shift by a single indentation step"]
        ["Shift region right" goofie-shift-right :active mark-active
         :help "Shift by a single indentation step"]
        "-"
        ["Mark block" goofie-mark-block
         :help "Mark innermost block around point"]
        ["Mark procedure/type" mark-defun
         :help "Mark innermost definition around point"]
        "-"
        ["Start of block" goofie-beginning-of-block
         :help "Go to start of innermost definition around point"]
        ["End of block" goofie-end-of-block
         :help "Go to end of innermost definition around point"]
        ["Start of definition" beginning-of-defun
         :help "Go to start of innermost definition around point"]
        ["End of definition" end-of-defun
         :help "Go to end of innermost definition around point"]
        ["Previous statement" goofie-previous-statement]
        ["Next statement" goofie-next-statement]))
    map))

(defvar goofie-mode-syntax-table
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

(defsubst goofie-in-string/comment ()
  "Return non-nil if point is in a GOOFIE literal (a comment or string)."
  (syntax-ppss-context (syntax-ppss)))

(defun goofie-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Doesn't move out of comments -- should be outside or at end of line."
  (forward-comment (if backward most-negative-fixnum most-positive-fixnum)))

(defun goofie-skip-comments/blanks2 (&optional backward)
  "Same as above"
  (let ((regexp (rx (1+ (or (eval (goofie-separators-regexp goofie-comments))
                            (1+ (char blank ?\r ?\f)))))))
    (if backward
        (re-search-backward regexp (point-min) t)
      (re-search-forward regexp (point-max) t))))

(defun goofie-fwc1 () "" (interactive) (goofie-skip-comments/blanks))
(defun goofie-bwc1 () "" (interactive) (goofie-skip-comments/blanks t))
(defun goofie-fwc2 () "" (interactive) (goofie-skip-comments/blanks2))
(defun goofie-bwc2 () "" (interactive) (goofie-skip-comments/blanks2 t))

(defun goofie-continuation-line-p ()
  "Return non-nil if current line continues a previous one.
The criteria are that the previous line ends in a punctuation (except semicolon)
or that the bracket/paren nesting depth is nonzero."
  (or (and (save-excursion
             (goofie-skip-comments/blanks t)
             (and (re-search-backward (rx graph) (point-min) t)
                  (looking-at (rx (syntax punctuation)))
                  (not (looking-at (rx (or ";" ")" "]" "}"))))))
           (not (syntax-ppss-context (syntax-ppss))))
      (/= 0 (syntax-ppss-depth
             (save-excursion      ; syntax-ppss with arg changes point
               (syntax-ppss (line-beginning-position)))))))

(defun goofie-comment-line-p (&optional arg)
  "Return non-nil iff current line has only a comment.
With optional arg, return non-nil iff current line is empty or only a comment."
  (save-excursion
    (end-of-line)
    (when (or arg (eq 'comment (syntax-ppss-context (syntax-ppss))))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start)
                          (eval (goofie-separators-regexp goofie-comments))
                          line-end))))))

(defun goofie-blank-line-p ()
  "Return non-nil if and only if current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun goofie-beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun goofie-open-block-statement-p ()
  "Return non-nil if statement at point opens a block.
In GOOFIE, a statement opens a block if next line is more indented"
  (let ((indent (current-indentation)))
    (if (eobp) nil
      (if (goofie-outdent-p) t
        (save-excursion
          (beginning-of-line)
          (forward-line)
          (if (or (goofie-comment-line-p) (goofie-blank-line-p))
              (goofie-skip-comments/blanks))
          (< indent (current-indentation)))))))

(defun goofie-close-block-statement-p ()
  "Return non-nil if current line is a statement closing a block.
The criteria is that the previous line has a lower indent"
  (let ((indent (current-indentation)))
    (if (bobp) nil
      (save-excursion
        (forward-line -1)
        (while (and (not (bobp)) (goofie-comment-line-p))
          (forward-line -1))
        (< indent (current-indentation))))))

(defun goofie-outdent-p ()
  "Return non-nil if current line should outdent a level."
  (save-excursion
    (back-to-indentation)
    (looking-at (rx (eval (cons 'or goofie-outdent-words))))))

;; -----------------------------------------------------------------------------
;;; Indentation.

(defun goofie-guess-indent ()
  "Guess step for indentation of current buffer.
Set `goofie-indent' locally to the value guessed."
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
                     (goofie-open-block-statement-p))
            (save-excursion
              (goofie-beginning-of-statement)
              (let ((initial (current-indentation)))
                (if (zerop (goofie-next-statement))
                    (setq indent (- (current-indentation) initial)))
                (if (and (>= indent 2) (<= indent 8)) ; sanity check
                    (setq done t))))))
        (when done
          (when (/= indent (default-value 'goofie-indent))
            (set (make-local-variable 'goofie-indent) indent)
            (unless (= tab-width goofie-indent)
              (setq indent-tabs-mode nil)))
          indent)))))

(defun goofie-calculate-indentation ()
  "Calculate GOOFIE indentation for line at point."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss))
          start)
      (cond
       ((eq 'string (syntax-ppss-context syntax)) ; multi-line string
        (if (not goofie-indent-string-contents)
            0
          (save-excursion
            ;; Find indentation of preceding non-blank line within string.
            (setq start (nth 8 syntax))
            (forward-line -1)
            (while (and (< start (point)) (looking-at "\\s-*$"))
              (forward-line -1))
            (current-indentation))))
       ((goofie-continuation-line-p)
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
                  (goofie-beginning-of-statement)
                  (+ (current-indentation)
                     (* (car syntax) goofie-indent)
                     goofie-list-offset)))
            ;; Otherwise backslash-continued.
            (forward-line -1)
            (if (goofie-continuation-line-p)
                ;; We're past first continuation line.  Align with
                ;; previous line.
                (current-indentation)
              ;; First continuation line.  Indent one step, with an
              ;; extra one if statement opens a block.
              (save-excursion
                (goofie-beginning-of-statement)
                (+ (current-indentation) goofie-continuation-offset
                   (if (goofie-open-block-statement-p)
                       goofie-indent
                     0)))))))
       ((bobp)
        0)
       (t (let ((point (point)))
            (if goofie-honour-comment-indentation
                ;; Back over whitespace, newlines, non-indentable comments.
                (catch 'done
                  (while t
                    (if (cond ((bobp))
                              ;; not at comment start
                              ((not (forward-comment -1))
                               (goofie-beginning-of-statement)
                               t)
                              ;; trailing comment
                              ((/= (current-column) (current-indentation))
                               (goofie-beginning-of-statement)
                               t)
                              ;; indentable comment like goofie-mode.el
                              ((and (looking-at (rx (and (syntax comment-start)
                                                         (or space line-end))))
                                    (/= 0 (current-column)))))
                        (throw 'done t))))
              ;; Else back over all comments.
              (goofie-skip-comments/blanks t)
              (goofie-beginning-of-statement))
            ;; don't lose on bogus outdent
            (max 0 (+ (current-indentation)
                      (or (cond ((goofie-open-block-statement-p)
                                 goofie-indent)
                                ((goofie-outdent-p)
                                 goofie-indent))
                          (progn (goto-char point)
                                 (if (goofie-outdent-p)
                                     (- goofie-indent)))
                          0)))))))))

;; -----------------------------------------------------------------------------
;;; Cycling through the possible indentations with TAB
;; These don't need to be buffer-local since they're only relevant during a cycle.

(defvar goofie-indent-list nil
  "Alist of possible indentations and start of statement they would close
Internal use.")

(defvar goofie-indent-list-length nil
  "Length of the above.
Internal use.")

(defvar goofie-indent-index nil
  "Current index into the alist.
Internal use.")

(defun goofie-initial-text ()
  "Text of line following indentation and ignoring any trailing comment."
  (buffer-substring (+ (line-beginning-position) (current-indentation))
                    (save-excursion
                      (end-of-line)
                      (forward-comment -1)
                      (point))))

(defun goofie-indentation-levels ()
  "Return a list of possible indentations for this line.
Includes the default indentation, one extra indent,
and those which would close all enclosing blocks.
Assumes the line has already been indented per
`goofie-indent-line'.  Elements of the list are actually pairs:
\(INDENTATION . TEXT), where TEXT is the initial text of the
corresponding block opening (or nil)."
  (let ((levels (list (cons (current-indentation) nil)))
        (unindents '()))
    ;; Only one possibility if we are in a continuation line.
    (unless (goofie-continuation-line-p)
      (progn
        (save-excursion
          (while (goofie-beginning-of-block)
            (push (cons (current-indentation) (goofie-initial-text)) unindents)))
        (setq levels (append (reverse unindents) levels unindents levels))
        (push (cons (+ (current-indentation) goofie-indent) t)
              levels)))
    levels))

(defun goofie-indent-line-1 (&optional leave)
  "Subroutine of `goofie-indent-line'.
Does non-repeated indentation.  LEAVE non-nil means leave
indentation if it is valid, i.e. one of the positions returned by
`goofie-calculate-indentation'.
This is basically what `goofie-indent-line' would be if we didn't do the cycling."
  (let ((target (goofie-calculate-indentation))
        (pos (- (point-max) (point))))
    (if (or (= target (current-indentation))
            ;; Maybe keep a valid indentation.
            (and leave goofie-indent-list
                 (assq (current-indentation) goofie-indent-list)))
        (if (< (current-column) (current-indentation))
            (back-to-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target)
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun goofie-unindent-for-tab ()
  "Indent for tab with a -1 argument"
  (interactive)
  (indent-for-tab-command))

(defun goofie-indent-line ()
  "Indent current line as GOOFIE code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  The cycle is broken by a command different
from `indent-for-tab-command', i.e. successive TABs do the cycling."
  (interactive)
  ;; Don't do extra work if invoked via `indent-region', for instance.
  (if (not (or (eq this-command 'indent-for-tab-command)
               (eq this-command 'goofie-unindent-for-tab)))
      (goofie-indent-line-1)
    (if (or (eq last-command 'indent-for-tab-command)
            (eq last-command 'goofie-unindent-for-tab))
        (if (= 1 goofie-indent-list-length)
            (message "Sole indentation")
          (progn (setq goofie-indent-index
                       (mod
                        (if (eq this-command 'goofie-unindent-for-tab)
                            (1- goofie-indent-index)
                          (1+ goofie-indent-index))
                        goofie-indent-list-length))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to (car (nth goofie-indent-index goofie-indent-list)))
                 (let ((text (cdr (nth goofie-indent-index
                                       goofie-indent-list))))

                   (if text
                       (cond
                        ((eq text t) (message "Indenting"))
                        ((stringp text) (message "Closes: %s" text)))
                     (message "Current indentation")))))
      (goofie-indent-line-1)
      (setq goofie-indent-list (goofie-indentation-levels)
            goofie-indent-list-length (length goofie-indent-list)
            goofie-indent-index (1- goofie-indent-list-length)))))

(defun goofie-indent-region (start end)
  "`indent-region-function' for Goofie.
Leaves validly-indented lines alone, i.e. doesn't indent to
another valid position."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (goofie-indent-line-1 t))
      (forward-line 1))
    (move-marker end nil)))

;; -----------------------------------------------------------------------------
;;; Movement.

(defun goofie-beginning-of-defun ()
  "`beginning-of-defun-function' for GOOFIE.
Finds beginning of innermost nested type or function definition.
Returns the name of the definition found at the end, or nil if reached
start of buffer."
  (let ((ci (current-indentation))
        (def-re (rx (and line-start (0+ space) (eval (cons 'or goofie-declarators))
                         (1+ space)
                         (group (1+ (or word (syntax symbol)))))))
        found lep def-line)
    (if (goofie-comment-line-p)
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
               (not (goofie-in-string/comment)))
          (setq found t)))
    (back-to-indentation)))

(defun goofie-beginning-of-statement ()
  "Go to start of current statement.
Accounts for continuation lines and multi-line strings."
  (interactive)
  (beginning-of-line)
  (goofie-beginning-of-string)
  (while (goofie-continuation-line-p)
    (beginning-of-line)
    (forward-line -1)))

(defun goofie-end-of-statement ()
  "Go to the end of the current statement and return point.
Usually this is the beginning of the next line, but if there is a continuation,
we need to skip additional lines."
  (interactive)
  (end-of-line)
  (goofie-skip-comments/blanks)
  (while (goofie-continuation-line-p)
    (end-of-line)
    (goofie-skip-comments/blanks))
  (point))

(defun goofie-previous-statement (&optional count)
  "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (goofie-next-statement (- count))
    (goofie-beginning-of-statement)
    (while (and (> count 0) (not (bobp)))
      (goofie-skip-comments/blanks t)
      (goofie-beginning-of-statement)
      (unless (bobp) (setq count (1- count))))
    (back-to-indentation)
    count))

(defun goofie-next-statement (&optional count)
  "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (goofie-previous-statement (- count))
    (beginning-of-line)
    (while (and (> count 0) (not (eobp)))
      (goofie-end-of-statement)
      (goofie-skip-comments/blanks)
      (setq count (1- count)))
    count))

(defun goofie-beginning-of-block (&optional arg)
  "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`goofie-end-of-block' instead.
If point is on the first line of a block, use its outer block.
If current statement is in column zero, don't move and return nil.
Otherwise return non-nil."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((zerop arg))
   ((< arg 0) (goofie-end-of-block (- arg)))
   (t
    (let ((point (point)))
      (if (or (goofie-comment-line-p) (goofie-blank-line-p))
          (goofie-skip-comments/blanks t))
      (goofie-beginning-of-statement)
      (let ((ci (current-indentation)))
        (if (zerop ci)
            (not (goto-char point))     ; return nil
          ;; Look upwards for less indented statement.
          (if (catch 'done
                (while (zerop (forward-line -1))
                  (when (and (< (current-indentation) ci)
                             (not (goofie-comment-line-p t))
                             ;; Move to beginning to save effort in case
                             ;; this is in string.
                             (progn (goofie-beginning-of-statement) t)
                             (goofie-open-block-statement-p))
                    (beginning-of-line)
                    (throw 'done t)))
                (not (goto-char point))) ; Failed -- return nil
              (goofie-beginning-of-block (1- arg)))))))))

(defun goofie-end-of-block (&optional arg)
  "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`goofie-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block, don't
move and return nil.  Otherwise return t."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (goofie-beginning-of-block (- arg)))
  (while (and (> arg 0)
              (let* ((point (point))
                     (_ (if (or (goofie-comment-line-p) (goofie-blank-line-p))
                            (goofie-skip-comments/blanks t)))
                     (ci (current-indentation))
                     (open (goofie-open-block-statement-p)))
                (if (and (zerop ci) (not open))
                    (not (goto-char point))
                  (catch 'done
                    (while (zerop (goofie-next-statement))
                      (when (or (and open (<= (current-indentation) ci))
                                (< (current-indentation) ci))
                        (goofie-skip-comments/blanks t)
                        (beginning-of-line 2)
                        (throw 'done t)))))))
    (setq arg (1- arg)))
  (zerop arg))

;; -----------------------------------------------------------------------------
;;; Imenu

(defvar goofie-imenu-generic-expression
  (list
   (list nil
         (purecopy
          (eval-when-compile
            (rx (and line-start (0+ blank)
                     (group (eval (cons 'or goofie-functors)))
                     (1+ (or blank "\n"))
                     (group (1+ (or word (syntax symbol))))))))
         2)

   (list (purecopy "Types")
         (purecopy
          (eval-when-compile
            (rx (and line-start (0+ blank)
                     (group (eval (cons 'or goofie-type-declarators)))
                     (1+ (or blank "\n"))
                     (group (1+ (or word (syntax symbol))))))))
         2)

   (list (purecopy "Modules")
         (purecopy
          (eval-when-compile
            (rx (and line-start (0+ blank)
                     (group (eval (cons 'or goofie-module-declarators)))
                     (1+ (or blank "\n"))
                     (group (1+ (or word (syntax symbol))))))))
         2))
  "Imenu expression for GOOFIE-mode.  See `imenu-generic-expression'.")

;; -----------------------------------------------------------------------------
;;; `Electric' commands

(defun goofie-backspace (arg)
  "Maybe delete a level of indentation on the current line.
If not at the end of line's indentation, or on a comment line, just call
`backward-delete-char-untabify'.  With ARG, repeat that many times."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (goofie-continuation-line-p))
      (backward-delete-char-untabify arg)
    (let ((indent 0))
      (save-excursion
        (while (and (> arg 0) (goofie-beginning-of-block))
          (setq arg (1- arg)))
        (when (zerop arg)
          (setq indent (current-indentation))
          (message "Closes %s" (goofie-initial-text))))
      (delete-horizontal-space)
      (indent-to indent))))
(put 'goofie-backspace 'delete-selection 'supersede)

;; -----------------------------------------------------------------------------
;;; Info-look functionality

(declare-function info-lookup-maybe-add-help "info-look" (&rest arg))

(defun goofie-after-info-look ()
  "Set up info-look for GOOFIE.
Used with `eval-after-load'."
  (let* ((version "dunno")
         ;; Whether info files have a GOOFIE version suffix, e.g. in Debian.
         (versioned
          (with-temp-buffer
            (with-no-warnings (Info-mode))
            (condition-case ()
                ;; Don't use `info' because it would pop-up a *info* buffer.
                (with-no-warnings
                  (Info-goto-node (format "(goofie%s-lib)Miscellaneous Index"
                                          version))
                  t)
              (error nil)))))
    (info-lookup-maybe-add-help
     :mode 'goofie-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
         ;; The empty prefix just gets us highlighted terms.
         `((,(concat "(goofie" version "-ref)Miscellaneous Index") nil "")
           (,(concat "(goofie" version "-ref)Module Index" nil ""))
           (,(concat "(goofie" version "-ref)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(goofie" version "-ref)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(goofie" version "-lib)Module Index" nil ""))
           (,(concat "(goofie" version "-lib)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(goofie" version "-lib)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(goofie" version "-lib)Miscellaneous Index" nil "")))
       '(("(goofie-ref)Miscellaneous Index" nil "")
         ("(goofie-ref)Module Index" nil "")
         ("(goofie-ref)Function-Method-Variable Index" nil "")
         ("(goofie-ref)Class-Exception-Object Index" nil "")
         ("(goofie-lib)Module Index" nil "")
         ("(goofie-lib)Class-Exception-Object Index" nil "")
         ("(goofie-lib)Function-Method-Variable Index" nil "")
         ("(goofie-lib)Miscellaneous Index" nil ""))))))

(eval-after-load "info-look" '(goofie-after-info-look))

;; -----------------------------------------------------------------------------
;;; Miscellancy

(defun goofie-fill-paragraph (&optional justify)
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

(defun goofie-shift-left (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the left.
COUNT defaults to `goofie-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie.  It is an error if any lines in the region are indented less than
COUNT columns."
  (interactive (if (/= (mark) (point))
                   (list (region-beginning) (region-end) current-prefix-arg)
                 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count goofie-indent))
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

(defun goofie-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `goofie-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive (if mark-active
                   (list (region-beginning) (region-end) current-prefix-arg)
                 (list (point) (point) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count goofie-indent))
  (indent-rigidly start end count))

(defun goofie-outline-level ()
  "`outline-level' function for GOOFIE mode.

For heading starting with /// the outline level is the number of proceeding
spaces.

For heading starting with `goofie-indent' steps of indentation followed by ///
the outline level is the number of `goofie-indent' steps.

For heading starting with `goofie-indent' steps of indentation followed by a
defining keyword the level is the number of `goofie-indent' steps of indentation - 1."
  (let (buffer-invisibility-spec)
    (if (string-match-p "^///" (match-string 0))
        (- (outline-level) 3)
      (if (string-match-p "^\\s-*///" (match-string 0))
          (/ (current-indentation) goofie-indent)
        (1+ (/ (current-indentation) goofie-indent))))))

(defun goofie-mark-block ()
  "Mark the block around point.
Uses `goofie-beginning-of-block', `goofie-end-of-block'."
  (interactive)
  (push-mark)
  (goofie-beginning-of-block)
  (push-mark (point) nil t)
  (goofie-end-of-block)
  (exchange-point-and-mark))

;; -----------------------------------------------------------------------------
;;; Mode

;;;###autoload
(define-derived-mode goofie-mode fundamental-mode "GOOFIE"
  "Major mode for editing GOOFIE files.

The Emacs commands which work with `defun's,
e.g. \\[beginning-of-defun], deal with nested `function' and
`type' blocks.  They take the innermost one as current without
distinguishing method and type definitions.  Used multiple times,
they move over others at the same indentation level until they
reach the end of definitions at that level, when they move up a
level.

\\<goofie-mode-map>

\\[goofie-backspace] at the beginning of an indented statement
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

\\{goofie-mode-map}"
  :group 'goofie

  ;; Set up font-locking
  (set (make-local-variable 'font-lock-defaults)
       '(goofie-font-lock-keywords nil nil nil nil ;; ((?_ . "w")) nil
                               (font-lock-syntactic-keywords
                                . goofie-font-lock-syntactic-keywords)))

  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")

  ;; Indentation and filling
  (set (make-local-variable 'indent-line-function) #'goofie-indent-line)
  (set (make-local-variable 'indent-region-function) #'goofie-indent-region)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'goofie-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)

  ;; Outline mode
  (set (make-local-variable 'outline-regexp)
       (concat "///[ ]+\\|\\s-*///[ ]+\\|\\s-*" (rx (eval (cons 'or goofie-declarators))) "\\>"))
  (set (make-local-variable 'outline-level) #'goofie-outline-level)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)

  ;; Function searching
  (set (make-local-variable 'beginning-of-defun-function)
       'goofie-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'goofie-end-of-block)

  ;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression goofie-imenu-generic-expression)

  ;; Initialise indentation
  (when goofie-guess-indent (goofie-guess-indent)))

;; Add custom options
(custom-add-option 'goofie-mode-hook 'imenu-add-menubar-index)
(custom-add-option 'goofie-mode-hook
                   '(lambda ()
                      "Turn on Indent Tabs mode."
                      (set (make-local-variable 'indent-tabs-mode) t)))

(provide 'goofie)

;; -----------------------------------------------------------------------------
;;; goofie.el ends here
