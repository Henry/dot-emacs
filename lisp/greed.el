;;; greed.el ---  multi-buffer occur, grep, find  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Henry G. Weller.
;; Copyright (C) 2002-2010 Matsushita Akihisa.
;; Copyright (C) 1991 Markus Freericks.

;; Maintainer: Henry G. Weller.
;; URL: https://github.com/Henry/dot-emacs/blob/master/lisp/greed.el
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: matching, tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides `greed' (general regular-expression search and edit)
;; provides multi-buffer and multi-file regular-expression search and edit
;; functions modelled after the standard `occur' mode
;;
;; `greed' is derived from the `color-moccur' and `moccur-edit' modes created by
;; Matsushita Akihisa based on the original `moccur' multi-buffer occur mode of
;; Markus Freericks.
;;
;;; Usage:
;;
;;;  Functions:
;;
;; greed, greed-occur, greed-grep, greed-dir, greed-dired, greed-grep-find
;; greed-buffer-menu, greed-ibuffer, greed-isearch
;;
;;;  greed:
;;
;; `greed' <regexp> shows all occurrences of <regexp> in all open buffers that
;; refer to files.  The occurrences are displayed in the `*Greed*' buffer
;; running in `greed' mode;
;;
;;;   Keybindings:
;;
;; C-c C-c or RET   : go to the occurrence in the matched file buffer
;; q                : quit
;; <up>, n, j       : move to previous match in the `greed' buffer
;; <down>, p, k     : move to next match in the `greed' buffer
;; b                : scroll-down in the matched file buffer
;; SPC              : scroll-up in the matched file buffer
;; M-v              : scroll-down in the `greed' buffer
;; C-v              : scroll-up in the `greed' buffer
;; <                : execute M-< in matched file buffer
;; >                : execute M-> in matched file buffer
;; t                : toggle whether file buffer is displayed in other window
;; r                : re-search in matched file buffers
;; d, C-k           : `kill-line' in `greed' buffer
;; greed-flush-lines: `flush-lines' in `greed' buffer
;; greed-keep-lines : `keep-lines' in `greed' buffer
;; /                : undo (may not be working)
;; s                : execute `greed' on matched buffers only
;; u                : execute `greed' on previous condition
;; C-c C-i, C-x C-q : execute `greed-edit'
;;
;;;   Variables
;;
;; `greed-follow-mode': set true for the cursor motion in the `greed' buffer
;; to automatic display of the corresponding file buffer location.
;;
;; `greed-split-word': set true to input regular-expression as words separated
;; by space.  e.g. to search for "defun greed (regexp)" type "defun regexp" or
;; "regexp defun".
;;
;; Also if the first word is a keyword in `greed-special-word-list' use the
;; corresponding search function e.g.
;;
;; "! function" -> return lines that contain to word "function" which is part of
;; a function name.
;;
;; "" string" -> return lines that contain the word "string" which is part of a
;; string.
;;
;; "/ comment" -> return lines that contain the word "comment" in a comment.
;;
;; `greed-use-keyword': set true to map keywords to regular-expressions from
;; `greed-search-keyword-alist',
;;  e.g. "url" -> "[fht]*ttp://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+"
;;
;;;  greed-grep, greed-grep-find
;;
;; `greed-grep' <regexp> shows all occurrences of <regexp> in all of the files
;; in the current directory.
;;
;; `greed-grep-find' <regexp> shows all occurrences of <regexp> in files of
;; current directory tree recursively.
;;
;;;   Variables
;;
;; `greed-grep-default-word-near-point': set true to use the word at the point
;; as default regexp.
;;
;; `greed-grep-default-mask': default `File-mask' e.g.
;; (setq-default greed-grep-default-mask ".el")
;; run `greed-grep', and choose directory, in minibuffer, following text is
;; displayed:
;;
;; Input Regexp and FileMask:  .el
;;
;;;  greed-isearch
;;
;; M-o during `isearch': execute `greed' with current `isearch' string on the
;; current buffer.
;;
;; M-O during `isearch': execute `greed' with current `isearch' string on all
;; buffers.
;;
;;;  greed-occur
;;
;; `greed-occur': search only the current buffer, equivalent to `occur'.
;;
;;;  greed-buffer-menu
;;
;; `greed-buffer-menu': search the buffers marked in `buffer-menu'.
;;
;;;  greed-ibuffer
;;
;; `greed-ibuffer': search the buffers marked in `ibuffer'.
;;
;;;  greed-dired
;;
;; `greed-dired': search through all marked files in `dired' buffer.
;;
;;;  greed-dir
;;
;; `greed-dir': search files in the current directory
;; C-u M-x `greed-dir': search favorite directory list `greed-dir-list'.
;;
;;;   Variables
;;
;; `greed-dir-exclusion-mask': exclude the file names that match this
;; regular-expression.
;;
;; `greed-dir-maximum-size': maximum file size (kB) opened and searched by
;;
;; `greed-dir' and `greed-grep' and `greed-grep-find'.
;;
;; `greed-dir-mask': open files which match this regular-expression, defult is
;; (".*").
;;
;; `greed-dir-list': favorite directory list, e.g.
;;
;; (setq greed-dir-list
;;       '(
;;      ;; name      directory               mask               option
;;         ("dir"    default-directory       (".*")             dir)
;;         ("config" "~/.emacs.d/my-lisp/"   ("\\.el$")         nil)
;;         ("multi"  (("~/.emacs.d/mylisp/")
;;                    ("~/.emacs.d/lisp/"))  ("\\.el$")         nil)
;;         ("emacs"  "~/Emacs/"              (".*")             sub)
;;       ))
;;
;;     name     : name for the directory
;;     directory: directory to search
;;     mask     : list of file-masks (regular-expressions).
;;     option   : `dir': select directory like `find-file'.
;;                `sub': select sub-directory.
;;
;; Moreover complex example of `greed-dir-list':
;; (setq greed-dir-list
;;       '(
;;         ;; Multi-directories may be specified if option is nil
;;         ("soft"
;;          (
;;           ("~/www/soft/")
;;           ("~/mylisp/")
;;           )
;;          ("\\.texi$") nil)
;;
;;         ;; Search files recursively in ~/.emacs.d/my-lisp.
;;         ;; Also search files in ~/Emacs.
;;         ("test-recursive"
;;          (("~/.emacs.d/my-lisp" t)
;;           ("~/Emacs"))
;;           (".*") nil)
;;
;;         ;; Search files recursively in ~/.emacs.d/my-lisp
;;         ;; other than those ending in .txt or .el
;;         ("ignore-txt"
;;          (("~/mylisp" t (".+.txt" ".+.el"))
;;           ("~/user"))
;;           (".*") nil)
;;
;;         ;; Search chosen directory recursively
;;         ("dir-recursive" ((default-directory t)) (".*") dir)
;;         ))
;;
;; `greed-dir-use-list': set true to execute `greed-dir' with prefix,
;; i.e. equivalent to C-u M-x `greed-dir'.
;;
;; `greed-dir-use-project': `greed-dir' need a name of `greed-dir-list'. If
;; `greed-dir-use-project' is nil, you have to type a name every time. If
;; `greed-dir-use-project' is non-nil and you searched current buffer by a name
;; of `greed-dir', `greed-dir' use the name.
;;
;;;  greed-edit
;;
;; `greed-edit': switch `greed' buffer to edit-mode in which changes can be made
;; to the selected lines and propagated to the source buffers when finished by
;; typing "C-c C-f" or "C-c C-c".  The changes may be aborted by typing "C-c
;; C-k" or removed from the region by typing "C-c C-r".
;;
;;;  Sample configuration
;;
;; (require 'greed)
;; (setq *greed-buffer-name-exclusion-list*
;;       '(".+TAGS.+" "*Completions*" "*Messages*" ".bbdb")
;;       greed-split-word t
;;       greed-dir-use-list t
;;       greed-dir-use-project t
;;       greed-dir-list
;;       '(
;;         ("dir" default-directory (".*") dir)
;;         ("soft" "~/www/soft/" ("\\.texi$") nil)
;;         ("config" "~/.emacs.d/my-lisp/"  ("\\.el$") nil)
;;         ("1.99" "~/stuff/1.99a6/"  (".*") sub)
;;         ))
;; (global-set-key "\C-x\C-o" 'greed-occur)
;; (define-key Buffer-menu-mode-map "O" 'greed-buffer-menu)
;; (define-key dired-mode-map "O" 'greed-dired)
;; (global-set-key "\C-c\C-x\C-o" 'greed)

;;; Code:

(eval-when-compile (require 'cl))

(defgroup greed nil
  "Customize greed"
  :group 'matching)

;;; Customizable Variables:

(defface greed-face
  '((((class color)
      (background dark))
     (:background "light grey" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "light cyan" :bold t))
    (t
     ()))
  "*Face used by greed to show the text that matches."
  :group 'greed)

(defface greed-current-line-face
  '((((class color)
      (background dark))
     (:underline t))
    (((class color)
      (background light))
     (:underline t))
    (t
     ()))
  "*Face used by greed."
  :group 'greed)

(defface greed-edit-face
  '((((class color)
      (background dark))
     (:background "Pink" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "*Face used for the changed text on greed buffer."
  :group 'greed-edit)

(defface greed-edit-file-face
  '((((class color)
      (background dark))
     (:background "gray30" :bold t))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "*Face used for the changed text on file buffer."
  :group 'greed-edit)

(defface greed-edit-done-face
  '((((class color)
      (background dark))
     (:foreground "gray30" :bold t))
    (((class color)
      (background light))
     (:foreground "ForestGreen" :bold t))
    (t
     ()))
  "*Face used for the line on greed buffer that can apply to file."
  :group 'greed-edit)

(defface greed-edit-reject-face
  '((((class color)
      (background dark))
     (:foreground "hot pink" :bold t))
    (((class color)
      (background light))
     (:foreground "red" :bold t))
    (t
     ()))
  "*Face used for the line on greed buffer that can not apply to file."
  :group 'greed-edit)

(defcustom greed-kill-greed-buffer nil
  "*Non-nil means to kill *greed* buffer automatically when you
exit *greed* buffer."
  :group 'greed
  :type 'boolean)

(defcustom greed-split-word nil
  "*Non-nil means means to input word splited by space.
You can search \"defun greed (regexp)\" by \"defun regexp\" or
\"regexp defun\".  You don't need to input complicated regexp.  But
you can not input regexp including space.."
  :group 'greed
  :type 'boolean)

(defcustom greed-default-ime-status t
  "*Non-nil means to inherit ime status."
  :group 'greed
  :type 'boolean)

(defcustom *greed-buffer-name-exclusion-list*
  '("TAGS" "*Completions*" "*Messages*" "^[ ].+")
  "Contains a list of regexps which don't search by greed.
Matching buffers are *not* searched for occurrences.  Per default, the
TAGS file is excluded."
  :group 'greed
  :type '(repeat regexp))

(defcustom *greed-buffer-name-inclusion-list* '("[^ ].*")
  "Contains a list of regexps.  *Only* matching buffers are searched.
Per default, this var contains only a \".*\" catchall-regexp."
  :group 'greed
  :type '(repeat regexp))

(defcustom greed-dir-mask '(".*")
  "Mask for greed-dir."
  :group 'greed
  :type '(repeat regexp))

(defcustom greed-dir-maximum-size nil
  "*Maximum size (kB) of a buffer for greed-dir and greed-grep(-find)."
  :group 'greed
  :type '(choice
          number
          (const :tag "infinite" nil)))

(defcustom greed-dir-exclusion-mask
  '( ;; binary
    "\\.elc$" "\\.exe$" "\\.dll$" "\\.lib$" "\\.lzh$"
    "\\.zip$" "\\.deb$" "\\.gz$" "\\.pdf$" "\\.tar$"
    "\\.gz$" "\\.7z$" "\\.o$" "\\.a$" "\\.mod$"
    "\\.nc$" "\\.obj$" "\\.ai$" "\\.fla$" "\\.swf$"
    "\\.dvi$" "\\.pdf$" "\\.bz2$" "\\.tgz$" "\\.cab$"
    "\\.sea$" "\\.bin$" "\\.fon$" "\\.fnt$" "\\.scr$"
    "\\.tmp$" "\\.wrl$" "\\.Z$"
    ;; sound & movie
    "\\.aif$" "\\.aiff$"  "\\.mp3$"  "\\.wma$" "\\.mpg$"
    "\\.mpeg$" "\\.aac$" "\\.mid$"  "\\.au$"  "\\.avi$"  "\\.dcr$"
    "\\.dir$"  "\\.dxr$" "\\.midi$"  "\\.mov$"  "\\.ra$"  "\\.ram$"
    "\\.vdo$" "\\.wav$"
    ;; Microsoft
    "\\.doc$" "\\.xls$" "\\.ppt$" "\\.mdb$" "\\.adp$"
    "\\.wri$"
    ;; image
    "\\.jpg$" "\\.gif$" "\\.tiff$" "\\.tif$" "\\.bmp$"
    "\\.png$" "\\.pbm$" "\\.jpeg$" "\\.xpm$" "\\.pbm$"
    "\\.ico$" "\\.eps$" "\\.psd$"
    ;;etc
    "/TAGS$"
    ;; backup file
    "\\~$"
    ;; version control
    "\\.svn/.+" "CVS/.+" "\\.git/.+"
    )
  "*List of file extensions which are excepted to search by
greed-dir and greed-grep(-find)."
  :group 'greed
  :type '(repeat regexp))

(defcustom greed-dir-use-list nil
  "Non-nil means to use your favorite directory list."
  :group 'greed
  :type 'boolean)

(defcustom greed-dir-use-project nil
  "Non-nil means to use your favorite directory list."
  :group 'greed
  :type 'boolean)

(defcustom greed-kill-dired-buffers nil
  "Non-nil means to kill buffer after greed-dired."
  :group 'greed
  :type 'boolean)

(defcustom greed-dir-list
  '(
    ;; name directory mask option
    ;; option = nil , dir , sub
    ("dir" default-directory (".*") dir)
    ("lisp" "~/mylisp/" ("\\.el" "\\.*texi") nil))
  "*List of directory which are searched by greed-dir."
  :group 'greed
  :type '(repeat
          (list (string :tag "Name")
                (choice
                 (directory :tag "Directory")
                 (file :tag "Filename")
                 (symbol :tag "Variable")
                 (repeat :tag "Advanced setting"
                         (list
                          (choice
                           (directory :tag "Directory")
                           (symbol :tag "Variable"))
                          (boolean :tag "Recursively" nil)
                          (repeat :tag "File Mask"
                                  (regexp :tag "File Mask not to search")))))
                (repeat :tag "File Mask" :default nil
                        (regexp :tag "File Mask" ".*"))
                (choice :tag "Option" :default nil
                        (const :tag "Default" nil)
                        (const :tag "Directory" dir)
                        (const :tag "Subdirectory" sub))
                (choice :tag "Default regexp" :default nil
                        (const :tag "Empty" nil)
                        (string :tag "Regexp" "")
                        (symbol :tag "Function to make regexp")))))

(defcustom greed-maximum-displayed-with-color 500
  "Max number that is displayed with color."
  :group 'greed
  :type 'number)

(defcustom greed-dir-recursive-search nil
  "Non-nil means to search files recursively."
  :group 'greed
  :type 'boolean)

(defcustom greed-buffer-sort-method 'greed-filepath-string<
  "Function to sort buffers."
  :group 'greed
  :type 'symbol)

(defcustom greed-special-word-list
  '(
    (";"
     greed-face-initialization
     greed-comment-check)
    ("/"
     greed-face-initialization
     greed-comment-check)
    ("\""
     greed-face-initialization
     greed-string-check)
    ("!"
     greed-face-initialization
     greed-function-check)
    (t ;; default
     greed-default-initial-function
     greed-default-check-function
     )
    )
  "Special-word function-to-initialize function-to-check."
  :group 'greed
  :type '(repeat
          (list (choice (string :tag "Keyword")
                        (const :tag "Default" t))
                (symbol :tag "Function to initialize")
                (symbol :tag "Function to check")
                )))

(defcustom greed-search-keyword-alist
  '(("url"  . "[fht]*ttp://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+")
    ("mail" . "[^][<>@ \n]+@[-_!~*'()a-zA-Z0-9?@&=+$,%#]+\\.[-_.!~*'()a-zA-Z0-9?@&=+$,%#]+"))
  "*Alist of KEYWORD and REGEXP."
  :group 'greed
  :type '(repeat
          (cons (string :tag "Keyword")
                (regexp :tag "Regexp"))))

(defcustom greed-use-keyword nil
  "Non-nil means to use greed-search-keyword-alist."
  :group 'greed
  :type 'boolean)

(defcustom greed-follow-mode nil
  "When t, cursor motion in the greed buffer causes
automatic display of the corresponding buffer location."
  :group 'greed
  :type 'boolean)

(defcustom greed-grep-default-word-near-point nil
  "When t, get a word near the point as default regexp string"
  :group 'greed
  :type 'boolean)

(defgroup greed-edit nil
  "Customize greed-edit"
  :group 'matching)

(defcustom greed-edit-highlight-edited-text nil
  "*Non-nil means to highlight the edited text."
  :group 'greed-edit
  :type 'boolean)

(defcustom greed-edit-remove-overlays nil
  "*Non-nil means to remove overlays when greed-quit is run."
  :group 'greed-edit
  :type 'boolean)

(defcustom greed-edit-remove-overlays-after-save-buffer t
  "*Non-nil means to remove overlays after save-buffer."
  :group 'greed-edit
  :type 'boolean)

(defcustom greed-query-when-buffer-read-only t
  "*Non-nil means query if read-only buffers should be made writable."
  :group 'greed-edit
  :type 'boolean)

;;; Hooks:

(defcustom greed-mode-hook nil
  "Hook run when entering greed mode."
  :type 'hook
  :group 'matching)

(defcustom greed-hook nil
  "Hook run by greed when there are any matches."
  :type 'hook
  :group 'matching)

(defcustom greed-goto-occurrence-hook nil
  "Hook run by greed after locating an occurrence.
This will be called with the cursor position at the occurrence.  An application
for this is to reveal context in an outline-mode when the occurrence is hidden."
  :type 'hook
  :group 'matching)

;;; Constants:

(defconst greed-buffer-heading-regexp
  "^[-+ ]*Buffer: \\([^\r\n]+\\) File: \\([^\r\n]+\\)$"
  "Regexp for matching buffer heading line in greed-mode buffer.")
(defconst greed-grep-buffer-heading-regexp
  "^[-+ ]*Buffer: File (grep): \\([^\r\n]+\\)$"
  "Regexp for matching buffer heading line in greed-grep-mode buffer.")
(defconst greed-line-number-regexp
  "^[ ]*\\([0-9]+\\) "
  "Regexp for matching line numbers in greed buffer.")

;;; Internal variables:

(defvar greed-grep-default-mask nil
  "File-mask string used for default in greed-grep and greed-grep-find")
(make-variable-buffer-local 'greed-grep-default-mask)

(defvar greed-overlays nil)
(make-variable-buffer-local 'greed-overlays)
(defvar greed-current-line-overlays nil)
(defvar greed-regexp-color "")
(defvar greed-regexp-list nil)
(defvar greed-file-name-regexp nil)
(defvar greed-regexp-input "")
(defvar greed-buffer-name "")
(defvar greed-buffer-match-count nil)
(defvar greed-before-buffer-name "")
(defvar greed-line nil)
(defvar greed-view-other-window t)
(make-variable-buffer-local 'greed-view-other-window)
(defvar greed-view-other-window-nobuf t)
(make-variable-buffer-local 'greed-view-other-window-nobuf)
(defvar greed-current-buffer nil)
(defvar greed-buffer-position nil)
(make-variable-buffer-local 'greed-buffer-position)
(defvar greed-buffers nil)
(defvar greed-match-buffers nil)
(defvar greed-buffers-before-greed nil)
(defvar greed-matches nil)
(defvar greed-buffer nil)
(defvar greed-last-command nil)
(defvar greed-windows-conf nil)
(defvar greed-special-word nil)
(defvar greed-fontlock-buffer nil)
(make-variable-buffer-local 'greed-fontlock-buffer)

;;;  greed-dir
(defvar greed-dir-mask-internal nil)
(defvar greed-dir-list-history nil)
(defvar greed-dir-buffer-project nil)
(make-variable-buffer-local 'greed-dir-buffer-project)
(defvar greed-dir-project-name nil)
(defvar greed-dir-project-list nil)
(defvar greed-dir-recursive-ignore-dir nil)

;;;  greed-grep
(defvar greed-grep-buffer-list nil)
(make-variable-buffer-local 'greed-grep-buffer-list)

;;;  greed-edit
(defvar greed-edit-overlays nil)
(defvar greed-edit-file-overlays nil)
(defvar greed-edit-result-overlays nil)
(make-local-variable 'greed-edit-file-overlays)
(defvar greed-edit-change-face-flg nil)
(defvar greed-edit-old-content)
(make-local-variable 'greed-edit-old-content)

;;; greed-isearch
(defun greed-isearch ()
  "Invoke `greed' from isearch within `current-buffer'."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search)
        (isearch-buffer (current-buffer)))
    (isearch-exit)
    (greed-setup)
    (greed-search
     (if isearch-regexp
         isearch-string
       (regexp-quote isearch-string))
     t
     (list isearch-buffer))))

(defun greed-isearch-all ()
  "Invoke `greed' from isearch in all buffers."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search)
        (buffers (greed-filter-buffers (buffer-list))))
    ;; sort
    (setq buffers (sort buffers greed-buffer-sort-method))
    (isearch-exit)
    (greed-setup)
    (greed-search
     (if isearch-regexp
         isearch-string
       (regexp-quote isearch-string))
     t
     buffers)))

(define-key isearch-mode-map (kbd "M-o") 'greed-isearch)
(define-key isearch-mode-map (kbd "M-O") 'greed-isearch-all)

;;; greed-occur
(defun greed-occur (regexp)
  "Use this instead of occur.
Argument REGEXP regexp.
Argument ARG whether buffers which is not related to files are searched."
  (interactive (list (greed-regexp-read-from-minibuf)
                     current-prefix-arg))
  (greed-setup)
  (setq greed-regexp-input regexp)
  (let ((buffers (list (current-buffer))))
    (greed-search regexp t buffers)))

;;; greed utility functions
(defun greed-goto-line (line)
  "Go to LINE, counting from line 1 at beginning of buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun greed-filepath-string< (buf1 buf2)
  "String< by function `buffer-file-name'.
Argument BUF1 BUFFER.
Argument BUF2 BUFFER."
  (if (and (buffer-file-name buf1)
           (buffer-file-name buf2))
      (string< (buffer-file-name buf1) (buffer-file-name buf2))
    (if (buffer-file-name buf1)
        buf1
      (if (buffer-file-name buf2)
          buf2
        (string< (buffer-name buf1) (buffer-name buf2))))))

(defun greed-buffer-in-list-p (buffer-name buffer-name-regexps)
  "Return t, if BUFFER-NAME match BUFFER-NAME-REGEXPS (list)."
  (cond ((null buffer-name-regexps) nil)
        ((eq (string-match (car buffer-name-regexps) buffer-name)
             0) t)
        (t (greed-buffer-in-list-p
            buffer-name (cdr buffer-name-regexps)))))

(defun greed-filter-buffers (buffer-list)
  "Return BUFFER-LIST which is filtered by some variables."
  (let ((greed-buffers nil))
    (while buffer-list
      (if (and (greed-buffer-in-list-p
                (buffer-name (car buffer-list))
                *greed-buffer-name-inclusion-list*)
               (not (greed-buffer-in-list-p
                     (buffer-name (car buffer-list))
                     *greed-buffer-name-exclusion-list*)))
          (setq greed-buffers
                (cons (car buffer-list)
                      greed-buffers)))
      (setq buffer-list (cdr buffer-list)))
    greed-buffers))

(defun greed-kill-buffer-func ()
  (when (get-buffer "*greed*") ;; there ought to be just one of these
    (let ((cur-buffer (current-buffer)))
      (with-current-buffer "*greed*"
        ;; remove current buffer from greed-grep-buffer-list so it won't get
        ;; killed in greed-grep-sync-kill-buffers
        (setq greed-grep-buffer-list
              (remq cur-buffer greed-grep-buffer-list))))
    (kill-buffer "*greed*")))

(defun greed-kill-buffer (arg)
  "Kill buffers related greed."
  (if arg
      (greed-kill-buffer-func)
    (if greed-kill-greed-buffer
        (greed-kill-buffer-func)
      (bury-buffer))))

(defun greed-bury-buffer ()
  "Kill buffers related greed."
  (if (get-buffer "*greed*") ;; there ought to be just one of these
      (bury-buffer (get-buffer "*greed*"))))

(defun greed-setup ()
  "Initialization of greed."
  (if (string= "*greed*"
               (buffer-name (current-buffer)))
      (greed-quit))
  (greed-kill-buffer t)
  (setq greed-current-buffer (current-buffer))
  (setq greed-windows-conf (current-window-configuration)))

(defun greed-insert-heading ()
  "Insert the 'Lines matching' heading in *greed* buffer, with
 the user input regexp displayed in font-lock-variable-name-face
 face."
  (let (pt)
    (setq pt (point))
    (insert "Lines matching")
    (when greed-split-word
      (insert " (split words)"))
    (insert ": ")
    (put-text-property pt (point) 'face 'font-lock-keyword-face)
    (setq pt (point))
    (insert greed-regexp-input "\n")
    (put-text-property pt (point) 'face 'font-lock-variable-name-face)))

(defun greed-file-size< (filename maxsize)
  (if
      (or
       (not maxsize)
       (> (* 1000 maxsize)
          (nth 7 (file-attributes filename))))
      t
    nil))

(defun greed-follow-mode ()
  (interactive)
  "Toggle follow-made: When t, cursor motion in the greed buffer causes
automatic display of the corresponding buffer location."
  (setq greed-follow-mode (not greed-follow-mode)))

;;;  Color and overlay
(defun greed-remove-overlays-on-all-buffers (&optional _beg _end _length)
  "Remove all overlays in all buffers.
Optional arguments are not used."
  (interactive "p")
  (if greed-current-line-overlays
      (progn
        (delete-overlay greed-current-line-overlays)
        (setq greed-current-line-overlays nil)))
  (save-excursion
    (let (buf (buflist (buffer-list)))
      (while buflist
        (setq buf (car buflist))
        (setq buflist (cdr buflist))
        (when (and buf
                   (buffer-live-p buf))
          (set-buffer buf)
          (when (not (memq major-mode '(greed-mode greed-grep-mode)))
            (while greed-overlays
              (delete-overlay (car greed-overlays))
              (setq greed-overlays (cdr greed-overlays))))
          (remove-hook 'after-change-functions
                       'greed-remove-overlays-on-all-buffers)
          (when greed-buffer-position
            (goto-char greed-buffer-position)
            (setq greed-buffer-position nil)))))))

(defun greed-buffer-hide-region (start end)
  (let ((o (make-overlay start end)))
    (overlay-put o 'invisible 'greed)
    (overlay-put o 'isearch-open-invisible
                 'outline-isearch-open-invisible)))

(defun greed-buffer-color ()
  "Put overlays in *greed* buffer."
  (let ((ov) (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (and (re-search-forward greed-line-number-regexp nil t)
                  (or (not greed-maximum-displayed-with-color)
                      (< count greed-maximum-displayed-with-color)))
        (progn
          (save-restriction
            (narrow-to-region (point) (line-end-position))
            (while (re-search-forward greed-regexp-color nil t)
              (setq count (+ count 1))
              (setq ov (make-overlay (match-beginning 0)
                                     (match-end 0)))
              (overlay-put ov 'face 'greed-face)
              (overlay-put ov 'priority 0)
              (setq greed-overlays (cons ov greed-overlays))))
          (when (> (+ 6 (save-excursion (end-of-line) (current-column)))
                   (frame-width))
            (save-excursion
              (beginning-of-line)
              (re-search-forward greed-line-number-regexp
                                 (line-end-position) t)
              (save-restriction
                (narrow-to-region (point) (line-end-position))
                (let ((end-pt (point)) (st (point)) (match-end-pt nil))
                  (while (re-search-forward greed-regexp-color
                                            (line-end-position) t)
                    (setq st (match-beginning 0))
                    (setq match-end-pt (match-end 0))
                    (cond
                     ((and
                       (> (length (buffer-substring-no-properties
                                   end-pt st))
                          10)
                       (< end-pt (- st 5)))
                      (greed-buffer-hide-region end-pt (- st 5))
                      (setq end-pt (+ 5 match-end-pt)))
                     (t
                      (setq end-pt (+ 5 match-end-pt))
                      (goto-char end-pt)
                      )))
                  (end-of-line)
                  (if (and
                       (> (line-end-position) end-pt)
                       (> (length (buffer-substring-no-properties
                                   end-pt (line-end-position)))
                          10))
                      (greed-buffer-hide-region end-pt
                                                (- (line-end-position) 5)))))))
          )))))

(defun greed-color-view ()
  "Put overlays to matched texts."
  (let ((ov) (count 0))
    (if (and greed-buffer-name
             (get-buffer greed-buffer-name))
        (progn
          (set-buffer (get-buffer greed-buffer-name))
          (when greed-current-line-overlays
            (delete-overlay greed-current-line-overlays)
            (setq greed-current-line-overlays nil))

          (save-excursion
            (goto-char (point-min))
            (greed-special-word-call-initialize-function)
            (while (and
                    (greed-search-line (car greed-regexp-list))
                    (or (not greed-maximum-displayed-with-color)
                        (< count greed-maximum-displayed-with-color)))
              (when (greed-special-word-call-check-function)
                (beginning-of-line)
                (while (and
                        (re-search-forward
                         greed-regexp-color (line-end-position) t)
                        (or (not greed-maximum-displayed-with-color)
                            (< count greed-maximum-displayed-with-color)))
                  (progn
                    (setq count (+ count 1))
                    (setq ov (make-overlay (match-beginning 0)
                                           (match-end 0)))
                    (overlay-put ov 'face 'greed-face)
                    (overlay-put ov 'priority 0)
                    (setq greed-overlays (cons ov greed-overlays)))))))
          (set-buffer greed-buffer)))))

(defun greed-color-current-line ()
  "Underline where the cursor is."
  (if (not greed-current-line-overlays)
      (setq greed-current-line-overlays
            (make-overlay
             (line-beginning-position) (1+ (line-end-position))))
    (move-overlay greed-current-line-overlays
                  (line-beginning-position) (1+ (line-end-position))))
  (overlay-put greed-current-line-overlays
               'face 'greed-current-line-face))

;;;  Display other window
(defun greed-get-info ()
  "Gets buffer name and line."
  (setq greed-view-other-window-nobuf t)
  (setq greed-buffer-name nil)
  (let ((end-pt) (start-pt) (file nil) (buffer nil) (str nil) (buf nil)
        (buflst (buffer-list)))
    ;;for greed-grep
    (when greed-follow-mode
      (save-window-excursion
        (save-excursion
          (greed-grep-goto)
          (setq buf (current-buffer))))
      (if (or
           (memq buf buflst)
           (memq buf greed-grep-buffer-list))
          ()
        (setq greed-grep-buffer-list
              (cons buf greed-grep-buffer-list))))
    ;;for greed-grep end
    (save-excursion
      (end-of-line)
      (if (re-search-backward
           "^[-+ ]*Buffer:[ ]*\\([^\r\n]*\\) File\\([^:/\r\n]*\\):[ ]*\\([^\r\n]+\\)$" nil t)
          (progn
            (setq start-pt (point))
            (setq buffer
                  (match-string-no-properties 1))
            (setq str (match-string-no-properties 2))
            (setq file (match-string-no-properties 3))
            (cond
             ((string-match "grep" str)
              (if (get-file-buffer file)
                  (setq greed-buffer-name
                        (buffer-name
                         (get-file-buffer file)))))
             (t
              (setq greed-buffer-name buffer))))
        (setq start-pt (point-min))))

    (save-excursion
      (end-of-line)
      (if (re-search-forward
           "^[-+ ]*Buffer: " nil t)
          (progn
            (beginning-of-line)
            (setq end-pt (point)))
        (setq end-pt (point-max))))

    (save-excursion
      (setq greed-buffer-match-count 0)
      (goto-char start-pt)
      (while (re-search-forward greed-line-number-regexp end-pt t)
        (setq greed-buffer-match-count (+ 1 greed-buffer-match-count))))

    (save-excursion
      (end-of-line)
      (if (re-search-backward greed-line-number-regexp
                              (line-beginning-position) t)
          (setq greed-line (buffer-substring
                            (match-beginning 1)
                            (match-end 1)))
        (setq greed-line "1")))
    (if (and greed-buffer-name
             (get-buffer greed-buffer-name)
             (buffer-live-p (get-buffer greed-buffer-name)))
        ()
      (setq greed-view-other-window-nobuf nil))))

(defun greed-color-check-view ()
  "If a matched buffer exists, the buffer is displayed."
  (if (and greed-buffer-name
           (get-buffer greed-buffer-name))
      (progn
        (set-buffer (get-buffer greed-buffer-name))
        (if greed-overlays
            ()
          (greed-color-view))
        (set-buffer greed-buffer))))

(defun greed-view-file ()
  "Display the matched buffer to other window."
  (if (eq greed-before-buffer-name greed-buffer-name)
      (greed-color-check-view)
    (if greed-current-line-overlays
        (progn
          (delete-overlay greed-current-line-overlays)
          (setq greed-overlays nil)))
    (greed-color-view))

  (switch-to-buffer-other-window
   (get-buffer greed-buffer-name))
  (greed-goto-line (string-to-number greed-line))
  (if (re-search-forward greed-regexp-color (line-end-position) t)
      ()
    (greed-goto-line (string-to-number greed-line)))

  ;; color
  (greed-color-current-line)

  (setq greed-before-buffer-name greed-buffer-name)
  (switch-to-buffer-other-window greed-buffer))

(defun greed-scroll-file (arg)
  "Scroll up the matched buffer.
If ARG is non-nil, scroll down the buffer."
  (switch-to-buffer-other-window
   (get-buffer greed-buffer-name))
  (condition-case nil
      (if arg
          (scroll-down)
        (scroll-up))
    (error nil))

  ;; color
  (greed-color-current-line)

  (setq greed-before-buffer-name greed-buffer-name)
  (switch-to-buffer-other-window greed-buffer))

(defun greed-internal-beginning-of-buffer (arg)
  "Begging-of-buffer in the matched buffer.
Argument ARG If non-nil, `end-of-buffer'."
  (switch-to-buffer-other-window
   (get-buffer greed-buffer-name))
  (condition-case nil
      (if arg
          (goto-char (point-max))
        (goto-char (point-min)))
    (error nil))

  ;; color
  (greed-color-current-line)

  (setq greed-before-buffer-name greed-buffer-name)
  (switch-to-buffer-other-window greed-buffer))

;;;  Minibuffer
(defvar greed-dir-default-word nil)
(defun greed-set-default-word ()
  "Set default word to regexp."
  (cond
   ((and greed-dir-project-name
         (nth 5 (assoc (car greed-dir-project-name) greed-dir-list)))
    (setq greed-dir-default-word
          (if (nth 5 (assoc (car greed-dir-project-name) greed-dir-list))
              (nth 5 (assoc (car greed-dir-project-name) greed-dir-list))
            ""))
    (if (stringp greed-dir-default-word)
        greed-dir-default-word
      (condition-case nil
          (funcall greed-dir-default-word)
        (error nil))))
   ((and
     (or (and (boundp 'mark-active) mark-active)
         (and (fboundp 'region-exists-p) (region-exists-p)))
     (< (- (region-end) (region-beginning)) 50))
    (buffer-substring-no-properties
     (region-beginning) (region-end)))
   ((> (length (thing-at-point 'symbol)) 0)
    (thing-at-point 'symbol))
   ((> (length (thing-at-point 'word)) 0)
    (thing-at-point 'word))
   (t
    (if (and regexp-history (stringp (car regexp-history)))
        (car regexp-history)
      ""))))

(defun greed-regexp-read-from-minibuf ()
  "Read regexp from minibuffer."
  (let (default input lst (search-lst nil) greed-dir-default-word)
    (setq default (greed-set-default-word))
    (setq input
          (read-from-minibuffer
           "List lines matching regexp: "
           ;;(format "List lines matching regexp (default `%s'): "
           ;;        default)
           (cons default 0) ;; initial string
           nil nil
           'regexp-history
           default
           greed-default-ime-status))
    (when (and (equal input "") default)
      (setq input default)
      (setq regexp-history (cons input regexp-history)))
    (when greed-split-word
      (setq lst (greed-split-string input))
      (while lst
        (if (string-match "^b:" (car lst))
            ()
          (setq search-lst (cons (car lst) search-lst)))
        (setq lst (cdr lst)))
      (if (= 0 (length search-lst))
          (error "Input search string")))
    input))

;;;  Search function
(defun greed-search-line (regexp)
  "Corresponding to re-search-line.
Argument REGEXP REGEXP to search."
  (let ((lst greed-regexp-list)
        (split-match 0))
    ;; if return nil, greed search next buffer
    (cond
     ((and greed-split-word lst)
      ;; search method for split-word
      (while (and (not (= (length greed-regexp-list) split-match))
                  (re-search-forward regexp nil t))
        (setq lst greed-regexp-list)
        (setq split-match 0)
        (while lst
          (save-excursion
            (beginning-of-line)
            (if (re-search-forward (car lst) (line-end-position) t)
                (setq split-match (+ split-match 1)))
            (setq lst (cdr lst)))))
      (if (= (length greed-regexp-list) split-match)
          t
        nil))
     (t
      ;; defualt
      (re-search-forward regexp nil t)))))

(defun greed-search-buffer (&optional regexp currbuf name)
  "Search REGEXP in CURRBUF.
If NAME exists, `greed-search-buffer' works as grep."
  (let ((match-str nil) fname)
    (set-buffer currbuf)
    (setq greed-buffer-position (point))

    ;;(make-local-hook 'after-change-functions)
    ;;(remove-hook 'after-change-functions 'greed-remove-overlays)
    (add-hook 'after-change-functions
              'greed-remove-overlays-on-all-buffers nil t)

    (goto-char (point-min))

    (greed-special-word-call-initialize-function)

    (while (greed-search-line regexp)
      (when (greed-special-word-call-check-function)
        (setq greed-matches (+ greed-matches 1))
        (let* ((linenum (count-lines
                         (save-restriction (widen) (point-min)) (point)))
               (tag (format "\n%5d " linenum)))
          (put-text-property 0 (length tag) 'face 'font-lock-constant-face tag)
          (setq
           match-str
           (cons
            (concat tag
                    (buffer-substring
                     (line-beginning-position) (line-end-position)))
            match-str))
          (forward-line nil))))
    (setq match-str (reverse match-str))
    (with-current-buffer greed-buffer
      (if (not match-str)
          nil
        (let (pt)
          (cond
           (name
            (setq pt (point))
            (insert "Buffer: File (grep): ")
            (put-text-property pt (point) 'face 'font-lock-keyword-face)
            (setq pt (point))
            (insert name "\n")
            (put-text-property pt (point) 'face 'font-lock-variable-name-face))
           (t
            (if (buffer-file-name currbuf)
                (setq fname (buffer-file-name currbuf))
              (setq fname "Not file"))
            (setq pt (point))
            (insert "Buffer: ")
            (put-text-property pt (point) 'face 'font-lock-keyword-face)
            (setq pt (point))
            (insert (buffer-name currbuf))
            (put-text-property pt (point) 'face 'font-lock-variable-name-face)
            (setq pt (point))
            (insert " File: ")
            (put-text-property pt (point) 'face 'font-lock-keyword-face)
            (setq pt (point))
            (insert fname "\n")
            (put-text-property pt (point) 'face
                               'font-lock-variable-name-face))))

        (while match-str
          (insert (car match-str))
          (setq match-str (cdr match-str)))
        (insert "\n\n")
        t))))

(defvar greed-searched-list nil)
(defun greed-search (regexp arg buffers)
  "Search REGEXP in BUFFERS (list).
If ARG is non-nil, also search buffer that doesn't have file name"

  (when (or
         (not regexp)
         (equal regexp ""))
    (error "No search word specified!"))
  ;; initialize
  (let ((lst
         (list
          regexp arg buffers)))
    (if (equal lst (car greed-searched-list))
        ()
      (setq greed-searched-list
            (cons
             (list
              regexp arg buffers)
             greed-searched-list))))

  (setq greed-special-word nil)
  (greed-set-regexp regexp)
  (greed-set-regexp-for-color)

  ;; variable reset
  (setq greed-dir-project-name nil)
  (setq greed-matches 0)
  (setq greed-match-buffers nil)
  (setq greed-regexp-input regexp)
  (if (eq (car regexp-history) greed-regexp-input)
      ()
    (setq regexp-history
          (cons greed-regexp-input regexp-history)))

  (save-excursion
    (setq greed-buffer (generate-new-buffer "*greed*"))
    (set-buffer greed-buffer)
    (greed-insert-heading)
    (setq greed-buffers buffers)

    ;; search all buffers
    (while buffers
      (if (and
           (car buffers)
           (buffer-live-p (car buffers))
           ;; if b:regexp exists,
           (if (and greed-file-name-regexp
                    greed-split-word)
               (string-match greed-file-name-regexp
                             (buffer-name (car buffers)))
             t))
          (if (and (not arg)
                   (not (buffer-file-name (car buffers))))
              (setq buffers (cdr buffers))
            (if (greed-search-buffer (car greed-regexp-list) (car buffers))
                (setq greed-match-buffers
                      (cons (car buffers) greed-match-buffers)))
            (setq buffers (cdr buffers)))
        ;; illegal buffer
        (setq buffers (cdr buffers))))
    (if (> greed-matches 0)
        (with-current-buffer greed-buffer
          (greed-mode)
          ;; highlight greed buffer
          (greed-buffer-color)
          (setq buffer-undo-list nil)

          ;; move cursor to the first matching text
          (set-buffer greed-buffer)

          (goto-char (point-min))
          (forward-line 2)

          (beginning-of-line)
          (re-search-forward greed-line-number-regexp nil t)
          (re-search-forward (car greed-regexp-list) nil t)

          (greed-get-info)

          (setq greed-before-buffer-name greed-buffer-name)
          (greed-color-view)

          ;; preview file
          (greed-view-file)
          (pop-to-buffer greed-buffer)
          (run-hooks 'greed-hook)
          (message "%d matches" greed-matches)
          t)
      (message "no matches")
      (setq greed-searched-list
            (cdr greed-searched-list))
      (greed-kill-buffer t)
      (greed-remove-overlays-on-all-buffers)
      nil)))

(defun greed-search-undo ()
  (interactive)
  (greed-setup)
  (setq greed-last-command 'greed-search-undo)
  (unless (nth 1 greed-searched-list)
    (error "No undo information"))
  (setq greed-searched-list (cdr greed-searched-list))
  (let ((buffers (car (cdr (cdr (car greed-searched-list)))))
        (regexp (car (car greed-searched-list)))
        (arg (car (cdr (car greed-searched-list)))))
    ;; sort
    (setq buffers (sort buffers greed-buffer-sort-method))
    (greed-search regexp arg buffers)))

(defun greed-search-update ()
  (interactive)
  (greed-setup)
  (setq greed-last-command 'greed-search-update)
  (let ((buffers (car (cdr (cdr (car greed-searched-list)))))
        (regexp (car (car greed-searched-list)))
        (arg (car (cdr (car greed-searched-list)))))
    ;; sort
    (setq buffers (sort buffers greed-buffer-sort-method))
    (greed-search regexp arg buffers)))

;;;  Search word
(defun greed-split-string (string &optional separators)
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ ]+\".

But if substring is invalid regexp, this function doesn't split into
substrings.

Example:
 greed split string -> '(\"greed\" \"split\" \"string\")
 greed [a-z ]+ search -> '(\"greed\" \"[a-z ]+\" \"search\")"

  ;; strip whitespace from end of string
  (setq string
        (substring
         string
         0
         (string-match "[ ]+$" string)))
  (while (string-match "^[ ]+" string)
    (setq string
          (substring
           string
           1)))
  (let* ((rexp (or separators "[ ]+"))
         (lst (split-string string rexp))
         (new-lst nil)
         (current-regexp nil)
         (regexp-p nil)
         (regexp nil))

    (when (and
           greed-split-word
           (assoc (car lst) greed-special-word-list)
           (> (length lst) 1))
      (setq greed-regexp-list (cdr greed-regexp-list))
      (setq greed-special-word (car lst))
      (setq lst (cdr lst)))

    (while lst
      (setq current-regexp (concat
                            regexp
                            (if regexp
                                " ")
                            (car lst)))
      (setq regexp nil)
      (setq lst (cdr lst))
      (setq regexp-p t)
      (condition-case nil
          (string-match current-regexp "test")
        (error (setq regexp-p nil)))

      (cond
       ((and greed-use-keyword
             regexp-p
             (assoc current-regexp greed-search-keyword-alist))
        (setq new-lst
              (cons
               (cdr (assoc current-regexp greed-search-keyword-alist))
               new-lst)))
       (regexp-p
        (setq new-lst (cons current-regexp new-lst)))
       (t
        (setq regexp (concat current-regexp
                             (if regexp " ") regexp)))))

    (if regexp
        (setq new-lst
              (append new-lst
                      (mapcar
                       #'(lambda (string)
                           (if (and greed-use-keyword
                                    (assoc string greed-search-keyword-alist))
                               (cdr (assoc string greed-search-keyword-alist))
                             (regexp-quote string)))
                       (split-string regexp)))))
    (if (and
         (not new-lst)
         (not regexp))
        (error "Invalid regexp"))

    (setq new-lst (reverse new-lst))

    new-lst))

(defun greed-word-split (regexp &optional norestrict)
  "Splits REGEXP into substrings."
  (setq greed-file-name-regexp nil)
  (let ((lst (greed-split-string regexp))
        (regexp-list nil))

    (while lst
      (if (and (not norestrict)
               greed-split-word (string-match "^b:" (car lst)))
          (setq greed-file-name-regexp
                (cons (substring (car lst) 2) greed-file-name-regexp))
        (setq regexp-list (cons (car lst) regexp-list)))
      (setq lst (cdr lst)))

    (if (and greed-split-word greed-file-name-regexp)
        (progn
          (setq lst greed-file-name-regexp)
          (setq greed-file-name-regexp (concat "\\(" (car lst)))
          (setq lst (cdr lst))
          (while lst
            (setq greed-file-name-regexp
                  (concat greed-file-name-regexp
                          "\\|"
                          (car lst)))
            (setq lst (cdr lst)))
          (setq greed-file-name-regexp
                (concat greed-file-name-regexp "\\)"))))
    regexp-list))

(defun greed-set-regexp (regexp)
  "Set `greed-regexp-list' and `greed-file-name-regexp' from user regexp."
  (setq greed-regexp-list nil)
  (setq greed-file-name-regexp nil)

  (if greed-split-word
      (setq greed-regexp-list (greed-word-split regexp))
    (setq greed-regexp-list (list regexp))))

(defun greed-set-regexp-for-color ()
  "Make regexp for coloring up."
  (let ((list (cdr greed-regexp-list)))
    (if greed-split-word
        (progn
          (setq greed-regexp-color (concat
                                    "\\(" (car greed-regexp-list)))
          (while list
            (setq greed-regexp-color
                  (concat greed-regexp-color
                          "\\|"
                          (car list)))
            (setq list (cdr list)))
          (setq greed-regexp-color
                (concat greed-regexp-color "\\)")))
      (setq greed-regexp-color (car greed-regexp-list)))))

;;;  greed special word
;;  basic functions
(defun greed-special-word-call-initialize-function ()
  "Initialize function for special word function."
  (cond
   ((and greed-split-word
         greed-special-word)
    (if (nth 1 (assoc greed-special-word greed-special-word-list))
        (funcall
         (nth 1 (assoc greed-special-word greed-special-word-list)))))
   (t
    (if (nth 1 (assoc t greed-special-word-list))
        (funcall
         (nth 1 (assoc t greed-special-word-list)))))))

(defun greed-special-word-call-check-function ()
  "Function to check whether the matched text is acceptable."
  (cond
   ((and greed-split-word
         greed-special-word)
    (or
     (and (assoc greed-special-word greed-special-word-list)
          (nth 2 (assoc greed-special-word greed-special-word-list))
          (funcall
           (nth 2 (assoc greed-special-word greed-special-word-list))))
     (not
      (assoc greed-special-word greed-special-word-list))
     (not
      (nth 2 (assoc greed-special-word greed-special-word-list)))))
   (t
    (if (nth 2 (assoc t greed-special-word-list))
        (funcall
         (nth 2 (assoc t greed-special-word-list)))
      t))))

;;;  Functions
(defun greed-face-check (facename)
  "Check whether the face of current point is FACENAME."
  (let ((face
         (save-excursion
           (forward-char -1)
           (get-text-property (point) 'face))))
    (cond
     ((listp face)
      (memq facename face))
     (t
      (eq facename face)))))

(defun greed-face-initialization ()
  "Call 'font-lock-default-fontify-buffer'."
  (let ((font-lock-support-mode 'fast-lock-mode))
    (if greed-fontlock-buffer
        ()
      (setq greed-fontlock-buffer t)
      (font-lock-default-fontify-buffer))))

(defun greed-default-initial-function ()
  ())

(defun greed-default-check-function ()
  t)

(defun greed-comment-check ()
  (greed-face-check 'font-lock-comment-face))

(defun greed-string-check ()
  (greed-face-check 'font-lock-string-face))

(defun greed-function-check ()
  (cond
   ((eq major-mode 'texinfo-mode)
    (greed-face-check 'texinfo-heading-face))
   ((eq major-mode 'change-log-mode)
    (greed-face-check 'change-log-file-face))
   ((eq major-mode 'outline-mode)
    (if (save-excursion
          (re-search-backward
           (concat "^" outline-regexp) (line-beginning-position) t))
        t
      nil))
   (t
    (or
     (greed-face-check 'font-lock-function-name-face)
     (greed-face-check 'font-lock-variable-name-face))
    )))


;;; greed
(defun greed (regexp arg)
  "Show all lines of all buffers containing a match for REGEXP.
The lines are shown in a buffer named *greed*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive (list (greed-regexp-read-from-minibuf)
                     current-prefix-arg))

  (greed-setup)
  (setq greed-last-command 'greed)

  (let ((buffers (greed-filter-buffers (buffer-list))))
    ;; sort
    (setq buffers (sort buffers greed-buffer-sort-method))
    (greed-search regexp arg buffers)))

(defun greed-grep-correspond-ext-p (filename list)
  (let ((ret nil))
    (while list
      (when (string-match (car list) filename)
        (setq ret t))
      (setq list (cdr list)))
    ret))

;;; greed-grep
(defun greed-search-file-p (filename)
  (and
   (file-readable-p filename)
   (and
    (greed-file-size< filename greed-dir-maximum-size)
    (not (greed-dir-in-list-p filename
                              greed-dir-exclusion-mask)))))

(defun greed-search-files-init (regexp)
  (setq greed-special-word nil)
  (greed-set-regexp regexp)
  (greed-set-regexp-for-color)

  (setq greed-matches 0)
  (setq greed-regexp-input regexp)
  (if (eq (car regexp-history) greed-regexp-input)
      ()
    (setq regexp-history
          (cons greed-regexp-input regexp-history))))

(defun greed-search-all-files (files)
  (let ((total (length files))
        (num 0))
    (condition-case nil
        (while files
          (setq num (+ num 1))
          (with-temp-buffer
            (when
                (or
                 (eq greed-last-command 'greed-grep)
                 (and
                  (not (eq greed-last-command 'greed-grep))
                  (greed-search-file-p (car files))))
              (message "Searching %d/%d (%d matches) : %s ..."
                       num total greed-matches
                       (file-relative-name (car files) default-directory))
              (condition-case nil
                  (insert-file-contents (car files))
                (error nil)))
            (widen)
            (greed-search-buffer (car greed-regexp-list) (current-buffer)
                                 (car files)))
          (setq files (cdr files)))
      (quit ()))))

(defun greed-search-files (regexp files)
  "Search REGEXP in FILES (list)."

  ;; initialize
  (greed-search-files-init regexp)

  (save-excursion
    (setq greed-buffer (generate-new-buffer "*greed*"))
    (set-buffer greed-buffer)
    (greed-insert-heading)

    ;; search all buffers
    (greed-search-all-files files)
    (message "Searching done!")
    (if (> greed-matches 0)
        (progn
          (set-buffer greed-buffer)
          (greed-grep-mode)
          ;; highlight greed buffer
          (greed-buffer-color)
          (setq buffer-undo-list nil)

          ;; move cursor to the first matching text
          (set-buffer greed-buffer)
          ;;(setq greed-view-other-window nil)

          (pop-to-buffer greed-buffer)
          (goto-char (point-min))

          (message "%d matches" greed-matches)
          t)
      (message "no matches")
      (greed-kill-buffer t)
      (greed-remove-overlays-on-all-buffers)
      nil)))

(defun greed-grep-sync-kill-buffers ()
  (let (buf)
    (when greed-grep-buffer-list
      (while greed-grep-buffer-list
        (setq buf (car greed-grep-buffer-list))
        (setq greed-grep-buffer-list
              (cdr greed-grep-buffer-list))
        (if (and (buffer-live-p buf)
                 (not (buffer-modified-p buf)))
            (kill-buffer buf))))))

(add-hook 'kill-buffer-hook
          '(lambda ()
             (if (eq major-mode 'greed-grep-mode)
                 (greed-grep-sync-kill-buffers))))

(defun greed-grep-goto ()
  (interactive)
  (let (file line)
    (save-excursion
      (if (re-search-backward greed-grep-buffer-heading-regexp nil t)
          (setq file
                (buffer-substring-no-properties
                 (match-beginning 1)
                 (match-end 1)))))
    (save-excursion
      (end-of-line)
      (if (re-search-backward greed-line-number-regexp nil t)
          (setq line
                (string-to-number
                 (buffer-substring-no-properties
                  (match-beginning 1)
                  (match-end 1))))))
    (when (and file line)
      (find-file-other-window file)
      (widen)
      (greed-goto-line line))))

(defun greed-grep-read-directory ()
  (let ((dir default-directory))
    (setq dir
          (read-file-name "Directory: " nil nil t))
    (if (and (file-exists-p dir)
             (file-directory-p  dir))
        (setq dir (file-name-as-directory dir))
      (setq dir (file-name-as-directory (file-name-directory dir)))
      (if (and (file-exists-p dir)
               (file-directory-p  dir))
          ()
        (error (format "No such directory %s" dir))
        (sleep-for 1)
        (setq dir nil)))
    dir))

(defun greed-grep-read-regexp (&optional mask)
  (let (input (wd nil) (init nil) (pt 1))
    (when greed-grep-default-word-near-point
      ;; get a word near the point as default regexp string
      (setq wd (thing-at-point 'symbol))
      (set-text-properties 0 (length wd) nil wd)
      ;; put point to the end of default word
      (setq pt (1+ (length wd))))
    (setq init (cons (concat wd " " mask) pt))
    (setq input
          (read-from-minibuffer "Input Regexp and FileMask: " init))
    (greed-split-string input " ")))

(defun greed-grep (dir inputs)
  (interactive
   (list (greed-grep-read-directory)
         (greed-grep-read-regexp greed-grep-default-mask)))
  (greed-setup)
  (setq greed-last-command 'greed-grep)

  (let (regexps mask files)
    (setq regexps
          (mapconcat 'concat
                     (if (= 1 (length inputs))
                         inputs
                       (reverse (cdr (reverse inputs))))
                     " "))
    (setq mask
          (if (= 1 (length inputs))
              "."
            (car (reverse inputs))))
    (setq files (directory-files dir t mask))
    (let (list)
      (dolist (elt files)
        (cond
         ((file-directory-p elt)
          ())
         (t
          (push elt list))))
      (setq files (reverse list)))
    (greed-search-files regexps files)
    ))

(defun greed-grep-find-subdir (dir mask)
  (let ((files (cdr (cdr (directory-files dir t)))) (list) (plist))
    (if (not (greed-search-file-p dir))
        (setq list nil)
      (dolist (elt files)
        (cond
         ((and
           (not (string-match "^[.]+$" (file-name-nondirectory elt)))
           (file-directory-p elt))
          (setq list (append (greed-grep-find-subdir elt mask) list)))
         ((string-match "^[.]+$" (file-name-nondirectory elt))
          ())
         ((string-match mask (file-name-nondirectory elt))
          (push elt list))
         (t ()))
        (if (not (eq list plist))
            (message "Listing %s ..." (file-name-directory elt)))
        (setq plist list)))
    list))

(defun greed-grep-find (dir inputs)
  (interactive
   (list (greed-grep-read-directory)
         (greed-grep-read-regexp greed-grep-default-mask)))
  (greed-setup)
  (setq greed-last-command 'greed-grep-find)

  (let (regexps
        mask (files nil)
        ;;(default-directory dir)
        )
    (setq regexps
          (mapconcat 'concat
                     (if (= 1 (length inputs))
                         inputs
                       (reverse (cdr (reverse inputs))))
                     " "))
    (setq mask
          (if (= 1 (length inputs))
              "."
            (car (reverse inputs))))
    (message "Listing files...")
    (cond
     ((listp dir)
      (while dir
        (cond
         ((file-directory-p (car dir))
          (setq files (append
                       (reverse (greed-grep-find-subdir (car dir) mask))
                       files)))
         (t
          (setq files (cons
                       (car dir)
                       files))))
        (setq dir (cdr dir))))
     (t
      (setq files (reverse (greed-grep-find-subdir dir mask)))))
    (message "Listing files done!")
    (greed-search-files regexps files)
    ))

;;; greed-dir
;;  utility
(defun greed-dir-in-list-p (dir-name dir-name-regexps)
  (let ((case-fold-search t))
    (cond ((null dir-name-regexps) nil)
          ((string-match  (car dir-name-regexps) dir-name) t)
          (t (greed-dir-in-list-p dir-name (cdr dir-name-regexps))))))

(defun greed-add-files-to-search-list (files dir &optional norest recursive)
  (let ((buffers nil) (file-regexps greed-dir-recursive-ignore-dir)
        (file-ignore nil)
        file-name buf-name
        (enable-local-eval t))
    (while files
      (setq file-ignore nil)
      (setq file-regexps greed-dir-recursive-ignore-dir)
      (setq buf-name nil)
      (setq file-name (expand-file-name (car files) dir))

      (while file-regexps
        (if (string-match (car file-regexps) file-name)
            (setq file-ignore t))
        (setq file-regexps (cdr file-regexps)))
      (when (not file-ignore)
        (if (file-directory-p file-name)
            (cond
             ((eq 'dired recursive)
              (setq buffers
                    (append
                     (greed-add-files-to-search-list
                      (directory-files file-name) file-name norest nil)
                     buffers)))
             ((and recursive
                   (not (string= (expand-file-name "." dir)
                                 file-name))
                   (not (string= (expand-file-name ".." dir)
                                 file-name)))
              (setq buffers
                    (append
                     (greed-add-files-to-search-list
                      (directory-files file-name) file-name norest recursive)
                     buffers)))
             (t
              nil))
          (if (and
               (file-readable-p file-name)
               (or norest
                   (and
                    (greed-file-size< file-name greed-dir-maximum-size)
                    (greed-dir-in-list-p file-name greed-dir-mask-internal)
                    (not (greed-dir-in-list-p file-name
                                              greed-dir-exclusion-mask)))))
              (progn
                (if (get-file-buffer file-name)
                    (setq buf-name (get-file-buffer file-name))
                  (setq buf-name (find-file-noselect file-name)))
                (if (cdr file-name-history)
                    (setq file-name-history (cdr file-name-history)))
                (save-current-buffer
                  (set-buffer buf-name)
                  (setq greed-dir-buffer-project greed-dir-project-name))
                (if buf-name
                    (setq buffers (cons buf-name buffers)))))))
      (setq files (cdr files)))
    buffers))

(defun greed-add-directory-to-search-list (dir)
  (setq greed-dir-recursive-ignore-dir nil)
  (let ((buffers nil))
    (if (listp dir)
        (progn
          (let ((recursive nil) (cdir nil))
            (while dir
              (setq cdir (eval (car (car dir))))
              (setq greed-dir-recursive-ignore-dir
                    (nth 2 (car dir)))
              (setq recursive
                    (nth 1 (car dir)))
              (setq buffers
                    (append
                     (if (file-directory-p cdir)
                         (greed-add-files-to-search-list
                          (directory-files cdir) cdir nil recursive)
                       (greed-add-files-to-search-list
                        (list cdir) (file-name-directory cdir) t))
                     buffers))
              (setq dir (cdr dir)))))
      (let ((files (directory-files dir)))
        (setq buffers
              (greed-add-files-to-search-list
               files dir nil greed-dir-recursive-search))))
    (let (list)
      (dolist (elt buffers)
        (unless (member elt list)
          (push elt list)))
      (setq buffers list))
    buffers))

;;;  Minibuffer
(defun greed-dir-read-directory-from-minibuf (default)
  (let ((dir nil))
    (while (not dir)
      (setq dir (read-file-name "Directory: " default nil t))
      ;;(read-file-name "Directory: " nil nil t default)))
      (if (and (file-exists-p dir)
               (file-directory-p  dir))
          (setq dir (file-name-as-directory dir))
        (setq dir (file-name-as-directory (file-name-directory dir)))
        (if (and (file-exists-p dir)
                 (file-directory-p  dir))
            ()
          (message "No such directory %s" dir)
          (sleep-for 1)
          (setq dir nil))))
    dir))

(defun greed-dir-read-project-name-from-minibuf (arg)
  (let (input-name)
    (if (and greed-dir-buffer-project
             greed-dir-use-project
             (or
              (and
               (not arg)
               greed-dir-use-list)
              (and
               arg
               (not greed-dir-use-list))))
        (setq input-name (car greed-dir-buffer-project))
      (setq input-name
            (completing-read
             (concat
              "greed-dir name "
              (when (car greed-dir-list-history)
                (format "(default %s)"
                        (car greed-dir-list-history)))
              " : ")
             (let (list)
               (dolist (elt (append
                             greed-dir-project-list
                             greed-dir-list))
                 (unless (assoc (car elt) list)
                   (push elt list)))
               list)
             nil nil nil 'greed-dir-list-history
             (if (car greed-dir-list-history)
                 (car greed-dir-list-history)
               nil))))
    input-name))

(defun greed-dir-set-sub-directory (dir)
  (let ((lst nil)
        (subdir
         (if (listp dir)
             (eval (nth 0 (car dir)))
           (eval dir))))
    (setq lst (mapcar #'(lambda (file)
                          (if (and (not (string-match "\\.+$" file))
                                   (file-directory-p file))
                              (file-name-nondirectory file)))
                      (directory-files
                       subdir t)))
    (setq lst (delq nil lst))

    (if (and greed-dir-buffer-project
             greed-dir-use-project)
        (setq subdir (car (cdr greed-dir-buffer-project)))
      (setq subdir (concat
                    (file-name-as-directory subdir)
                    (completing-read
                     "greed-dir sub directory : "
                     (mapcar 'list lst)
                     nil t)
                    "/")))
    (if (listp dir)
        (list (cons subdir (nthcdr 1 (car dir))))
      subdir)))

(defun greed-dir-set-project (arg)
  (setq greed-dir-project-name nil)
  (let (input-name name dir)
    (setq input-name (greed-dir-read-project-name-from-minibuf arg))

    (if (assoc input-name greed-dir-project-list)
        (setq name (nth 1 (assoc input-name greed-dir-project-list)))
      (setq name input-name))
    (cond
     ((assoc name greed-dir-list)
      ;; default directory
      (setq dir
            (if (listp (nth 1 (assoc name greed-dir-list)))
                (condition-case nil
                    (eval (nth 1 (assoc name greed-dir-list)))
                  (error
                   (nth 1 (assoc name greed-dir-list))))
              (file-name-as-directory
               (eval (nth 1 (assoc name greed-dir-list))))))

      ;; 'sub option
      (if (eq 'sub (nth 3 (assoc name greed-dir-list)))
          (if (and (listp dir)
                   (not (= (length dir) 1)))
              (error "Multiple directory exists!")
            (setq dir
                  (greed-dir-set-sub-directory dir))))

      ;; if buffer-project exists, use it
      (if (and greed-dir-buffer-project
               greed-dir-use-project)
          ()
        (if (eq 'dir (nth 3 (assoc name greed-dir-list)))
            (if (and (listp dir)
                     (not (= (length dir) 1)))
                (error "Multiple directory exists!")
              (if (listp dir)
                  (setq dir
                        (list
                         (cons
                          (greed-dir-read-directory-from-minibuf
                           (eval (car (car dir)))) (cdr (car dir)))))
                (setq dir (greed-dir-read-directory-from-minibuf dir))))))

      ;; set current project
      (setq greed-dir-project-name
            (if (listp dir)
                (cons name dir)
              (cons name (cons dir greed-dir-project-name))))

      ;; mask
      (setq greed-dir-mask-internal (nth 2 (assoc name greed-dir-list))))

     ((assoc input-name greed-dir-project-list)
      (if (and (eq name input-name)
               (string-match "^greed-dir" name))
          (setq name "greed-dir"))
      (if (or (eq 'dir (nth 3 (assoc name greed-dir-list)))
              (eq 'sub (nth 3 (assoc name greed-dir-list)))
              (string= name "greed-dir"))
          (setq dir
                (substring input-name
                           (progn
                             (string-match (concat name "-") input-name)
                             (match-end 0))))
        (setq dir (nth 1 (assoc name greed-dir-project-list))))
      (setq greed-dir-project-name (cons name dir)))
     (t
      (setq greed-dir-list-history (cdr greed-dir-list-history))
      (error "Input correct name!")))
    dir))

(defun greed-dir-read-from-minibuf (arg)
  (let ((dir nil))
    (if (or arg
            greed-dir-use-list)
        (setq dir (greed-dir-set-project arg))
      (setq dir default-directory)
      (setq greed-dir-mask-internal greed-dir-mask)
      (setq dir (greed-dir-read-directory-from-minibuf dir)))
    dir))

;;;  Interactive
(defun greed-dir (dir regexp arg)
  "Show all lines of all buffers containing a match for REGEXP.
The lines are shown in a buffer named *greed*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive (list (greed-dir-read-from-minibuf current-prefix-arg)
                     (greed-regexp-read-from-minibuf)
                     current-prefix-arg))
  (greed-setup)

  (setq greed-last-command 'greed-dir)
  (let* ((list-name (if (car greed-dir-project-name)
                        (car greed-dir-project-name) "greed-dir"))
         (buffers
          (greed-add-directory-to-search-list dir))
         (name
          (list
           (if (and
                (or greed-dir-use-list arg)
                (or
                 (not (or (eq 'dir
                              (nth 3 (assoc list-name greed-dir-list)))
                          (eq 'sub
                              (nth 3 (assoc list-name greed-dir-list)))))
                 (assoc list-name greed-dir-project-list)))
               list-name
             (concat list-name "-"
                     (if (listp dir) (expand-file-name (car (car dir)))
                       (expand-file-name dir))))
           list-name)))
    ;; sort
    (setq buffers (sort buffers greed-buffer-sort-method))

    (if (assoc (car name) greed-dir-project-list)
        (progn
          (let* ((lst (assoc (car name) greed-dir-project-list))
                 (old-buffers (nthcdr 2 lst)))
            (setq greed-dir-project-list (delete lst greed-dir-project-list))
            (setq name
                  (append name
                          (let ((list nil))
                            (dolist (elt (append
                                          old-buffers
                                          buffers))
                              (unless (memq elt list)
                                (push elt list)))
                            list)))))
      (setq name
            (append name
                    buffers)))

    (setq greed-dir-project-list
          (cons
           name
           greed-dir-project-list))

    (if (nth 4 (assoc list-name greed-dir-list))
        (let* ((conf (if (nth 4 (assoc list-name greed-dir-list))
                         (nth 4 (assoc list-name greed-dir-list))
                       nil))
               (greed-split-word (car (cdr conf))))
          (greed-search regexp arg buffers))
      (greed-search regexp arg buffers))))

(defun clean-greed-dir-buffers ()
  (interactive)
  (let (name buffers lst)
    (setq name (completing-read
                (concat
                 "greed-dir name "
                 " : ")
                greed-dir-project-list))

    (setq buffers (nthcdr 2 (assoc name greed-dir-project-list)))
    (setq lst (list
               (nth 1 (assoc name greed-dir-project-list))))
    (setq lst (append (list name) lst))
    (setq lst (append lst buffers))

    (setq greed-dir-project-list (delete lst greed-dir-project-list))
    (while buffers
      (if (and (car buffers)
               (buffer-live-p (car buffers))
               (get-buffer (car buffers))
               (not (buffer-modified-p (car buffers))))
          (kill-buffer (car buffers)))
      (setq buffers (cdr buffers)))))

;;; greed-dired
(defun greed-dired-by-greed (regexp arg)
  (let ((buffers (greed-add-files-to-search-list
                  (funcall 'dired-get-marked-files
                           t nil) default-directory t 'dired))
        (buff nil))
    (greed-search regexp arg buffers)
    (setq greed-last-command 'greed-dired)
    (when greed-kill-dired-buffers
      (while buffers
        (setq buff (car buffers))
        (if (memq buff greed-match-buffers)
            ()
          (if (memq buff greed-buffers-before-greed)
              (delq buff buffers)
            (kill-buffer buff)))
        (setq buffers (cdr buffers))))))

(defun greed-dired-by-mgrep (regexp)
  (let* ((files (funcall 'dired-get-marked-files
                         t nil)))
    (greed-grep-find files
                     (greed-split-string
                      (concat regexp " .") " "))
    (setq greed-last-command 'greed-dired)))

(defun greed-dired (regexp arg)
  "Show all lines of all buffers containing a match for REGEXP.
The lines are shown in a buffer named *greed*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive (list (greed-regexp-read-from-minibuf)
                     current-prefix-arg))
  (greed-setup)
  (setq greed-buffers-before-greed (buffer-list))
  (if arg
      (greed-dired-by-greed regexp arg)
    (greed-dired-by-mgrep regexp)))

(defun greed-quit ()
  (interactive)

  ;; Kill buffers opened by greed-dired
  (let ((buffers greed-match-buffers)
        (buff nil)
        (gred-window (selected-window))
        (gred-buffer (window-buffer (selected-window))))
    (while buffers
      (setq buff (car buffers))
      (when (and (eq greed-last-command 'greed-dired)
                 greed-kill-dired-buffers
                 (buffer-live-p buff)
                 (buffer-name buff))
        (select-window (next-window gred-window))
        (set-window-buffer (selected-window) buff)
        (if (and (buffer-file-name buff)
                 (buffer-modified-p buff)
                 (y-or-n-p (concat "Buffer "
                                   (buffer-name buff)
                                   " modified. Save it? ")))
            (save-buffer)
          (set-buffer-modified-p nil)) ;; mark as not modified
        (display-buffer gred-buffer)
        (select-window gred-window)
        (if (memq buff greed-buffers-before-greed)
            (delq buff buffers)
          (kill-buffer buff)))
      (setq buffers (cdr buffers))))

  (greed-kill-buffer nil)

  (when (buffer-live-p greed-current-buffer)
    (switch-to-buffer greed-current-buffer)
    (when greed-windows-conf
      (set-window-configuration greed-windows-conf)))

  ;; This is needed as "save-excursion" is used in
  ;; "greed-remove-overlays-on-all-buffers", so we have to make sure the point
  ;; in current buffer is already restored before calling
  ;; "greed-remove-overlays-on-all-buffers"
  (when greed-buffer-position
    (goto-char greed-buffer-position)
    (setq greed-buffer-position nil))

  (greed-remove-overlays-on-all-buffers)

  (when greed-edit-remove-overlays
    (greed-edit-remove-overlays)))

;;; greed-buffer-menu
(defun greed-buffer-menu (regexp arg)
  (interactive (list (greed-regexp-read-from-minibuf)
                     current-prefix-arg))
  (setq arg 1)
  (greed-kill-buffer t)
  (setq greed-last-command 'greed-buffer-menu)
  (let ((marked-buffer) (marked-files))
    (goto-char (point-min))
    (while (search-forward "\n>" nil t)
      (setq marked-buffer (Buffer-menu-buffer t))
      (setq marked-files (cons marked-buffer marked-files)))
    (greed-search regexp arg marked-files)))

;;; greed-ibuffer
(defun greed-ibuffer (regexp arg)
  (interactive (list (greed-regexp-read-from-minibuf)
                     current-prefix-arg))
  (setq arg 1)
  (greed-kill-buffer t)
  (setq greed-last-command 'greed-buffer-menu)
  (let ((marked-buffers nil))
    (ibuffer-map-marked-lines
     #'(lambda (buf _mark) (push buf marked-buffers)))
    (ibuffer-unmark-all ?\>)
    (greed-search regexp arg marked-buffers)))

;;; greed mode keymap
(defvar greed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'greed-toggle-buffer)
    (define-key map "\C-c\C-f" 'greed-follow-mode)
    (define-key map "\C-c\C-c" 'greed-mode-goto-occurrence)
    (define-key map "\C-m" 'greed-mode-goto-occurrence)
    (define-key map "d" 'greed-kill-line)
    (define-key map "\C-k" 'greed-kill-line)
    (define-key map "\M-d" 'greed-mode-kill-file)
    (define-key map "/" 'greed-mode-undo)
    (define-key map "q" 'greed-quit)
    (define-key map "n" 'greed-next)
    (define-key map "p" 'greed-prev)
    (define-key map "j" 'greed-next)
    (define-key map "k" 'greed-prev)
    (define-key map "\M-k" 'greed-next)
    (define-key map "\M-i" 'greed-prev)
    (define-key map "\M-l" 'greed-next)
    (define-key map "\M-j" 'greed-prev)
    (define-key map '[wheel-down] 'greed-next)
    (define-key map '[wheel-up] 'greed-prev)
    (define-key map "s" 'greed-narrow-down)
    (define-key map "u" 'greed-search-undo)
    (define-key map "g" 'greed-search-update)
    (define-key map '[down] 'greed-next)
    (define-key map '[up] 'greed-prev)
    (define-key map '[tab] 'greed-show)
    (define-key map "t" 'greed-toggle-view)
    (define-key map "b" 'greed-file-scroll-down)
    (define-key map " " 'greed-file-scroll-up)
    (define-key map "\M-v" 'greed-scroll-down)
    (define-key map "\C-v" 'greed-scroll-up)
    (define-key map "h" 'greed-next-file)
    (define-key map "l" 'greed-prev-file)
    (define-key map "\M-n" 'greed-next-file)
    (define-key map "\M-p" 'greed-prev-file)
    (define-key map '[M-wheel-down] 'greed-next-file)
    (define-key map '[M-wheel-up] 'greed-prev-file)

    (define-key map '[down-mouse-1] 'greed-mouse-select1)

    (define-key map "<" 'greed-file-beginning-of-buffer)
    (define-key map ">" 'greed-file-end-of-buffer)

    (define-key map "r" 'greed-edit-mode-in)
    (define-key map "\C-x\C-q" 'greed-edit-mode-in)
    (define-key map "\C-c\C-i" 'greed-edit-mode-in)

    map)
  "Keymap for `greed-mode'.")

;;;  Utility
(defun greed-outline-level ()
  (if (looking-at "\\(^Buffer: \\)")
      0
    (if (looking-at "\\(^[ ]*[0-9]+ \\)")
        1)))

;;;  Re-search function
(defun greed-narrow-down-get-targets (target-regexp target-type)
  (let ((case-fold-search t)
        (targets nil) target-name)
    (with-current-buffer (get-buffer "*greed*")
      (goto-char (point-min))
      (while (re-search-forward target-regexp nil t)
        (setq target-name (buffer-substring-no-properties
                           (match-beginning 1)
                           (match-end 1)))
        (if (equal target-type 'file)
            (setq targets (cons target-name targets))
          (if (get-buffer target-name)
              (setq targets (cons
                             (get-buffer target-name) targets)))))
      targets)))

(defun greed-narrow-down-get-buffers()
  (greed-narrow-down-get-targets greed-buffer-heading-regexp 'buffer))

(defun greed-narrow-down-get-files()
  (greed-narrow-down-get-targets greed-grep-buffer-heading-regexp 'file))

;;;  Functions
(defun greed-narrow-down (regexp arg)
  "Show all lines of all buffers containing a match for REGEXP.
The lines are shown in a buffer named *greed*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive (list (greed-regexp-read-from-minibuf)
                     current-prefix-arg))

  (setq greed-buffer (current-buffer))
  (setq greed-last-command 'greed-narrow-down)
  (if (equal major-mode 'greed-grep-mode)
      (let ((files (reverse (greed-narrow-down-get-files))))
        (greed-setup)
        (greed-search-files regexp files))
    (let ((buffers (reverse (greed-narrow-down-get-buffers))))
      (greed-setup)
      (greed-search regexp arg buffers))))

(defun greed-mode-goto-occurrence ()
  "Go to the line this occurrence was found in, in the buffer it was found in."
  (interactive)
  ;;    (if (not (and greed-view-other-window
  ;;            greed-view-other-window-nobuf))
  ;;        (greed-view-file)
  (setq greed-buffer (current-buffer))
  (if (not (eq major-mode 'greed-mode))
      (error "This is no greed buffer")
    (let ((beg nil)
          (line nil)
          (lineno nil)
          (dstbuf nil))
      (greed-remove-overlays-on-all-buffers)
      (save-excursion
        (beginning-of-line 1)
        (setq beg (point))
        (end-of-line 1)
        (setq line (buffer-substring beg (point)))
        (if (or (string-match "^[ ]*[0-9]* " line)
                (string-match "^[-+ ]*Buffer: " line))
            (progn
              (if (string-match "^[-+ ]*Buffer: " line)
                  (setq lineno nil)
                (setq lineno (car (read-from-string line))))
              (if (re-search-backward "^[-+ ]*Buffer: ")
                  (progn
                    (search-forward "Buffer: ")
                    (setq beg (point))
                    (search-forward " File:")
                    (setq line (buffer-substring beg (- (point) 6)))
                    (setq dstbuf (get-buffer line))
                    (if (not dstbuf)
                        (message "buffer: <%s> doesn't exist anymore" line)))
                (error "What did you do with the header?!")))
          (error "This is no occurrence line!")))
      (if dstbuf
          (progn
            (if lineno
                (message "selecting <%s> line %d" line lineno)
              (message "selecting <%s>" line))
            (pop-to-buffer dstbuf)
            (when lineno
              (greed-goto-line lineno)
              (run-hooks 'greed-goto-occurrence-hook)))))))

(defun greed-toggle-buffer ()
  (interactive))

(defun greed-mouse-select1 (e)
  (interactive "e")
  (mouse-set-point e)
  (save-excursion
    (beginning-of-line)
    (greed-next 0)))

(defun greed-mouse-goto-occurrence (e)
  (interactive "e")
  (mouse-set-point e)
  (save-excursion
    (beginning-of-line)
    (greed-mode-goto-occurrence)))

(defun greed-show ()
  "Show the current candidate in the buffer in the other window"
  (interactive)
  (setq greed-buffer (current-buffer))
  (beginning-of-line)
  (greed-get-info)
  (if (and greed-view-other-window
           greed-view-other-window-nobuf)
      (greed-view-file)))

(defun greed-next (arg)
  (interactive "p")
  (setq greed-buffer (current-buffer))
  (if arg
      (forward-line arg)
    (forward-line 1))
  (beginning-of-line)

  (when (re-search-forward greed-line-number-regexp nil t)
    (save-restriction
      (narrow-to-region (point) (line-end-position))
      (re-search-forward (car greed-regexp-list) nil t)))
  (greed-get-info)
  (if (and greed-view-other-window
           greed-view-other-window-nobuf
           greed-follow-mode)
      (greed-view-file)))

(defun greed-prev (arg)
  (interactive "p")
  (setq greed-buffer (current-buffer))
  (if arg
      (forward-line (* -1 arg))
    (forward-line -1))
  (end-of-line)
  (if (re-search-backward greed-line-number-regexp nil t)
      (save-restriction
        (re-search-forward greed-line-number-regexp nil t)
        (narrow-to-region (point) (line-end-position))
        (re-search-forward (car greed-regexp-list) nil t))
    (beginning-of-line))
  (greed-get-info)
  (if (and greed-view-other-window
           greed-view-other-window-nobuf
           greed-follow-mode)
      (greed-view-file)))

(defun greed-file-scroll-up ()
  (interactive)
  (setq greed-buffer (current-buffer))
  (greed-get-info)
  (if (and greed-view-other-window
           greed-view-other-window-nobuf)
      (greed-scroll-file nil)))

(defun greed-file-scroll-down ()
  (interactive)
  (setq greed-buffer (current-buffer))
  (greed-get-info)
  (if (and greed-view-other-window
           greed-view-other-window-nobuf)
      (greed-scroll-file t)))

(defun greed-file-beginning-of-buffer ()
  (interactive)
  (setq greed-buffer (current-buffer))
  (greed-get-info)
  (if (and greed-view-other-window
           greed-view-other-window-nobuf)
      (greed-internal-beginning-of-buffer nil)))

(defun greed-file-end-of-buffer ()
  (interactive)
  (setq greed-buffer (current-buffer))
  (greed-get-info)
  (if (and greed-view-other-window
           greed-view-other-window-nobuf)
      (greed-internal-beginning-of-buffer t)))

(defun greed-scroll-up ()
  (interactive)
  (scroll-up)
  (if (boundp 'forward-visible-line)
      (forward-visible-line -1)
    (forward-line -1))
  (end-of-line)
  (greed-next 1))

(defun greed-scroll-down ()
  (interactive)
  (scroll-down)
  (if (boundp 'forward-visible-line)
      (forward-visible-line 1)
    (forward-line 1))
  (beginning-of-line)
  (greed-prev 1))

(defun greed-next-file ()
  (interactive)
  (if (re-search-forward "^[-+ ]*Buffer: " nil t)
      (greed-next 1)
    (goto-char (point-min))
    (greed-next 1)))

(defun greed-prev-file ()
  (interactive)
  (if (re-search-backward "^[-+ ]*Buffer: " nil t 2)
      (greed-next 1)
    (goto-char (point-max))
    (if (re-search-backward "^[-+ ]*Buffer: " nil t)
        (greed-next 1))))

(defun greed-mode-kill-file-internal ()
  (let ((start-pt (progn
                    (re-search-backward "^[-+ ]*Buffer: " nil t)
                    (line-beginning-position)))
        (end-pt nil))

    (forward-line 1)
    (if (re-search-forward greed-buffer-heading-regexp nil t)
        (setq end-pt (line-beginning-position))
      (setq end-pt (point-max)))
    (delete-region start-pt end-pt)))

(defun greed-mode-kill-line-internal ()
  (delete-region (line-beginning-position)
                 (+ (line-end-position) 1))

  (greed-get-info)
  (when (= 0 greed-buffer-match-count)
    (greed-mode-kill-file)))

(defun greed-kill-line ()
  (interactive)
  (let* ((str
          (regexp-quote
           (progn
             (save-excursion
               (beginning-of-line)
               (re-search-forward "[^ ]" (line-end-position) t)
               (buffer-substring-no-properties
                (- (point) 1) (line-end-position)))))))

    (goto-char (point-min))
    (if (string-match "^[+-]" str)
        (setq str (substring str 2)))
    (let ((buffer-read-only nil)
          (inhibit-read-only nil))
      (when (re-search-forward str nil t)
        (line-beginning-position)
        (cond
         ((string-match "^[ ]*$" str)
          ())
         ((string-match greed-buffer-heading-regexp str)
          (greed-mode-kill-file-internal))

         ((string-match greed-line-number-regexp str)
          (greed-mode-kill-line-internal))
         (t
          ()))))))

(defun greed-mode-kill-file ()
  (interactive)
  (let* ((str
          (regexp-quote
           (progn
             (save-excursion
               (end-of-line)
               (re-search-backward "^[-+ ]*Buffer: " nil t)
               (buffer-substring-no-properties
                (point) (line-end-position)))))))

    (goto-char (point-min))
    (if (string-match "^[+-]" str)
        (setq str (substring str 2)))
    (let ((buffer-read-only nil)
          (inhibit-read-only nil))
      (when (re-search-forward (regexp-quote str) nil t)
        (line-beginning-position)
        (greed-mode-kill-file-internal)))))

(defun greed-mode-undo ()
  (interactive)
  (let* ((str
          (regexp-quote
           (progn
             (save-excursion
               (end-of-line)
               (re-search-backward "^[-+ ]*Buffer: " nil t)
               (buffer-substring-no-properties
                (point) (line-end-position)))))))

    (if (string-match "^[+-]" str)
        (setq str (substring str 2)))
    (let ((buffer-read-only nil)
          (inhibit-read-only nil))
      (condition-case nil
          (undo)
        (error nil))
      (goto-char (point-min))
      (re-search-forward (regexp-quote str) nil t))))

(defun greed-flush-lines ()
  (interactive)
  (let ((str
         (progn
           (save-excursion
             (if (and (not (and (boundp 'running-xemacs) running-xemacs))
                      transient-mark-mode mark-active)
                 (goto-char (region-beginning)))
             (beginning-of-line)
             (re-search-forward "[^ ]" nil t)
             (regexp-quote
              (buffer-substring-no-properties
               (- (point) 1) (line-end-position))))))
        (rend-str (if (and (not (and (boundp 'running-xemacs) running-xemacs))
                           transient-mark-mode mark-active)
                      (progn
                        (save-excursion
                          (goto-char (region-end))
                          (beginning-of-line)
                          (re-search-forward "[^ ]" (line-end-position) t)
                          (regexp-quote
                           (buffer-substring-no-properties
                            (- (point) 1) (line-end-position)))))
                    nil))
        (regexp
         (read-from-minibuffer
          "Flush lines (containing match for regexp): " nil nil nil
          'regexp-history nil t)))

    (goto-char (point-min))
    (if (string-match "^[+-]" str)
        (setq str (substring str 2)))
    (if (and rend-str
             (string-match "^[+-]" rend-str))
        (setq rend-str (substring rend-str 2)))

    (re-search-forward (regexp-quote str) nil t)
    (beginning-of-line)
    (let (rstart rend
                 (buffer-read-only nil)
                 (inhibit-read-only nil))
      (setq rstart (point))
      (if rend-str
          (setq rend (copy-marker
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward (regexp-quote rend-str) nil t)
                        (end-of-line)
                        (point))))
        (setq rend (point-max-marker)))
      (goto-char rstart)
      (let ((case-fold-search case-fold-search))
        (save-excursion
          (while (and (< (point) rend)
                      (re-search-forward regexp rend t))
            (goto-char (line-beginning-position))
            (unless (re-search-forward
                     greed-buffer-heading-regexp (line-end-position) t)
              (line-beginning-position)
              (greed-mode-kill-line-internal))))))))

(defun greed-keep-lines ()
  (interactive)
  (let ((str
         (progn
           (save-excursion
             (if (and (not (and (boundp 'running-xemacs) running-xemacs))
                      transient-mark-mode mark-active)
                 (goto-char (region-beginning)))
             (beginning-of-line)
             (re-search-forward "[^ ]" nil t)
             (regexp-quote
              (buffer-substring-no-properties
               (- (point) 1) (line-end-position))))))
        (rend-str (if (and (not (and (boundp 'running-xemacs) running-xemacs))
                           transient-mark-mode mark-active)
                      (progn
                        (save-excursion
                          (goto-char (region-end))
                          (beginning-of-line)
                          (re-search-forward "[^ ]" (line-end-position) t)
                          (regexp-quote
                           (buffer-substring-no-properties
                            (- (point) 1) (line-end-position)))))
                    nil))
        (regexp (read-from-minibuffer
                 "Flush lines (containing match for regexp): " nil nil nil
                 'regexp-history nil t)))

    (goto-char (point-min))
    (if (string-match "^[+-]" str)
        (setq str (substring str 2)))
    (if (and rend-str
             (string-match "^[+-]" rend-str))
        (setq rend-str (substring rend-str 2)))

    (re-search-forward (regexp-quote str) nil t)
    (beginning-of-line)
    (let (rstart rend
                 (buffer-read-only nil)
                 (inhibit-read-only nil))
      (setq rstart (point))
      (if rend-str
          (setq rend (copy-marker
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward (regexp-quote rend-str) nil t)
                        (end-of-line)
                        (point))))
        (setq rend (point-max-marker)))
      (goto-char rstart)
      (let ((case-fold-search case-fold-search))
        (save-excursion
          (while (< (point) rend)
            (goto-char (beginning-of-line))
            (unless (or (eq
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)) "")
                        (save-excursion
                          (re-search-forward regexp (line-end-position) t)))
              (unless
                  (re-search-forward
                   greed-buffer-heading-regexp (line-end-position) t)
                (beginning-of-line)
                (greed-mode-kill-line-internal)
                (forward-line -1)))
            (forward-line 1)))))))

(defun greed-toggle-view ()
  (interactive)
  (setq greed-view-other-window (not greed-view-other-window)))

;;;  Body
(defun greed-mode ()
  "Major mode for output from \\[greed].
Move point to one of the occurrences in this buffer,
then use \\[greed-mode-goto-occurrence] to move to the buffer and
line where it was found.
\\{occur-mode-map}"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'greed-mode)
  (setq mode-name "greed")
  (use-local-map greed-mode-map)
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (add-to-invisibility-spec '(greed . t))
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "\\(^Buffer: \\|^[ ]*[0-9]+ \\)")
  (make-local-variable 'outline-level)
  (setq outline-level 'greed-outline-level)
  (run-hooks 'greed-mode-hook))

(defun greed-grep-mode ()
  "Major mode for output from \\[greed-grep].
Move point to one of the occurrences in this buffer,
then use \\[greed-grep-goto] to move to the buffer and
line where it was found.
\\{occur-mode-map}"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'greed-grep-mode)
  (setq mode-name "greed-grep")
  (use-local-map greed-mode-map)
  ;; Commented out by <WL> (who should we disable greed-toggle-view here?)
  ;; (local-unset-key "t")
  (local-set-key "\C-m" 'greed-grep-goto)
  (local-set-key "\C-c\C-c" 'greed-grep-goto)
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (add-to-invisibility-spec '(greed . t))

  (turn-on-font-lock)

  (make-local-variable 'outline-regexp)
  (setq outline-regexp "\\(^Buffer: File (grep): \\)")
  (make-local-variable 'outline-level)
  (setq outline-level 'greed-outline-level)
  (run-hooks 'greed-mode-hook))


;;; greed-edit

(defun greed-mode-change-face (beg end _leng-before)
  (interactive)
  (let ((ov (overlays-in beg end))
        (edit-ov nil)
        (exist-ovelays nil))
    (if greed-edit-change-face-flg
        (progn
          (save-excursion
            (while ov
              (if (overlay-get (car ov) 'greed-edit)
                  (setq exist-ovelays t))
              (setq ov (cdr ov)))
            (if exist-ovelays
                ()
              (setq edit-ov (make-overlay (line-beginning-position)
                                          (line-end-position)))
              (overlay-put edit-ov 'greed-edit t)
              (overlay-put edit-ov 'face 'greed-edit-face)
              (overlay-put edit-ov 'priority 0)
              (setq greed-edit-overlays (cons edit-ov greed-edit-overlays))
              ))))))

(defvar greed-edit-buf "")
(defvar greed-edit-line "")
(defvar greed-edit-text "")

(defun greed-edit-get-info ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-backward "^[-+ ]*Buffer:" nil t)
      (if (re-search-forward "File (grep)" (line-end-position) t)
          (progn
            (if (re-search-forward ":[ ]+\\([^\r\n]+\\)$" (line-end-position) t)
                (setq greed-edit-buf
                      (cons "grep"
                            (buffer-substring-no-properties
                             (match-beginning 1)
                             (match-end 1))
                            ))
              (setq greed-edit-buf "grep")))
        (beginning-of-line)
        (if (re-search-forward
             "^[-+ ]*Buffer: \\([^\r\n]*\\) File:" (line-end-position) t)
            (setq greed-edit-buf (buffer-substring-no-properties
                                   (match-beginning 1)
                                   (match-end 1)))))))
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^[ ]*\\([0-9]+\\) \\([^\n]+$\\)"
                           (line-end-position) t)
        (progn
          (setq greed-edit-line
                (string-to-number (buffer-substring-no-properties
                                   (match-beginning 1)
                                   (match-end 1))))
          (setq greed-edit-text (buffer-substring-no-properties
                                  (match-beginning 2)
                                  (match-end 2)))))))

(defun greed-edit-change-file ()
  "*The changes on the greed buffer apply to the file"
  (if buffer-read-only
      (if (and greed-query-when-buffer-read-only
               (buffer-file-name)
               ;;(file-writable-p (buffer-file-name))
               (y-or-n-p (format "Make buffer %s writable?"
                                 (current-buffer)))
               (or (read-only-mode -1) t)
               (not buffer-read-only))
          (greed-edit-change-file)
        (display-warning             ; (Not defined in old Emacses! *)
         'greed-edit
         (format "Buffer read only: %s" (current-buffer)))
        nil)
    (greed-goto-line greed-edit-line)
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert greed-edit-text)
    t))

(defun greed-edit-put-color-file ()
  "*Highlight the changed line of the file"
  (let ((fileov))
    (setq fileov (make-overlay
          (line-beginning-position)
          (line-end-position)))
    (overlay-put fileov 'face 'greed-edit-file-face)
    (overlay-put fileov 'priority 0)
    (setq greed-edit-file-overlays (cons fileov greed-edit-file-overlays))
    ))

(defun greed-edit-put-face (face)
  (let ((ov))
    (beginning-of-line)
    (re-search-forward "^[ ]*[0-9]+ " nil t)
    (setq ov (make-overlay (point) (line-end-position)))
    (overlay-put ov 'greed-edit t)
    (overlay-put ov 'face face)
    (overlay-put ov 'priority 0)
    (setq greed-edit-result-overlays (cons ov greed-edit-result-overlays))))

(defun greed-edit-remove-overlays ()
  "Remove all overlays in all buffers."
  (interactive)
  (save-current-buffer
    (let (buf (buflist (buffer-list)))
      (while buflist
        (setq buf (car buflist))
        (setq buflist (cdr buflist))
        (when (and buf
                   (buffer-live-p buf))
          (set-buffer buf)
          (while greed-edit-file-overlays
            (delete-overlay (car greed-edit-file-overlays))
            (setq greed-edit-file-overlays
                  (cdr greed-edit-file-overlays))))))))

(defun greed-edit-remove-overlays-after-save-buffer ()
  (when greed-edit-remove-overlays-after-save-buffer
    (greed-edit-remove-overlays)))

(add-hook 'after-save-hook
          'greed-edit-remove-overlays-after-save-buffer nil t)

(defun greed-edit-finish-edit ()
  "*The changes on the grep buffer apply to the file"
  (interactive)
  (let ((ov) beg cbuf)
    (setq cbuf (current-buffer))
    (while greed-edit-overlays
      (setq ov (car greed-edit-overlays))
      (setq greed-edit-overlays (cdr greed-edit-overlays))
      (setq beg (overlay-start ov))
      (when beg
        (goto-char beg)
        (greed-edit-get-info)

        (if (and
             (listp greed-edit-buf)
             (string= (car greed-edit-buf) "grep"))
            (set-buffer (find-file-noselect (cdr greed-edit-buf)))
          (set-buffer greed-edit-buf))
        ;; <WL: fixed a bug here>
        (if (greed-edit-change-file)
            ;; File is changed. t: success
            (progn
              (when greed-edit-highlight-edited-text
                (greed-edit-put-color-file)) ;; Highlight changed text
              (set-buffer cbuf)
              (greed-edit-put-face 'greed-edit-done-face))
         (set-buffer cbuf)
         (greed-edit-put-face 'greed-edit-reject-face))
        ;; Return previous buffer
        (set-buffer cbuf)
        (delete-overlay ov)
        )))
  (greed-edit-reset-key))

(defun greed-edit-remove-change (beg end)
  (interactive "r")
  (let ((ov (overlays-in beg end)))
    (while ov
      (if (overlay-get (car ov) 'greed-edit)
          (delete-overlay (car ov)))
      (setq ov (cdr ov))))
  (setq mark-active nil))

(defun greed-edit-mode-in ()
  (interactive)
  (greed-edit-mode)
  (force-mode-line-update)
  (setq greed-edit-old-content
        (buffer-substring (point-min) (point-max)))
  (setq greed-edit-change-face-flg nil)
  (setq buffer-read-only nil)
  (greed-edit-set-readonly-area)
  (setq greed-edit-change-face-flg t)

  (setq line-move-ignore-invisible nil)
  ;; Cause use of ellipses for invisible text.
  (setq buffer-invisibility-spec nil)

  (add-hook 'after-change-functions 'greed-mode-change-face nil t))

(defun current-line ()
  "Return the vertical position of point..."
  (1+ (count-lines 1 (point))))

(defun max-line ()
  "Return the vertical position of point..."
  (save-excursion
    (goto-char (point-max))
    (current-line)))

(defun greed-edit-kill-all-change ()
  (interactive)
  (let (pos)
    (setq pos (current-line))
    (greed-edit-remove-change (point-min) (point-max))
    (greed-edit-reset-key)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert greed-edit-old-content)
      (greed-buffer-color))
    (if (> pos (max-line))
        (greed-goto-line (max-line))
      (greed-goto-line pos))))

(defun greed-edit-set-readonly-area ()
  (let ((inhibit-read-only t) beg end)
    (save-excursion
      (goto-char (point-min))
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      (put-text-property beg end 'read-only t)
      (put-text-property beg end 'front-sticky '(read-only))
      (while (re-search-forward "\\(^[-+ ]*Buffer: [^\n]*File[^\n]+$\\)" nil t)
        (put-text-property (match-beginning 0)
                               (match-end 0) 'read-only t)
        (put-text-property (match-beginning 0)
                           (match-end 0) 'front-sticky '(read-only)))
      (goto-char (point-min))
      (while (re-search-forward "\\(^[ ]*[0-9]+\\)" nil t)
        (put-text-property (match-beginning 1)
                           (match-end 1) 'read-only t)
        (put-text-property (match-beginning 0)
                           (match-end 0) 'front-sticky '(read-only)))
      (goto-char (point-min))
      (while (re-search-forward "^\\([\r\n]+\\)" nil t)
        (put-text-property (match-beginning 1)
                           (match-end 1) 'read-only t)
        (put-text-property (match-beginning 0)
                           (match-end 0) 'front-sticky '(read-only)))
      )))

(defun greed-edit-reset-key ()
  (interactive)
  (setq buffer-read-only t)
  (cond
   ((string= mode-name "greedg-edit")
    (greed-grep-mode))
   (t
    (greed-mode)))
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (add-to-invisibility-spec '(greed . t))
  (force-mode-line-update)
  )

;; greed-mode
(defvar greed-edit-mode-map ())
(defun greed-edit-set-key ()
  (define-key greed-edit-mode-map '[down] 'greed-next)
  (define-key greed-edit-mode-map '[up] 'greed-prev)
  (define-key greed-edit-mode-map "\C-c\C-r"
    'greed-edit-remove-change)
  (define-key greed-edit-mode-map "\C-c\C-f"
    'greed-edit-finish-edit)
  (define-key greed-edit-mode-map "\C-x\C-s"
    'greed-edit-finish-edit)
  (define-key greed-edit-mode-map "\C-c\C-c"
    'greed-edit-finish-edit)
  (define-key greed-edit-mode-map "\C-c\C-k"
    'greed-edit-kill-all-change)
  (define-key greed-edit-mode-map "\C-xk"
    'greed-edit-kill-all-change)
  (define-key greed-edit-mode-map "\C-ck"
    'greed-edit-kill-all-change)
  (define-key greed-edit-mode-map "\C-c\C-u"
    'greed-edit-kill-all-change)
  )

(if greed-edit-mode-map
    ()
  (setq greed-edit-mode-map (make-sparse-keymap))
  (greed-edit-set-key)
  )

(defun greed-edit-mode ()
  "Major mode"
  (let ((mode
         (cond
          ((eq major-mode 'greed-grep-mode)
           'grep)
          (t nil))))
    (kill-all-local-variables)
    (use-local-map greed-edit-mode-map)
    (setq major-mode 'greed-edit-mode)
    (cond
     ((eq mode 'grep)
      (setq mode-name "greedg-edit"))
     (t
      (setq mode-name "greed-edit")))
    (greed-edit-set-key)))

;; advice for query-replace
(defun greed-edit-add-skip-in-replace (command)
  "Advice COMMAND to skip matches while they have read-only properties.
This is useful to avoid \"read-only\" errors in search and replace
commands.  This advice only has effect in greed-edit mode."
  (eval
   `(defadvice ,command (around greed-edit-discard-read-only activate)
      ,(format "Make %s to work better with greed-edit,\n%s."  command
               "skipping read-only matches when invoked without argument")
      ad-do-it
      (if (eq major-mode 'greed-edit-mode)
          (while (and ad-return-value
                      (text-property-any
                       (max 1 (1- (match-beginning 0))) (match-end 0)
                       'read-only t))
            ad-do-it))
      ad-return-value)))

(defun greed-edit-replace-advice (command)
  "Advice COMMAND to skip matches while they have read-only properties.
This is useful to avoid \"read-only\" errors in search and replace
commands.  This advice only has effect in greed-edit mode."
  (eval
   `(defadvice ,command (around greed-edit-grok-read-only activate)
      ,(format "Make %s to work better with greed-edit,\n%s."  command
               "skipping read-only matches when invoked without argument")
      (if (eq major-mode 'greed-edit-mode)
          (progn
            (greed-edit-add-skip-in-replace 'search-forward)
            (greed-edit-add-skip-in-replace 're-search-forward)
            (unwind-protect
                ad-do-it
              (progn
                (ad-remove-advice 'search-forward
                                  'around 'greed-edit-discard-read-only)
                (ad-remove-advice 're-search-forward
                                  'around 'greed-edit-discard-read-only)
                (ad-update 'search-forward)
                (ad-update 're-search-forward))))
        ad-do-it)
      ad-return-value)))

(mapc 'greed-edit-replace-advice
        '(query-replace query-replace-regexp replace-string))

(provide 'greed)

;;; greed.el ends here
