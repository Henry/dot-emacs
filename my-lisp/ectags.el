;;; ectags.el --- Select from multiple exuberant-ctags

;; Copyright (C) 2007  Scott Frazer and (C) 2008 John Connors

;; Author: John Connors <johnc@yagc.ndo.co.uk>
;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: John Connors <johnc@yagc.ndo.co.uk>
;;
;; Keywords: exuberant-ctags ectags tag select

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Reworking of etags-select to work with exuberant-ctags.  ectags-tag-directory
;; will generate a tag file in the current directory with the specified language
;; using exuberant-ctags extended tag format which contains useful information
;; about the current tag such as the signature, it's parent class, which class
;; it is a member of.  The tag file needs to be in a specific format, hence the
;; ectags-tag-directory-command.  ectags-visit-tags-table is used to load in a
;; tag table.

;; Open a buffer with file/lines of exact-match tags shown.  Select one by
;; going to a line and pressing return.  pop-tag-mark still works with this
;; code.

;;; TODO

;; TODO : do sthing about ginormous tag files
;; TODO : .. use abbreviated info
;; TODO : .. use gzipped tag files
;; TODO : use search for locating tag rather than line #
;; TODO : Additional highlighting for select buffer
;; TODO : include line matched when searching for references

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

;;;###autoload
(defgroup ectags nil
  "Exuberant Ctags Support for Emacs"
  :version "23"
  :group 'tools)

;;;###autoload
(defcustom ectags-command "ectags"
  "Name of the exuberant ctags executable on your system"
  :type 'string
  :group 'ectags)

;;;###autoload
(defcustom ectags-config-file "~/.ectags"
  "Name of the exuberant-ctags configuration file."
  :type 'string
  :group 'ectags)

;;;###autoload
(defcustom ectags-language-file-suffix-alist
  '(( "asp"    .  ( "*.asp" "*.asa" ))
    ( "awk"    .  ( "*.awk" "*.gawk" "*.mawk"))
    ( "c"      .  ( "*.c" "*.h" ))
    ( "c++"    .  ( "*.c++" "*.cc" "*.cp" "*.cpp"  "*.cxx" "*.h" "*.h++" "*.hh"
                    "*.hp" "*.hpp" "*.hxx" "*.c" "*.C" "*.h" "*.H"))
    ( "c#"     .  ( "*.cs" ))
    ( "java"   .  ( "*.java " ))
    ( "lisp"   .  (  "*.cl" "*.clisp" "*.el" "*.l" "*.lisp" "*.lsp" "*.ml"))
    ( "python" .  ( "*.py" "*.python" ))
    ( "SQL"    .  (  "*.sql" ))
    ( "Tcl"    .  ( "*.tcl" "*.tk" "*.wish" "*.itcl" )))
  "Association list defining file masks for languages"
  :type 'alist
  :group 'ectags)

;;;###autoload
(defcustom ectags-system-tag-table-list nil
  "List of tags tables that include system headers"
  :type 'list
  :group 'ectags)

;;;###autoload
(defcustom ectags-api-files
  '(( "wx"    .  "/usr/local/include/wx" )
    ( "gtk"    .  "/usr/include/gtk-2.0" )
    ( "glib"      . "/usr/include/glib-2.0" ))
  "Association list mapping apis to directories"
  :type 'alist
  :group 'ectags)

;;;###autoload
(defcustom ectags-select-mode-hook nil
  "*List of functions to call on entry to ectags-select-mode mode."
  :group 'ectags
  :type 'hook)

;;;###autoload
(defcustom ectags-window-split-function 'split-window-vertically
  "*Function to call to split the window when displaying tags."
  :group 'ectags
  :type 'symbol)

;;;###autoload
(defcustom ectags-select-highlight-tag-after-jump t
  "*If non-nil, temporarily highlight the tag after you jump to it."
  :group 'ectags-select-mode
  :type 'boolean)

;;;###autoload
(defcustom ectags-select-highlight-delay 1.0
  "*How long to highlight the tag."
  :group 'ectags-select-mode
  :type 'number)

;;;###autoload
(defface ectags-select-highlight-tag-face
  '((t (:foreground "white" :background "cadetblue4" :bold t)))
  "Font Lock mode face used to highlight tags."
  :group 'ectags-select-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar ectags-matches nil
  "List of candiate tag matches")

(defvar ectags-regexp nil
  "Holds regexp currently being sought in tags")

(defvar ectags-max-candidates 7
  "How many candidates to select between")

(defvar ectags-case-sensitive t
  "Is the tag matching case sensitive?")

(defvar ectags-obarray nil
  "Obarray used for ectags completions.")

(defvar ectags-select-buffer-name "*ectags-select*"
  "ectags-select buffer name.")

(defvar ectags-reference-buffer-name "*ectag References*"
  "ectags-reference buffer-name")

(defvar ectags-select-mode-font-lock-keywords nil
  "ectags-select font-lock-keywords.")

(defvar ectags-select-source-buffer nil
  "ectags-select source buffer tag was found from.")

(defvar ectags-reference-source-buffer nil
  "ectags-reference source buffer tag was found from.")

(defvar ectags-select-opened-window nil
  "ectags-select opened a select window.")

(defvar ectags-reference-opened-window nil
  "ectags-referecnce opened a reference window.")

(defvar ectags-scan-marks nil
  "Holds markers where matches found.")

(defvar ectags-current-overlay nil
  "Currently displayed tag highlight overlay.")
(make-variable-buffer-local 'ectags-current-overlay)

(defconst ectags-select-non-tag-regexp "\\(^ \\|In:\\|Finding tag:\\)"
  "ectags-select non-tag regex.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

;; Klaus Berndl <klaus.berndl@sdm.de>: we have to take account that GNU Emacs
;; > 21.3 has changed its split-string function! For the new split-string is
;; (cdr (split-string ...)) not nil (at least in our context below), for GNU
;; Emacs <= 21.3 nil!
(defun ectags-left-trim (str)
  "Return a string stripped of all leading whitespaces of STR."
  (let ((split-result (split-string str "^[\n\t ]*")))
    (or (or (and (cdr split-result) ;; GNU Emacs > 21.3
                 (car (cdr split-result)))
            (car split-result))
        "")))

(defun ectags-right-trim (str)
  "Return a string stripped of all trailing whitespaces of STR."
  (or (car (split-string str "[\n\t ]*$")) ""))

(defun ectags-trim (str)
  "Applies `ectags-right-trim' and `ectags-left-trim' to STR."
  (ectags-left-trim (ectags-right-trim str)))

(defun insert-with-face (face string)
  "Insert STRING with face FACE."
  (let ((pt (point)))
    (insert string)
    (put-text-property pt (point) 'face face)))

(defun insert-with-faces (&rest args)
  "Insert ARGS with optional faces provided as the previous argument."
  (while args
    (if (facep (car args))
        (insert-with-face (pop args) (pop args))
      (insert (pop args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Creating tag files

(defun ectags-make-suffix-clauses (languages)
  (mapcar (lambda (l)
            (mapcar (lambda (s)
                      (concat  " -iname \"" s "\""))
                    (cdr (assoc-string l ectags-language-file-suffix-alist))))
          (split-string languages)))

(defun ectags-make-shell-command-prefix (directory)
  (concat "find " (expand-file-name directory)))

(defun ectags-make-tag-file-name (directory)
  (expand-file-name (concat directory "/tags")))

(defun ectag-directory-command (directory languages)
  "Produce a command needed to scan the given directory for files
   of the given language and produce tags"
  (let*
      ((suffix-clauses
        (car (ectags-make-suffix-clauses languages)))
       (shell-command-prefix
        (ectags-make-shell-command-prefix directory))
       (shell-command-suffix
        (concat " | " ectags-command
                " -o "  (ectags-make-tag-file-name directory)
                " --options=" (expand-file-name ectags-config-file)
                " --verbose --excmd=n --extra=+fq --fields=+afiKlmnsSzt"
                " --file-scope=no -L -")))
    (concat shell-command-prefix
            (car suffix-clauses)
            (apply 'concat
                   (mapcar (lambda (s)
                             (concat " -o" s))
                           (cdr suffix-clauses)))
            shell-command-suffix)))

;;;###autoload
(defun ectags-tag-directory ()
  "Prompt for a directory and a langage and create a tag file."
  (interactive)
  ;; prompt for directory
  (let ((tag-directory
         (read-directory-name "Directory to tag? " default-directory))
        (tag-languages
         (completing-read "Languages to tag? "
                          ectags-language-file-suffix-alist nil nil)))
    (add-to-list
     'compilation-error-regexp-alist
     '("^\\([^:]+\\) confusing argument declarations beginning at line \\([0-9]+\\))" 1 2))
    (compile (ectag-directory-command tag-directory tag-languages) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Building tag completion obarray

(defun ectags-extract-tags (&optional tag-buffer obarray)
  "Extract the list of tags from the TAG-BUFFER if provided otherwise the
current buffer.
Add the tags to OBARRAY if provided otherwise to a new obarray and return it."
  (save-excursion
    (when tag-buffer
      (set-buffer tag-buffer))
    (goto-char (point-min))
    (forward-line 5)
    ;; Now point is at first tag,
    ;; Read the tags as the first word of each line
    (while (/= (point) (point-max))
      (forward-line)
      (intern
       (buffer-substring-no-properties (point) (progn (forward-word) (point)))
       obarray)))
  obarray)

(defun ectags-extract-files (&optional tag-buffer)
  "Extract a list of tags from a tag-buffer"
  (let ((result nil))
    (save-excursion
      (when tag-buffer
        (set-buffer tag-buffer))
      (goto-char (point-min))
      (forward-line 5)
      ;; now point is at first tag
      (while (search-forward "kind:file" (point-max) t)
        (beginning-of-line)
        (when (search-forward "	" (point-max) t)
          (let* ((start
                  (point-marker))
                 (end (progn
                        (search-forward "	" (point-max) t)
                        (backward-char)
                        (point-marker))))
            (add-to-list 'result (buffer-substring-no-properties start end)))
          (end-of-line))))
    result))

(defun ectags-make-obarray ()
  (let ((result (make-vector 511 0)))
    (mapc (lambda (b)
            (when (bufferp b)
              (save-excursion
                (ectags-extract-tags b result))))
          (ectags-table-list))
    (setq ectags-obarray result)))

(defun ectags-flatten-file-list (l)
  (let (result stack)
    (while (or stack l)
      (if l
          (if (consp l)
              (setq stack (cons (cdr l)
                                stack)
                    l (car l))
            (setq result (cons l result)
                  l nil))
        (setq l (car stack)
              stack (cdr stack))))
    result))

(defun ectags-make-tags-file-list ()
  "Create a list of all files in the tags"
  (let ((result nil))
    (setq result
          (mapcar (lambda (b) (ectags-extract-files b)) (ectags-table-list)))
    (ectags-flatten-file-list result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tags table mode

(defun ectags-table-list ()
  "Return a list of available tag tables."
  (let (tags-table-list)
    (dolist (buffer (buffer-list) tags-table-list)
      (when (assoc 'is-ectag-table (buffer-local-variables buffer))
        (push buffer tags-table-list)))
    tags-table-list))

(defvar ectags-table-mode-syntax-table
  (let ((ectags-syntax-table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" ectags-syntax-table)
    (modify-syntax-entry ?- "w" ectags-syntax-table)
    (modify-syntax-entry ?# "w" ectags-syntax-table)
    (modify-syntax-entry ?! "w" ectags-syntax-table)
    (modify-syntax-entry ?\" "w" ectags-syntax-table)
    (modify-syntax-entry ?& "w" ectags-syntax-table)
    (modify-syntax-entry ?< "w" ectags-syntax-table)
    (modify-syntax-entry ?\( "w" ectags-syntax-table)
    (modify-syntax-entry ?\) "w" ectags-syntax-table)
    (modify-syntax-entry ?: "w" ectags-syntax-table)
    (modify-syntax-entry ?\; "w" ectags-syntax-table)
    (modify-syntax-entry ?? "w" ectags-syntax-table)
    (modify-syntax-entry ?@ "w" ectags-syntax-table)
    (modify-syntax-entry ?\ "w" ectags-syntax-table)
    (modify-syntax-entry ?\[ "w" ectags-syntax-table)
    (modify-syntax-entry ?\] "w" ectags-syntax-table)
    (modify-syntax-entry ?\{ "w" ectags-syntax-table)
    (modify-syntax-entry ?\} "w" ectags-syntax-table)
    (modify-syntax-entry ?| "w" ectags-syntax-table)
    (modify-syntax-entry ?\' "w" ectags-syntax-table)
    (modify-syntax-entry ?^ "w" ectags-syntax-table)
    (modify-syntax-entry ?, "w" ectags-syntax-table)
    (modify-syntax-entry ?` "w" ectags-syntax-table)
    (modify-syntax-entry ?~ "w" ectags-syntax-table)
    ectags-syntax-table)
  "Punctuation free table")

(defvar is-ectag-table nil
  "ectag-table predicate.")

;;;###autoload
(defun ectags-table-mode ()
  "Major mode for exuberant ctags table file buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ectags-table-mode)
  (set-syntax-table ectags-table-mode-syntax-table)
  (setq mode-name "ECTags Tags Table")
  (set (make-local-variable 'is-ectag-table) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Removing tags tables

(defun ectags-wipe-tag-tables ()
  "Wipe out all ectags tables"
  (interactive)
  (mapc
   (lambda (x)
     (when (bufferp x) (progn (bury-buffer x) (kill-buffer x))))
   (ectags-table-list))
  (setq ectags-obarray nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Adding tags tables

;; Expand tags table name FILE into a complete file name.
(defun ectags-expand-table-name (file)
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (expand-file-name "tags" file)
    file))

;; Return non-nil iff the current buffer is a valid ectags TAGS file.
(defun ectags-verify-tags-table ()
  "Is current buffer actually an ectags table."
  ;; Use eq instead of = in case char-after returns nil.
  (goto-char (point-min))
  (looking-at "!_TAG_FILE_FORMAT"))

(defun ectags-verify-table (file)
  "Given a file, read it in to a buffer and validate it as a tags table."
  (save-excursion
    (message "Validating tags table %s " file)
    (if (get-file-buffer file)
        (progn
          (set-buffer (get-file-buffer file))
          (unless (ectags-verify-tags-table)
            (fundamental-mode)))
      (when (file-exists-p file)
        (progn
          (set-buffer (find-file-noselect file))
          (when (ectags-verify-tags-table)
            (ectags-table-mode)))))
    (assoc 'is-ectag-table (buffer-local-variables))))

;;;###autoload
(defun ectags-visit-tags-table (name)
  "Visit an exuberant ctags file and add it to the current list of tags tables."
  (interactive
   (list (read-file-name "Visit tags table (default tags): "
                         default-directory
                         (expand-file-name "tags"
                                           default-directory)
                         t)))
  (let ((curbuf (current-buffer))
        (local-tags-filename (ectags-expand-table-name name)))
    (if (ectags-verify-table local-tags-filename)
        ;; We have a valid tags table.
        (progn (message "Valid tags table")
               (setq ectags-obarray (ectags-make-obarray)))
      ;; The buffer was not valid.  Don't use it again.
      (progn (error "Not a valid tags table")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Saving and reloading sets of tag tables

(defun ectags-output (tag-buffer)
  "Output a line needed to restore this table to the tags buffer list"
  (insert "(add-working-ectags-table " (buffer-file-name tag-buffer) ")\n"))

(defun ectags-save-working-tables (fname)
  "Save the current working list of ectags tables in a file"
  (interactive "fFile to save tags tables in?:")
  (save-excursion
    (with-temp-buffer
      (insert
       ";; -*- mode: fundamental; coding: emacs-mule; -*-\n"
       ";; Created " (current-time-string) "\n"
       ";; Emacs version " emacs-version "\n\n"
       (dolist (tagbuff (ectags-table-list))
         (ectags-output tagbuff))
       "\;;")
      (write-region (point-min) (point-max) fname nil 'nomessage))))

(defun ectags-read-working-tables (fname)
  "Read the current working list of ectags tables in a file"
  (interactive "fFile to read tags tables from?:")
  (load fname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Actually finding tags and so forth

(defun ectags-match-tagname (tag-match)
  (nth 1 tag-match))

(defun ectags-match-filename (tag-match)
  (nth 2 tag-match))

(defun ectags-match-linenumber (tag-match)
  (nth 3 tag-match))

(defun ectags-match-tag-info (tag-match)
  (nth 4 tag-match))

(defun ectags-match-tags (tag fname lnumber info)
  "Given a tags match, rank it (via regexp match length) and
plonk it in the match candidates."
  (let*
      ((saved-fold-search case-fold-search)
       (case-fold-search (not ectags-case-sensitive))
       (match-rank (string-match ectags-regexp tag)))
    (when match-rank
      (let
          ((full-match-rank (- (length tag) (length ectags-regexp))))
        ;;        (message (format "Found %s ranking %d " tag full-match-rank))
        (add-to-list 'ectags-matches
                     (list
                      full-match-rank
                      tag
                      fname
                      (string-to-number lnumber)
                      (ectags-trim info)))))
    (setq case-fold-search saved-fold-search)))

(defun ectags-scan-tag (fn tag-buffer)
  "Scan a tag table buffer for a match with a tag. Applies fn to all matches."
  (save-excursion
    (set-buffer tag-buffer)
    (goto-char (point-min))
    (while (re-search-forward
            (format "^\\([^	]*%s[^	]*\\)	\\([^	]+\\)	\\([0-9]+\\);\"\\(.+\\)$"
                    ectags-regexp)  nil t)
      (apply fn (list (match-string-no-properties 1)
                      (match-string-no-properties 2)
                      (match-string-no-properties 3)
                      (match-string-no-properties 4))))))

(defun ectags-find-tag (fn tag-buffer)
  "Scan a tag table buffer for an exact match with a tag"
  (save-excursion
    (set-buffer tag-buffer)
    (goto-char (point-min))
    (while (re-search-forward
            (format "^\\(%s\\)	\\([^	]+\\)	\\([0-9]+\\);\"\\(.+\\)$"
                    ectags-regexp)  nil t)
      (apply fn (list (match-string-no-properties 1)
                      (match-string-no-properties 2)
                      (match-string-no-properties 3)
                      (match-string-no-properties 4))))))

(defun ectags-seek-tag  (regexp locate-fn)
  "Seek a match for the current regexp with the tags in the current
tag table buffer"
  (setq ectags-matches nil)
  (setq ectags-regexp regexp)
  (dolist (tags-buffer (ectags-table-list))
    (funcall locate-fn 'ectags-match-tags tags-buffer)
    (setq ectags-matches
          (sort ectags-matches '(lambda (x y)
                                    (< (car x) (car y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Ectags search

(defun ectags-file-scan (file-list tag)
  "Scan the list of files for the tag and return a list of markers
where it is found"
  (let ((result)
        (found))
    (loop
     for file in file-list
     do (save-excursion
          (message "Scanning %s " file)
          (find-file file)
          (setq found nil)
          (while (search-forward tag (point-max) t)
            (setq found t)
            (message "Found in %s " file)
            (add-to-list 'result (list file (line-number-at-pos (point)))))
          (kill-buffer nil)))
    result))

(defun ectags-reference-tag (tag)
  "Scan all currently tagged files for a tag and return a list of markers"
  (let*  ((file-list (ectags-make-tags-file-list)))
    (setq ectags-scan-marks (ectags-file-scan file-list tag))))

(defun ectags-next-tag-reference ()
  "Goto next ectag reference in current list, used as with tags-loop-continue"
  (interactive)
  (if (not (zerop (length ectags-scan-marks)))
      (let ((mark (car ectags-scan-marks)))
        (find-file (car mark))
        (forward-line (cadr mark))
        (setq ectags-scan-marks (cdr ectags-scan-marks)))
    (let ((tag
           (or (find-tag-default)
               (completing-read "Tag to reference " ectags-obarray))))
      (ectags-reference-tag tag)
      (when (not (zerop (length ectags-scan-marks)))
        (ectags-next-tag-reference)))))

(defun ectags-insert-tag-references (tagname)
  "Insert a refererence to a tag in an ectags-select buffer"
  (loop
   for index from 0 below (length ectags-scan-marks)
   do
   (let ((mark (nth index ectags-scan-marks)))
     (insert-with-faces
      "<"
      'font-lock-warning-face (int-to-string index)
      ">:["
      'font-lock-keyword-face tagname
      " in "
      'font-lock-string-face (car mark)
      "@"
      'font-lock-warning-face (int-to-string (cadr mark))
      "]\n" "    "  "\n"))))

(defun ectags-list-tag-references (tag)
  "List all references to the tag in a suitable buffer"
  (setq ectags-scan-marks nil)
  (setq ectags-reference-source-buffer (buffer-name))
  (get-buffer-create ectags-reference-buffer-name)
  (set-buffer ectags-reference-buffer-name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Finding tag: " tag "\n")
  (ectags-reference-tag tag)
  (if (not (zerop  (length ectags-scan-marks)))
      (progn
        (ectags-insert-tag-references tag)
        (set-buffer ectags-reference-buffer-name)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (setq ectags-reference-opened-window (selected-window))
        (unless (get-buffer-window ectags-reference-buffer-name)
          (select-window (split-window-vertically))
          (switch-to-buffer ectags-reference-buffer-name)
          (ectags-select-mode))
        (shrink-window-if-larger-than-buffer))
    (progn
      (message "Failed to find any references to tag %s " tag)
      (ding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Ectags mode

(defun ectags-select-case-fold-search ()
  "Get case-fold search."
  (when (boundp 'tags-case-fold-search)
    (if (memq tags-case-fold-search '(nil t))
        tags-case-fold-search
      case-fold-search)))

(defun ectags-select-insert-matches (tagname)
  "Insert matches to TAGNAME."
  (when ectags-matches
    (set-buffer ectags-select-buffer-name)
    (loop for index from 0 below
          (min (length ectags-matches) ectags-max-candidates)
          do
          (let ((mtch (nth index ectags-matches)))
            (insert-with-faces
             "<"
             'font-lock-warning-face (int-to-string index)
             ">:["
             'font-lock-keyword-face (ectags-match-tagname mtch)
             " in "
             'font-lock-string-face (ectags-match-filename mtch)
             "@"
             'font-lock-warning-face (int-to-string (ectags-match-linenumber mtch))
             "]\n" "    "
             (replace-regexp-in-string
              "signature:" "\n    signature:"
              (ectags-match-tag-info mtch))
             "\n")))))

       ;; (list "\\s \\(\\w+\\):\\w"
       ;;       '(1 font-lock-keyword-face))
       ;; (list "\\(signature\\):\\(.*\\)"
       ;;       '(1 font-lock-keyword-face)
       ;;       '(2 font-lock-comment-face))

(defun ectags-select-find (tagname)
  "Actually find a list of tags and push them into the tags select buffer"
  (setq ectags-select-source-buffer (buffer-name))
  (get-buffer-create ectags-select-buffer-name)
  (set-buffer ectags-select-buffer-name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Finding tag: " tagname "\n")
  (ectags-seek-tag tagname 'ectags-scan-tag)
  (if (>  (length ectags-matches) 0)
      (progn  (ectags-select-insert-matches tagname)
              (set-buffer ectags-select-buffer-name)
              (goto-char (point-min))
              (ectags-select-next-tag)
              (set-buffer-modified-p nil)
              (setq buffer-read-only t)
              (setq ectags-select-opened-window (selected-window))
              (unless (get-buffer-window ectags-select-buffer-name)
                (when ectags-window-split-function
                  (select-window (funcall ectags-window-split-function)))
                (switch-to-buffer ectags-select-buffer-name)
                (ectags-select-mode))
              (shrink-window-if-larger-than-buffer))
    (progn
      (message "Failed to find tag: %s " tagname)
      (ding))))

(defun ectags-select-goto-tag (&optional arg other-window)
  "Goto the file/line of the tag under the cursor.
Use the C-u prefix to prevent the etags-select window from closing."(interactive)
  (let ((case-fold-search (not ectags-case-sensitive)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "Finding tag: \\(.*\\)$"))
    (beginning-of-line)
    (if (not (looking-at "<"))
        (message "Please put the cursor on a line with a tag")
      (setq overlay-arrow-position (point-marker))
      (re-search-forward "\\[\\([^ ]+\\) in \\([^@]+\\)@\\([0-9]+\\)")
      (beginning-of-line)
      (let ((tag (match-string-no-properties 1))
            (fname (match-string-no-properties 2))
            (lnno (match-string-no-properties 3)))
        (unless arg
          (kill-buffer ectags-select-buffer-name)
          (when ectags-select-opened-window
            (delete-window (selected-window))
            (select-window ectags-select-opened-window)))
        (switch-to-buffer ectags-select-source-buffer)
        ;;(ring-insert find-tag-marker-ring (point-marker))
        (if other-window
            (find-file-other-window fname)
          (find-file fname))
        (goto-char (point-min))
        (forward-line (1- (string-to-number lnno)))
        (when (re-search-forward
               (replace-regexp-in-string
                "\\(.*\\s.\\)\\(\\w\\|\\s_\\)+" "" tag nil nil 1)
               (line-end-position) t)
          (goto-char (match-beginning 0))
          (when ectags-select-highlight-tag-after-jump
            (ectags-select-highlight (match-beginning 0) (match-end 0))))))))

(defun ectags-select-highlight (beg end)
  "Highlight a region temporarily.
If `ectags-select-highlight-delay' = 0 maintain the highlight
until another tag is selected.
If `ectags-select-highlight-delay' > 0 maintain the highlight
for `ectags-select-highlight-delay' seconds."
  (when ectags-current-overlay
    (delete-overlay ectags-current-overlay)
    (setq ectags-current-overlay nil))
  (setq ectags-current-overlay (make-overlay beg end))
  (overlay-put ectags-current-overlay 'face 'ectags-select-highlight-tag-face)
  (unless (= ectags-select-highlight-delay 0)
    (sit-for ectags-select-highlight-delay)
    (delete-overlay ectags-current-overlay)
    (setq ectags-current-overlay nil)))

(defun ectags-select-next-tag ()
  "Move to next tag in buffer."
  (interactive)
  (beginning-of-line)
  (when (not (eobp))
    (forward-line))
  (while (and (looking-at ectags-select-non-tag-regexp) (not (eobp)))
    (forward-line))
  (when (eobp)
    (goto-char (point-min))
    (ectags-select-next-tag)))

(defun ectags-select-previous-tag ()
  "Move to previous tag in buffer."
  (interactive)
  (beginning-of-line)
  (when (not (bobp))
    (forward-line -1))
  (while (and (looking-at ectags-select-non-tag-regexp) (not (bobp)))
    (forward-line -1))
  (when (bobp)
    (goto-char (point-max))
    (ectags-select-previous-tag)))

(defun ectags-select-quit ()
  "Quit ectags-select buffer."
  (interactive)
  (kill-buffer nil)
  (delete-window))

(defun ectags-select-by-tag-number (first-digit)
  (let ((tag-num (read-from-minibuffer "Tag number? " first-digit))
        (current-point (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^<" tag-num ">") nil t)
        ;; TODO -- need to push tag and close window
        (ectags-select-goto-tag)
      (goto-char current-point)
      (message (concat "Couldn't find tag number " tag-num))
      (ding))))

(defun ectags-select-tag-show-other-window ()
  "Select tag and show in other window."
  (interactive)
  (save-selected-window (ectags-select-goto-tag 4 t))
  (switch-to-buffer ectags-select-buffer-name))

(defun ectags-select-next-tag-show ()
  "Move next tag and show in other window."
  (interactive)
  (ectags-select-next-tag)
  (ectags-select-tag-show-other-window))

(defun ectags-select-previous-tag-show ()
  "Move previous tag and show in other window."
  (interactive)
  (ectags-select-previous-tag)
  (ectags-select-tag-show-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  User commands

;;;###autoload
(defun ectags-select-find-tag-at-point ()
  "Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `ectags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (let ((tag-to-find
         (or (find-tag-default)
             (completing-read "Tag to find" ectags-obarray))))
    (ectags-select-find tag-to-find)))

;;;###autoload
(defun ectags-select-reference-tag-at-point ()
  "Do a search for tag in all files in tags tables and list all hits"
  (interactive)
  (let ((tagname
         (or (find-tag-default)
             (completing-read "Tag to find" ectags-obarray))))
    (ectags-list-tag-references tagname)))


;;;###autoload
(defun ectags-select-find-tag ()
  "Do a find-tag, and display all exact matches.  If only one match is
found, see the `ectags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (let ((tagname (read-from-minibuffer
                  (format "Find tag (default %s): " (find-tag-default)) nil nil
                  nil 'find-tag-history)))
    (when (string= tagname "")
      (setq tagname (find-tag-default)))
    (ectags-select-find tagname)))

;;;###autoload
(defun ectags-select-reference-tag ()
  "Do a find-tag, and display all exact matches.  If only one match is
found, see the `ectags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (let ((tagname (read-from-minibuffer
                  (format "Find tag (default %s): " (find-tag-default)) nil nil
                  nil 'find-tag-history)))
    (when (string= tagname "")
      (setq tagname (find-tag-default)))
    (ectags-list-tag-references tagname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap

(defvar ectags-select-mode-map nil "'ectags-select-mode' keymap.")
(if (not ectags-select-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(return)] 'ectags-select-goto-tag)
      (define-key map [(? )] 'ectags-select-goto-tag)
      (define-key map [(?n)] 'ectags-select-next-tag)
      (define-key map [(down)] 'ectags-select-next-tag)
      (define-key map [(?p)] 'ectags-select-previous-tag)
      (define-key map [(up)] 'ectags-select-previous-tag)

      (define-key map [(control return)] 'ectags-select-tag-show-other-window)
      (define-key map [(control down)] 'ectags-select-next-tag-show)
      (define-key map [(control up)] 'ectags-select-previous-tag-show)

      (define-key map [(q)] 'ectags-select-quit)
      (define-key map "0" (lambda () (interactive) (ectags-select-by-tag-number "0")))
      (define-key map "1" (lambda () (interactive) (ectags-select-by-tag-number "1")))
      (define-key map "2" (lambda () (interactive) (ectags-select-by-tag-number "2")))
      (define-key map "3" (lambda () (interactive) (ectags-select-by-tag-number "3")))
      (define-key map "4" (lambda () (interactive) (ectags-select-by-tag-number "4")))
      (define-key map "5" (lambda () (interactive) (ectags-select-by-tag-number "5")))
      (define-key map "6" (lambda () (interactive) (ectags-select-by-tag-number "6")))
      (define-key map "7" (lambda () (interactive) (ectags-select-by-tag-number "7")))
      (define-key map "8" (lambda () (interactive) (ectags-select-by-tag-number "8")))
      (define-key map "9" (lambda () (interactive) (ectags-select-by-tag-number "9")))
      (setq ectags-select-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode startup

(defun ectags-select-mode ()
  "ectags-select-mode is a mode for browsing through exuberant ctags.\n\n
\\{ectags-select-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ectags-select-mode)
  (setq mode-name "Ectags")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map ectags-select-mode-map)
  (setq overlay-arrow-position nil)
  (run-hooks 'ectags-select-mode-hook))


(provide 'ectags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ectags.el ends here
