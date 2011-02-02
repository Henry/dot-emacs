;;; rw-acronyms.el --- interface between acronym files and Emacs/Gnus
;;
;; Copyright (C) 2009 Ralf Wachinger
;;
;; Author: Ralf Wachinger <rwachinger@gmx.de>
;; Version: 0.8
;; Keywords: acronym gnus
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; rw-acronyms.el is an interface between acronym files and Emacs/Gnus.
;; It can parse plain text acronym files and show the explanations for
;; the acronyms, which are included in the given acronym files.
;;
;; A big compilation is V.E.R.A. with over 11000 entries,
;; see ftp://ftp.gnu.org/gnu/vera/ or http://home.snafu.de/ohei/FTP
;; You can use it with rw-acronyms.el, for the configuration see below.
;;
;; Additionally you can use other files, resp. you can create your own files.
;;
;; Save rw-acronyms.el in a convenient directory, preferably in
;; your `load-path'. Add the following to your `user-init-file':
;;
;;   (require 'rw-acronyms)
;;
;; When you have the file vera.txt (this is the file you can make
;; with the scripts makevera and vera.pl) and an own file, e.g.
;; one with entries with the separator "=". Configure it as follows,
;; manually or better with customize (group `rw-acronyms'):
;;
;;   (setq rw-acronyms-files-data
;;         '(("<full-path>/own-file.txt" iso-8859-1 "=" nil)
;;           ("<full-path>/vera.txt" iso-8859-1 nil 13)))
;;
;; For Gnus you can add key bindings to your `gnus-init-file':
;;
;;   (define-key gnus-summary-mode-map "vya"
;;     'rw-acronyms-buttons-gnus-add-hook)
;;   (define-key gnus-summary-mode-map "vyr"
;;     'rw-acronyms-buttons-gnus-remove-hook)
;;   (define-key gnus-summary-mode-map "vyb"
;;     'rw-acronyms-buffer-gnus)
;;   (define-key gnus-summary-mode-map "vye"
;;     'rw-acronyms-erase-buffer)
;;
;; "vyb": Creates an buffer that contains the found acronyms and explanations
;; from the article buffer, and switches to the new buffer in view mode.
;; Typing "q" switches back to the summary buffer. Every function call
;; adds new text at the end of the acronym buffer.
;;
;; "vya": Add an hook that buttonize the found acronyms in the article buffer,
;; and shows the explanations in the minibuffer, when you point on
;; a created button with the mouse cursor, or when you go to the button
;; with the TAB-key. "vyr" removes the hook. The hook method is an alternative
;; to the extra buffer method. The function has to be called only once.
;;
;; Instead of calling the function interactively, you can add the following
;; to your `gnus-init-file' from rw-acronyms.el v0.3 on:
;;
;;   (rw-acronyms-buttons-gnus-add-hook)
;;
;; A possible menu item for Gnus in GNU Emacs (requires easy-menu):
;;
;;   (easy-menu-add-item
;;    gnus-summary-mode-map
;;    '("menu-bar" "Article")
;;    '("Acronyms"
;;      ["buffer with acronyms" rw-acronyms-buffer-gnus t]
;;      ["erase acronym buffer" rw-acronyms-erase-buffer
;;       (get-buffer rw-acronyms-buffer-name)]
;;      ["add hook" rw-acronyms-buttons-gnus-add-hook
;;       (not rw-acronyms-hook-added)]
;;      ["remove hook"  rw-acronyms-buttons-gnus-remove-hook
;;       rw-acronyms-hook-added]))
;;
;; For arbitrary Emacs buffers you can add key bindings
;; to your `user-init-file':
;;
;;   (global-set-key "\C-cyb" 'rw-acronyms-buffer-general)
;;   (global-set-key "\C-cye" 'rw-acronyms-erase-buffer)
;;   (global-set-key "\C-cyc" 'rw-acronyms-clear-data)
;;   (global-set-key "\C-cyn" 'rw-acronyms-read-explanations-anew)
;;   (global-set-key "\C-cyl" 'rw-acronyms-look-up)
;;   (global-set-key "\C-cyt" 'rw-acronyms-look-up-current)
;;   (global-set-key [C-M-mouse-1] 'rw-acronyms-look-up-current-tooltip)
;;
;; "\C-cyb": Similar to "vyb", but the current buffer is searched for acronyms.
;;
;; The following functions are intended for given acronyms:
;; "\C-cyl": The acronyms that you type into the minibuffer are searched.
;; "\C-cyt": The acronym that is marked or at point in the buffer is searched.
;; "[C-M-mouse-1]": Like "\C-cyt", when the mouse is used.
;;
;; The first call of the functions that search for acronyms takes a few
;; seconds with big acronym files, because the data is read into the memory
;; (if not already done) for an efficient access. If you add new files
;; to `rw-acronyms-files-data', or the file contents are changed, call
;;
;;   M-x rw-acronyms-read-explanations-anew RET
;;
;; afterwards, so that the new data is read into the memory.
;;
;; Tips for setting up your own acronym file:
;;
;; Choose a separator (a string), which is not found in the acronyms,
;; or write the explanations in a fixed column (use SPACEs, not TABs).
;; You can configure `rw-acronyms-files-data' to reflect that.
;; When you include abbreviations, write it in the file without spaces
;; and upper-case (it's the best anyway for acronyms to be found),
;; for example "E.G.", not "e.g." or "e. g.", see point 3. e).
;;
;; Search strategy used
;; in function `rw-acronyms-explanations' and `rw-acronyms-get-explanation':
;;
;; 1. Go to every character sequence resp. possible acronym in buffer.
;; 2. Search it in `rw-acronyms-hash-table' (efficient searching).
;; 3. We assume for acronyms:
;;    a) very most acronyms are upper-case
;;    b) some acronyms are lower-case
;;    c) acronyms contain alphanumeric characters and sometimes
;;       certain other characters, see `rw-acronyms-other-chars'
;;    d) acronyms can contain characters that are also used as 
;;       joining characters, normally hyphens, see `rw-acronyms-joiners'
;;    e) acronyms can be abbreviations with additional characters,
;;       normally dots, see `rw-acronyms-abbreviators',
;;       with spaces (e.g. usual in German), without spaces, or mixed,
;;       and they can be variably upper-case, lower-case, or mixed
;;    f) acronyms with non-alphanumeric characters can have different meanings
;;       than acronyms with only alphanumeric characters (e.g. <eg>, e.g., EG)
;; 4. We assume for acronym files:
;;    a) vera.txt contains only the alphanumeric characters of acronyms,
;;       other acronym files contain the acronyms with all characters
;;    b) some acronym files contain the acronyms in capital letters only,
;;       some contain them literally in capital and/or small letters
;;    c) included abbreviations are written without spaces
;; 5. Character sequences are parsed in a special way, when they contain
;;    a) hyphens (joiners), action: look up the whole sequence and every part,
;;    b) dots (abbreviators), action: take spaces into account and join parts.
;; 6. Thus - step b) when step a) does not found anything, and so on:
;;    a) the basic search is case sensitive,
;;       the original string is always searched first
;;    b) acronyms are searched upper-case,
;;       when `rw-acronyms-always-ignore-case' is t,
;;       when they are manually typed or selected,
;;       or when they contain non-alphanumeric characters
;;       (acronyms with only alphanumeric characters are normally
;;        simple words, they were false positives)
;;    c) if the string contains non-alphanumeric characters,
;;       it is searched without them
;;    d) the same as in c), and addtionally b)
;;
;; Reading strategy used in function `rw-acronyms-puthash',
;; when simple reading is off, see also `rw-acronyms-simple-reading':
;;
;; Do not save duplicated information from different files, e.g. for "BTW":
;; old: "By The Way (DFU-Slang, Usenet, IRC)", new: "By The Way"
;;   --> "BTW = By The Way (DFU-Slang, Usenet, IRC)" (discard new value)
;; new: "By The Way (DFU-Slang, Usenet, IRC)", old: "By The Way"
;;   --> "BTW = By The Way (DFU-Slang, Usenet, IRC)" (discard old line)
;; old: "By The Way (DFU-Slang, Usenet, IRC)", new: "By The Way <uebrigens>"
;;   --> "BTW = By The Way <uebrigens> (DFU-Slang, Usenet, IRC)"
;;
;;; TODO:
;;
;; Better parsing for complex acronyms/abbreviations.
;; Which key bindings and menu items for general use?
;;
;;; Change Log:
;; 
;; 2009-04-27 (0.8)
;;
;;    * improved function rw-acronyms-get-explanation (search logic/strategy)
;;    * commentary section updated/extended (search logic/strategy)
;;    * symbol 'rw-acronym for thing-at-point/rw-acronyms-base-look-up-current
;;    * all-chars as return value of function rw-acronym-all-chars
;;
;; 2009-04-26 (0.7)
;;
;;    * improved function rw-acronyms-puthash
;;    * rw-acronyms-get-explanation with new optional argument ignore-case
;;    * new user functions rw-acronyms-look-up-current[-tooltip|-echo-area]
;;      (issue: why is message-truncate-lines t in tooltip-show-help-non-mode?)
;;    * rw-acronyms-look-up and rw-acronyms-look-up-current[-tooltip|-echo-area]
;;      call rw-acronyms-get-explanation with argument ignore-case
;;    * commentary section updated/extended
;; 
;; 2009-04-14 (0.6)
;;
;;    * improvements relating to reading the explanations/definition
;;    * new user function rw-acronyms-read-explanations-anew
;;    * function rw-acronyms-puthash extended
;;    * new user option rw-acronyms-simple-reading
;;    * new user option rw-acronyms-to-length-not-to-process
;;    * user function rw-acronyms-look-up improved
;;    * commentary section updated/extended
;;
;; 2009-04-08 (0.5)
;;
;;    * made rw-acronyms-other-chars an user option
;;    * new user options rw-acronyms-joiners and rw-acronyms-abbreviators
;;    * new user option rw-acronyms-simple-parsing
;;    * function rw-acronyms-explanations finds abbreviations and compound
;;      acronyms in addition to "real" acronyms with "stop-go-review-parsing"
;;    * extended/improved function rw-acronyms-get-explanation
;;    * in function rw-acronyms-look-up the separators must be commas
;;    * updated commentary
;;    * decided: acronym buttonizing of arbitrary buffers is not really useful
;;    * decided: handling of coding systems (input from files) is sufficient
;;    * decided: rw-acronyms-clear-data is sufficient (call this function
;;               manually after acronym files are changed or files are added)
;;
;; 2009-03-29 (0.4)
;;
;;    * improved function rw-acronyms-explanations
;;    * extra functions rw-acronyms-write-acronym-buffer
;;      and rw-acronyms-get-explanation
;;    * other small code improvements
;;    * improved/extended recognizing of/searching for acronyms
;;    * search strategy explained in commentary section
;;    * new user option rw-acronyms-always-ignore-case
;;    * added function rw-acronyms-look-up
;;    * updated commentary
;;
;; 2009-03-27 (0.3)
;;
;;    * improved rw-acronyms-buttons-gnus-add-hook
;;      and rw-acronyms-buttons-gnus-remove-hook
;;    * updated commentary relating to the hook
;;    * improved and extended commentary (operating instructions in a way)
;;
;; 2009-03-26 (0.2)
;;    * rw-acronyms-alist replaced by rw-acronyms-hash-table
;;    * rw-acronyms-clear-alist renamed to rw-acronyms-clear-data
;;    * faster reading of data and faster search
;;    * improved (but not perfect yet) recognizing of acronyms
;;    * all explanations are showed when going to a created acronym button
;;    * beginning-of-line-no-mark removed
;;
;; 2009-03-24 (0.1)
;;    Initial Release.
;;
;;; Code:

;; User options.

(defgroup rw-acronyms nil
  "Acronyms customization options."
  :group 'applications)

(defface rw-acronyms-button
  '((t :underline "gray"))
  "Face of buttons that mark the found acronyms."
  :group 'rw-acronyms)

(defcustom rw-acronyms-buffer-name "*acronyms*"
  "Name of the acronym buffer."
  :group 'rw-acronyms
  :type 'string)

(defcustom rw-acronyms-files-data nil
  "List of data for acronym files.
Each entry is a list of the name of the acronyms file with full path,
the correct coding system of the file (e.g. iso-8859-1),
the acronym separator (a string), which divides search text and explanation,
and the column number (first column is 0), in which the explanation begin.
An acronym file must contain either lines with separators,
e.g. \"FTP=File Transfer Protocol (Internet, RFC 959)\",
or lines with columns,
e.g. \"FTP          File Transfer Protocol (Internet, RFC 959)\"."
  :group 'rw-acronyms
  :type '(repeat (list (file :tag "File name" :must-match t)
                       (coding-system :tag "File coding system"
                                      :value iso-8859-1)
                       (choice :tag "Separator"
                               (const :tag "None" nil)
                               (string :tag "String"))
                       (choice :tag "Column"
                               (const :tag "None" nil)
                               (integer :tag "Integer")))))

(defcustom rw-acronyms-strings-not-to-process nil
  "List of acronym strings that must not be processed."
  :group 'rw-acronyms
  :type '(repeat string))

(defcustom rw-acronyms-to-length-not-to-process 0
  "Acronym strings having this length or a smaller one are not processed."
  :group 'rw-acronyms
  :type 'integer)

(defcustom rw-acronyms-always-ignore-case nil
  "Always ignore case when searching for acronyms when non-nil."
  :group 'rw-acronyms
  :type 'boolean)

(defcustom rw-acronyms-other-chars "/$&!?*<>"
  "Characters that are considered additionally to alphanumeric characters
in resp. for acronyms, in a form suitable for `skip-chars-forward'."
  :group 'rw-acronyms
  :type 'string)

(defcustom rw-acronyms-joiners "-"
  "Characters that can be a part of acronyms, or that can join words,
in a form suitable for regexp character alternatives. Example: \"Co-op.\"."
  :group 'rw-acronyms
  :type 'string)

(defcustom rw-acronyms-abbreviators "."
  "Characters that can be abbreviation markers in acronyms,
in a form suitable for regexp character alternatives. Example: \"i.e.\"."
  :group 'rw-acronyms
  :type 'string)

(defcustom rw-acronyms-simple-parsing nil
  "Simple parsing ignoring joiners and abbreviations with spaces when non-nil.
See `rw-acronyms-joiners' and acronyms with `rw-acronyms-abbreviators'."
  :group 'rw-acronyms
  :type 'boolean)

(defcustom rw-acronyms-simple-reading nil
  "Simple reading of data from acronym files when non-nil.
Faster reading but with possibly redundant data. See `rw-acronyms-puthash'."
  :group 'rw-acronyms
  :type 'boolean)

;; Internal.

(defvar rw-acronyms-hash-table
  (make-hash-table :test 'equal)
  "Contains all data read from the acronym files.")

(defvar rw-acronyms-hook-added nil
  "Flag if acronym hook is added")

(defun rw-remove-trailing-whitespace (string)
  "Remove trailing whitespace in STRING and return changed string."
  (replace-regexp-in-string "[ \t\f\v\r\n]+$" "" string))

(defun rw-alnum-string (string &optional upcase)
  "Return STRING with alphanumeric characters only.
When UPCASE is non-nil return STRING upper-case."
  (mapconcat #'identity
             (split-string (if upcase (upcase string) string) "[^[:alnum:]]" t)
             ""))

(defun rw-acronyms-all-chars ()
  "Return all characters that are considered in resp. for an acronym,
in a form suitable for `skip-chars-forward'."
  (concat "[:alnum:]"
          rw-acronyms-other-chars
          rw-acronyms-abbreviators))

(defun rw-acronyms-puthash (key value)
  "Associate KEY with VALUE in `rw-acronyms-hash-table'.
If KEY is already present in table, concat VALUE to old value."
  (let ((case-fold-search t)
        (separators "[(<]")
        (non-separators " = [?(<]")
        (old-value (gethash key rw-acronyms-hash-table))
        (key-and-value (concat key " = " value))
        (new-values nil)
        (position 0))
    (if old-value
        (if rw-acronyms-simple-reading
            (puthash key (concat old-value "\n" key-and-value)
                     rw-acronyms-hash-table)
          ;; See commentary section for used search strategy.
          (unless (string-match (rw-alnum-string value)
                                (rw-alnum-string old-value))
            (dolist (line (split-string old-value "\n"))
              (setq position (or (string-match separators line)
                                 (length line)))
              (if (and (not (string-match non-separators line))
                       (string-match
                        (rw-alnum-string (substring line 0 position))
                        (rw-alnum-string (car (split-string key-and-value
                                                            separators)))))
                  (setq key-and-value (concat key-and-value " "
                                              (substring line position)))
                (add-to-list 'new-values line t)))
            (add-to-list 'new-values key-and-value t)
            (puthash key (mapconcat #'identity new-values "\n")
                     rw-acronyms-hash-table)))
      (puthash key key-and-value rw-acronyms-hash-table))))

(defun rw-acronyms-string-match (search-string limit &rest char-strings)
  "Determine if SEARCH-STRING matches a regexp character alternative composed
of CHAR-STRINGS. LIMIT search to: 'last or 'one character, or no limit: nil."
  (let* ((char-string (mapconcat 'identity char-strings "")))
    (when (string= char-string "")
      (error "Empty string: other-chars or joiners or abbreviators"))
    (string-match (if limit
                      (concat "^[" char-string "]$")
                    (concat "[" char-string "]"))
                  (if (and (eq limit 'last) (> (length search-string) 0))
                      (substring search-string -1)
                    search-string))))

(defun rw-acronyms-read-explanation-file (acronym-file-data)
  "Read all explanations from a explanation file."
  (let ((acronym-file-name (car acronym-file-data))
        (acronym-file-coding-system (nth 1 acronym-file-data))
        (acronym-separator (nth 2 acronym-file-data))
        (acronym-column (nth 3 acronym-file-data)))
    (condition-case ()
        (with-temp-buffer
          (let ((coding-system-for-read acronym-file-coding-system))
            (insert-file-contents acronym-file-name))
          (let ((case-fold-search nil))
            (goto-char (point-min))
            (while (not (eobp))
              (when (and acronym-separator (not acronym-column))
                (when (looking-at
                       (concat "^.+" (regexp-quote acronym-separator) ".+"))
                  (let* ((splitted-line
                          (split-string (thing-at-point 'line)
                                        acronym-separator t))
                         (found-text (rw-remove-trailing-whitespace
                                      (car splitted-line)))
                         (found-explanation
                          (rw-remove-trailing-whitespace
                           (mapconcat 'identity
                                      (cdr splitted-line)
                                      acronym-separator))))
                    (unless (or (<= (length found-text)
                                    rw-acronyms-to-length-not-to-process)
                                (member found-text
                                        rw-acronyms-strings-not-to-process))
                      (rw-acronyms-puthash found-text found-explanation)))))
              (when (and acronym-column (not acronym-separator))
                (when (looking-at
                       (concat "^.\\{"
                               (number-to-string (+ acronym-column 1))
                               ",\\}"))
                  (let* ((found-text
                          (rw-remove-trailing-whitespace
                           (substring (thing-at-point 'line)
                                      0 acronym-column)))
                         (found-explanation
                          (rw-remove-trailing-whitespace
                           (substring (thing-at-point 'line)
                                      acronym-column))))
                    (unless (or (<= (length found-text)
                                    rw-acronyms-to-length-not-to-process)
                                (member found-text
                                        rw-acronyms-strings-not-to-process))
                      (rw-acronyms-puthash found-text found-explanation)))))
              (forward-line 1))))
      (file-error
       (error "Failure to open the file %s" acronym-file-name)))))

(defun rw-acronyms-read-explanations ()
  "Read all explanations from the explanation files once."
  (when (= 0 (hash-table-count rw-acronyms-hash-table))
    (with-temp-message "Reading data in..."
      (dolist (acronym-file-data rw-acronyms-files-data)
        (rw-acronyms-read-explanation-file acronym-file-data)))
    (message "Reading data in...done")))

(defun rw-acronyms-get-explanation (acronym &optional ignore-case)
  "Get explanation for ACRONYM, if there is one.
Ignore case when IGNORE-CASE is non-nil."
  (let ((alnum-acronym (rw-alnum-string acronym)))
    (or (gethash acronym
                 rw-acronyms-hash-table)
        (and (or rw-acronyms-always-ignore-case
                 ignore-case
                 (not (equal acronym alnum-acronym)))
             (gethash (upcase acronym)
                      rw-acronyms-hash-table))
        (and (not (equal acronym alnum-acronym))
             (gethash alnum-acronym
                      rw-acronyms-hash-table))
        (and (not (equal acronym alnum-acronym))
             (or rw-acronyms-always-ignore-case
                 ignore-case)
             (gethash (upcase alnum-acronym)
                      rw-acronyms-hash-table))
        "")))

(defun rw-acronyms-write-acronym-buffer (found-explanations-list)
  "Write the found explanations into the acronym buffer."
  (let ((inhibit-read-only t)
        (acronym-buffer (get-buffer-create rw-acronyms-buffer-name)))
    (save-excursion
      (set-buffer acronym-buffer)
      (terpri acronym-buffer)
      (setq found-explanations-list (sort found-explanations-list 'string<))
      (dolist (found-explanation-to-print found-explanations-list)
        (princ found-explanation-to-print acronym-buffer)
        (terpri acronym-buffer)))
    (view-buffer rw-acronyms-buffer-name)
    (goto-char (point-max))
    (recenter -1)))

(defun rw-acronyms-produce-outcome (acronym begin-marker end-marker)
  "Make a button for current explanation, and/or add it to explanations list.
Called only from `rw-acronyms-explanations'."
  (let ((found-explanation (rw-acronyms-get-explanation acronym)))
    (unless (equal found-explanation "")
      (when make-buttons-y-n
        (widget-convert-button 'item
                               (marker-position begin-marker)
                               (marker-position end-marker)
                               :button-face 'rw-acronyms-button
                               :help-echo found-explanation))
      (when write-acronym-buffer-y-n
        (add-to-list 'found-explanations-list found-explanation)))))

(defun rw-acronyms-explanations (make-buttons-y-n
                                 write-acronym-buffer-y-n
                                 gnus-article-buffer-y-n)
  "Make buttons and/or a special buffer with helping textes for acronyms
based on acronym files, in/from current buffer or in/from gnus article buffer."
  (let ((inhibit-read-only t)
        (found-explanations-list (list))
        (begin (make-marker))
        (end (make-marker))
        (begin-joined (make-marker))
        (end-joined (make-marker))
        (acronym "")
        (joined-acronym "")
        (chars-between "")
        (chars-between-previous ""))
    (rw-acronyms-read-explanations)
    (save-excursion
      (when (and (featurep 'gnus) gnus-article-buffer-y-n)
        (set-buffer gnus-article-buffer))
      (save-restriction
        (when (and (featurep 'gnus) gnus-article-buffer-y-n)
          (gnus-narrow-to-body))
        (goto-char (point-min))
        ;; See commentary section for used search strategy.
        (skip-chars-forward (concat "^" (rw-acronyms-all-chars)))
        (while (not (eobp))
          ;; for every (i.e. single or possibly joined) string
          (set-marker begin (point))
          (skip-chars-forward (rw-acronyms-all-chars))
          (set-marker end (point))
          (setq acronym
                (buffer-substring-no-properties (marker-position begin)
                                                (marker-position end)))
          (rw-acronyms-produce-outcome acronym begin end)
          (skip-chars-forward (concat "^" (rw-acronyms-all-chars)))
          (unless rw-acronyms-simple-parsing
            (setq chars-between-previous chars-between)
            (setq chars-between
                  (buffer-substring-no-properties (marker-position end)
                                                  (point)))
            (cond (;; for every but last part of a joined string
                   (or (and (string= chars-between " ")
                            (rw-acronyms-string-match
                             acronym 'last rw-acronyms-abbreviators))
                       (rw-acronyms-string-match
                        chars-between 'one rw-acronyms-joiners))
                   (when (string= joined-acronym "")
                     (set-marker begin-joined (marker-position begin)))
                   (set-marker end-joined (marker-position end))
                   (setq joined-acronym (concat joined-acronym acronym
                                                chars-between)))
                  (;; for last (i.e. completing) part of a joined string
                   (and (not (string= joined-acronym ""))
                        (or (and (string= chars-between-previous " ")
                                 (rw-acronyms-string-match
                                  acronym 'last rw-acronyms-abbreviators))
                            (rw-acronyms-string-match
                             chars-between-previous 'one rw-acronyms-joiners)))
                   (setq joined-acronym (concat joined-acronym acronym))
                   (rw-acronyms-produce-outcome
                    (replace-regexp-in-string " " "" joined-acronym)
                    begin-joined end)
                   (setq joined-acronym ""))
                  (;; for back-processing an completed joined string
                   (not (string= joined-acronym ""))
                   (rw-acronyms-produce-outcome
                    (replace-regexp-in-string " " "" joined-acronym)
                    begin-joined end-joined)
                   (setq joined-acronym "")))))))
    (when write-acronym-buffer-y-n
      (rw-acronyms-write-acronym-buffer found-explanations-list))))

;; Symbol 'rw-acronym for thing-at-point.
(put 'rw-acronym 'beginning-op
     #'(lambda ()
         (skip-chars-backward (rw-acronyms-all-chars))))

(put 'rw-acronym 'end-op
     #'(lambda ()
         (skip-chars-forward (rw-acronyms-all-chars))))

(defun rw-acronyms-base-look-up-current (result-where)
  "Look up the explanation for an marked acronym or an acronym at point
in `rw-acronyms-hash-table'. RESULT-WHERE is a symbol that says where
the result is showed: 'echo-area, 'tooltip, 'acronym-buffer."
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                 (thing-at-point 'rw-acronym)))
         (explanation (if (or (not text) (equal text ""))
                          ""
                        (rw-acronyms-get-explanation text t)))
         (result (if (or (not text) (equal text ""))
                     "<no text/acronym there>"
                   (if (equal explanation "")
                       (concat text " -- <not found>")
                     explanation))))
    (rw-acronyms-read-explanations)
    (cond ((or (eq result-where 'acronym-buffer)
               (not (featurep 'tooltip)))
           (rw-acronyms-write-acronym-buffer (list result)))
          ((eq result-where 'tooltip)
           (tooltip-show result))
          ((eq result-where 'echo-area)
           (tooltip-show result t)))))

;; User functions.

(defun rw-acronyms-buffer-general ()
  "Make a special acronym buffer."
  (interactive)
  (rw-acronyms-explanations nil t nil))

(defun rw-acronyms-erase-buffer ()
  "Erase the acronym-buffer."
  (interactive)
  (let ((acronym-buffer (get-buffer rw-acronyms-buffer-name))
        (inhibit-read-only t))
    (save-excursion
      (when acronym-buffer
        (set-buffer acronym-buffer)
        (erase-buffer)
        (message "Acronym-buffer erased")))))

(defun rw-acronyms-buttons-general ()
  "Make acronym buttons."
  (interactive)
  (rw-acronyms-explanations t nil nil))

(defun rw-acronyms-clear-data ()
  "Clear `rw-acronyms-hash-table'.
When this hash-table is empty, the acronyms are read from the files again.
Purpose: After a file is changed, or is added to `rw-acronyms-files-data'."
  (interactive)
  (clrhash rw-acronyms-hash-table)
  (message "Data cleared"))

(defun rw-acronyms-read-explanations-anew ()
  "Clear acronym data and read acronym files anew.
Needed when the files have been changed or new files have been added."
  (interactive)
  (rw-acronyms-clear-data)
  (rw-acronyms-read-explanations))

(defun rw-acronyms-look-up (acronyms)
  "Look up the explanation for ACRONYMS in `rw-acronyms-hash-table'."
  (interactive "sAcronyms (separated by comma): ")
  (rw-acronyms-read-explanations)
  (rw-acronyms-write-acronym-buffer
   (mapcar #'(lambda (text)
               (let ((explanation (rw-acronyms-get-explanation text t)))
                 (if (equal explanation "")
                     (concat text " -- <not found>")
                   explanation)))
           (split-string acronyms ","))))

(defun rw-acronyms-look-up-current ()
  "Look up the explanation for an marked acronym in the acronym buffer."
  (interactive)
  (rw-acronyms-base-look-up-current 'acronym-buffer))

(defun rw-acronyms-look-up-current-tooltip ()
  "Look up the explanation for an marked acronym as tooltip."
  (interactive)
  (rw-acronyms-base-look-up-current 'tooltip))

(defun rw-acronyms-look-up-current-echo-area ()
  "Look up the explanation for an marked acronym in the echo-area."
  (interactive)
  (rw-acronyms-base-look-up-current 'echo-area))

;; User functions for Gnus.

(eval-after-load 'gnus
  '(progn
     (defun rw-acronyms-buttons-gnus ()
       "Make acronym buttons in gnus article buffer."
       (interactive)
       (rw-acronyms-explanations t nil t))

     (defun rw-acronyms-buttons-gnus-add-hook ()
       "Add hook so that every article is acronym-buttonized."
       (interactive)
       (add-hook 'gnus-article-prepare-hook
                 'rw-acronyms-buttons-gnus)
       (setq rw-acronyms-hook-added t)
       (when (and (gnus-buffer-live-p gnus-summary-buffer)
                  (gnus-buffer-live-p gnus-article-buffer))
         (gnus-summary-show-article))
       (message "Acronym-buttons activated"))

     (defun rw-acronyms-buttons-gnus-remove-hook ()
       "Remove hook so that no more article is acronym-buttonized."
       (interactive)
       (remove-hook 'gnus-article-prepare-hook
                    'rw-acronyms-buttons-gnus)
       (setq rw-acronyms-hook-added nil)
       (when (and (gnus-buffer-live-p gnus-summary-buffer)
                  (gnus-buffer-live-p gnus-article-buffer))
         (gnus-summary-show-article))
       (message "Acronym-buttons deactivated"))

     (defun rw-acronyms-buffer-gnus ()
       "Make a special acronym buffer for gnus article buffer."
       (interactive)
       (rw-acronyms-explanations nil t t))))

(provide 'rw-acronyms)

;;; rw-acronyms.el ends here
