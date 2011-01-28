;;; init-latex.el --- Initialisation for LaTeX
;; -----------------------------------------------------------------------------
;;; LaTeX settings

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/auctex"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/auctex/share/info") t)
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/latexrefman") t)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(load "cdlatex.el" nil t t)

(add-to-list 'auto-mode-alist '("\\.tex$"  . LaTeX-mode))

(defun my-TeX-mode-hook ()
  (setq fill-column 80
        truncate-lines nil)
  (turn-on-auto-fill)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  (setq TeX-brace-indent-level 4
        TeX-default-mode 'latex-mode)

  ;; Set the regexp used by outline-mode to find the headings
  (set (make-local-variable 'outline-regexp) "%%%[ ]+\\|(......")

  ;; Remove the number of "\\\" from the outline-level
  (set (make-local-variable 'outline-level) '(lambda () (- (outline-level) 3)))

  ;; Set the heading levels for promotion and demotion
  (setq outline-promotion-headings '("%%% " "%%%  " "%%%   "))

  ;; Turn on outline minor mode by default
  (outline-minor-mode +1)

  ;; Spell-checking settings
  (setq ispell-parser 'tex)

  (add-to-list
   'TeX-command-list
   '("PDF" "dvipdf %s" TeX-run-command nil t))

  (add-to-list
   'TeX-command-list
   '("PDFView" "kpdf %s.pdf" TeX-run-discard nil t))

  ;; Better commenting/un-commenting
  (define-key TeX-mode-map "\C-c\C-c" 'comment-dwim-line)

  (define-key TeX-mode-map [f5] 'my-tex-run-tex)
  (define-key TeX-mode-map [f6] 'my-tex-run-bibtex)
  (define-key TeX-mode-map [f7] 'my-tex-run-view)
  (define-key TeX-mode-map [f8] 'my-tex-run-dvipdf)
  (define-key TeX-mode-map [f9] 'my-tex-run-acroread)
  )
(add-hook 'TeX-mode-hook 'my-TeX-mode-hook)

(defun my-LaTeX-mode-hook ()
  (setq LaTeX-indent-level 4)
  (setq LaTeX-left-right-indent-level 4)
  (setq LaTeX-item-indent 0)
  (turn-on-cdlatex)

  ;; Better commenting/un-commenting
  (define-key LaTeX-mode-map "\C-c\C-c" 'comment-dwim-line)

  (define-key LaTeX-mode-map [f5] 'my-tex-run-latex)
  (define-key LaTeX-mode-map [f6] 'my-tex-run-bibtex)
  (define-key LaTeX-mode-map [f7] 'my-tex-run-view)
  (define-key LaTeX-mode-map [f8] 'my-tex-run-dvipdf)
  (define-key LaTeX-mode-map [f9] 'my-tex-run-acroread)

  ;; Reclaim TAB for indentation
  (define-key cdlatex-mode-map "\t" 'indent-for-tab-command)
  (define-key cdlatex-mode-map [backtab] 'cdlatex-tab)

  (cond
   ((eq window-system 'x)
    (font-lock-add-keywords
     nil
     '(("^%%% [^ ].*" 0 'outline-2 t)))
    (font-lock-add-keywords
     nil
     '(("^%%%  [^ ].*" 0 'outline-3 t)))
    (font-lock-add-keywords
     nil
     '(("^%%%   [^ ].*" 0 'outline-4 t)))

    ;; Make the \> tabs less obvious is the tabbing environment
    (make-face 'latex-tabbing-tab)
    (set-face-attribute 'latex-tabbing-tab nil
                        :foreground "gainsboro")
    (font-lock-add-keywords
     nil
     '(("\\\\>" 0 'latex-tabbing-tab t)))
    ))
  )
(add-hook 'LaTeX-mode-hook 'my-TeX-mode-hook)
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)


;; Functions by Carsten Dominik <dominik@astro.uva.nl>
(defun my-tex-run-command (cmd &optional recenter)
  (let ((buf (current-buffer)))
    (save-buffer)
    (TeX-command-menu cmd)
    ;; herbert: no output buffer by default
    (setq recenter nil)
    ;; /herbert
    (if recenter
        (condition-case nil
            (save-excursion
              (set-buffer buf)
              (TeX-recenter-output-buffer nil))
          (error nil)))))

(defun my-tex-run-tex ()
  "Run TeX on current document."
  (interactive)
  (my-tex-run-command "TeX" 'recenter))

(defun my-tex-run-latex ()
  "Run LaTeX on current document."
  (interactive)
  (my-tex-run-command "LaTeX" 'recenter))

(defun my-tex-run-bibtex ()
  "Run BibTeX on current document."
  (interactive)
  (my-tex-run-command "BibTeX" 'recenter))

(defun my-tex-run-makindex ()
  "Run BibTeX on current document."
  (interactive)
  (my-tex-run-command "Index" 'recenter))

(defun my-tex-run-view ()
  "Run View on current document."
  (interactive)
  (let* ((entry (copy-sequence (assoc "View" TeX-command-list)))
         TeX-command-list)
    (rplaca (nthcdr 3 entry) nil)
    (setq TeX-command-list (list entry))
    (my-tex-run-command "View")))

(defun my-tex-run-print ()
  "Run Print on current document."
  (interactive)
  (let* ((entry (copy-sequence (assoc "Print" TeX-command-list)))
         TeX-command-list)
    (rplaca (nthcdr 3 entry) nil)
    (setq TeX-command-list (list entry))
    (TeX-command-menu-print TeX-printer-default "dvips -f %s|lpc" "Print")))

(defun my-tex-run-file ()
  "Run File on current document."
  (interactive)
  (let* ((entry (copy-sequence (assoc "File" TeX-command-list)))
         TeX-command-list)
    (rplaca (nthcdr 3 entry) nil)
    (setq TeX-command-list (list entry))
    (my-tex-run-command "File" 'recenter)))

(defun my-tex-run-latex-and-file (&optional arg)
  "Run LaTeX and dvips."
  (interactive "P")
  (let ((buf (current-buffer)) ret)
    (my-tex-run-latex)
    (set-buffer buf)
    (while (and (setq ret (sit-for 1))
                (TeX-process (TeX-master-file))))
    (and ret (my-tex-run-file))))

(defun my-tex-run-gv ()
  "View current document using gv."
  (interactive)
  (my-tex-run-command "gv"))
;; end functions by Carsten Dominik <dominik@astro.uva.nl>

(defun my-tex-run-dvipdf ()
  "Run dvipdf on current document."
  (interactive)
  (my-tex-run-command "PDF"))

(defun my-tex-run-acroread ()
  "View current PDF document using."
  (interactive)
  (my-tex-run-command "PDFView"))


;; -----------------------------------------------------------------------------
;;; BibTeX

;;;  General settings

(require 'bibtex)

(add-to-list 'auto-mode-alist '("\\.bib$"  . bibtex-mode))

(setq bibtex-autokey-name-case-convert 'identity
      bibtex-autokey-year-length 2
      bibtex-autokey-titlewords 3
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-length -1
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-titleword-separator nil
      bibtex-autokey-edit-before-use nil
      bibtex-autokey-before-presentation-function 'my-bibtex-autokey-unique
      bibtex-align-at-equal-sign t
      bibtex-entry-format '(opts-or-alts required-fields numerical-fields
                                         whitespace realign last-comma
                                         delimiters unify-case braces strings)
      bibtex-autokey-titleword-ignore
      '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
        "[^[:upper:]].*" ".*[^[:upper:]0-9].*"
        "the" "of" "a" "in" "on" "an" "for" "and" "terms"
        "to" "it" "its" "by"))

(defun my-bibtex-autokey-unique (key)
  "Make a unique version of KEY
   by converting the last character to an integer 1-9."
  (save-excursion
    (let ((trykey key)
          (next ?1))
      (while (and (bibtex-find-entry trykey t)
                  (<= next ?9))
        (aset trykey (1- (length trykey)) next)
        (setq next (1+ next)))
      trykey)))

;;;  Key-change data and functions

(require 'eieio)
(require 'eieio-base)

(defclass my-key-change-list-class (eieio-persistent)
  ((map  :initarg :map
         :initform nil
         :type list
         :documentation "The BibTeX key change map.")
   (file-header-line :initform ";; BibTeX key map"))
  "Class to hold the BibTeX key change map.")

(defvar my-key-change-list
  (my-key-change-list-class "BibTexMap" :file "~/Latex/bib/bibKeyMap.elo")
  "Map of key changes made in the bibtex file")

;;(eieio-persistent-save my-key-change-list)
;;(eieio-persistent-read (oref my-key-change-list :file))

(defun my-bibtex-correct-keys ()
  "Correct all the keys in the buffer and store the change-map in my-key-change-list."
  (interactive)
  (dolist (key-map (oref my-key-change-list map))
    (goto-char (point-min))
    (while (search-forward (car key-map) nil t)
      (replace-match (cdr key-map) t nil))))

(defun my-bibtex-clean-entry (&optional called-by-reformat)
  "Finish editing the current BibTeX entry and clean it up.
Check that no required fields are empty and format entry dependent
on the value of `bibtex-entry-format'.
If the reference key of the entry is empty or a prefix argument is given,
calculate a new reference key.  (Note: this works only if fields in entry
begin on separate lines prior to calling `bibtex-clean-entry' or if
'realign is contained in `bibtex-entry-format'.)
Don't call `bibtex-clean-entry' on @Preamble entries.
At end of the cleaning process, the functions in
`bibtex-clean-entry-hook' are called with region narrowed to entry."
  ;; Opt. arg CALLED-BY-REFORMAT is t if `bibtex-clean-entry'
  ;; is called by `bibtex-reformat'
  (interactive "P")
  (let ((case-fold-search t)
        (start (bibtex-beginning-of-entry))
        (_ (or (looking-at bibtex-any-entry-maybe-empty-head)
               (error "Not inside a BibTeX entry")))
        (entry-type (bibtex-type-in-head))
        (key (bibtex-key-in-head)))
    (cond ((bibtex-string= entry-type "preamble")
           ;; (bibtex-format-preamble)
           (error "No clean up of @Preamble entries"))
          ((bibtex-string= entry-type "string")
           (setq entry-type 'string))
          ;; (bibtex-format-string)
          (t (bibtex-format-entry)))
    ;; set key
    (let ((old-key key))
      (setq key (bibtex-generate-autokey))
      (when (not (equal key old-key))
        (object-add-to-list my-key-change-list :map (cons old-key key)) ; HGW
      ;; Sometimes `bibtex-generate-autokey' returns an empty string
      (if (or bibtex-autokey-edit-before-use (string= "" key))
          (setq key (if (eq entry-type 'string)
                        (bibtex-read-string-key key)
                      (bibtex-read-key "Key to use: " key))))
      (save-excursion
        (re-search-forward (if (eq entry-type 'string)
                               bibtex-string-maybe-empty-head
                             bibtex-entry-maybe-empty-head))
        (if (match-beginning bibtex-key-in-head)
            (delete-region (match-beginning bibtex-key-in-head)
                           (match-end bibtex-key-in-head)))
        (insert key))))

    (unless called-by-reformat
      (let* ((end (save-excursion
                    (bibtex-end-of-entry)
                    (if (re-search-forward
                         bibtex-entry-maybe-empty-head nil 'move)
                        (goto-char (match-beginning 0)))
                    (point)))
             (entry (buffer-substring start end))
             ;; include the crossref key in index
             (index (let ((bibtex-maintain-sorted-entries 'crossref))
                      (bibtex-entry-index))) ; moves point to end of head
             error)
        ;; sorting
        (if (and bibtex-maintain-sorted-entries
                 (not (and bibtex-sort-ignore-string-entries
                           (eq entry-type 'string))))
            (progn
              (delete-region start end)
              (setq error (not (bibtex-prepare-new-entry index))
                    start (point)) ; update start
              (save-excursion (insert entry)))
          (bibtex-search-entry key)
          (setq error (or (/= (point) start)
                          (bibtex-search-entry key nil end))))
        (if error
            (error "New inserted entry yields duplicate key"))
        (dolist (buffer (bibtex-initialize))
          (with-current-buffer buffer
            (if (cdr (assoc-string key bibtex-reference-keys))
                (error "Duplicate key in %s" (buffer-file-name)))))

        ;; Only update the list of keys if it has been built already.
        (cond ((eq entry-type 'string)
               (if (and (listp bibtex-strings)
                        (not (assoc key bibtex-strings)))
                   (push (cons key (bibtex-text-in-string
                                    (bibtex-parse-string) t))
                         bibtex-strings)))
              ;; We have a normal entry.
              ((listp bibtex-reference-keys)
               (cond ((not (assoc key bibtex-reference-keys))
                      (push (cons key t) bibtex-reference-keys))
                     ((not (cdr (assoc key bibtex-reference-keys)))
                      ;; Turn a crossref key into a header key
                      (setq bibtex-reference-keys
                            (cons (cons key t)
                                  (delete (list key) bibtex-reference-keys)))))
               ;; Handle crossref key.
               (if (and (nth 1 index)
                        (not (assoc (nth 1 index) bibtex-reference-keys)))
                   (push (list (nth 1 index)) bibtex-reference-keys)))))

      ;; final clean up
      (if bibtex-clean-entry-hook
          (save-excursion
            (save-restriction
              (bibtex-narrow-to-entry)
              (run-hooks 'bibtex-clean-entry-hook)))))))

(defun my-bibtex-clean-enties ()
  (interactive)
  (bibtex-progress-message "Formatting" 1)
  (bibtex-map-entries (lambda (key beg end)
                        (message "key %s" key)
                        (bibtex-progress-message)
                        (my-bibtex-clean-entry nil t)))
  (bibtex-progress-message 'done))

;; -----------------------------------------------------------------------------
;;;  Ebib

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/ebib/src"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/ebib/manual") t)
(require 'ebib)

;; -----------------------------------------------------------------------------
;;; RefTeX

(setq reftex-plug-into-AUCTeX t)
(add-hook 'TeX-mode-hook 'turn-on-reftex)     ; with AUCTeX TeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

;;----------------------------------------------------------------------------
;;; init-latex.el ends here
