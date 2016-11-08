;;; init-latex.el --- Initialisation for LaTeX
;; -----------------------------------------------------------------------------
;;; LaTeX settings

(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/doc/latexrefman") t)

(use-package cdlatex
  :ensure t)

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'TeX-mode-hook 'turn-on-reftex)
    (add-hook 'TeX-mode-hook 'my-TeX-mode-hook)
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook 'my-TeX-mode-hook)
    (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t
          reftex-plug-into-AUCTeX t)
    (setq TeX-master t))
  :config
  (require 'preview))

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

(use-package bibtex
  :ensure t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t
          bibtex-autokey-name-case-convert 'identity
          bibtex-autokey-year-length 2
          bibtex-autokey-titlewords 3
          bibtex-autokey-titlewords-stretch 0
          bibtex-autokey-titleword-length -1
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator ""
          bibtex-autokey-titleword-separator nil
          bibtex-autokey-edit-before-use nil
          bibtex-align-at-equal-sign t
          bibtex-entry-format '(opts-or-alts
                                required-fields numerical-fields
                                whitespace realign last-comma
                                delimiters unify-case braces strings)
          bibtex-autokey-titleword-ignore
          '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
            "[^[:upper:]].*" ".*[^[:upper:]0-9].*"
            "the" "of" "a" "in" "on" "an" "for" "and" "terms"
            "to" "it" "its" "by"))
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

;; -----------------------------------------------------------------------------
;;;  Ebib
(use-package ebib
  :ensure t)

;;----------------------------------------------------------------------------
;;; init-latex.el ends here
