;;; init-org.el --- Initialisation for the org-mode organiser
;; -----------------------------------------------------------------------------
;;; Package paths
;; (add-to-list 'load-path
;;              (expand-file-name "~/.emacs.d/packages/org-mode/lisp"))
;; (add-to-list 'load-path
;;              (expand-file-name "~/.emacs.d/packages/org-mode/contrib/lisp"))
;; (add-to-list 'Info-directory-list
;;              (expand-file-name "~/.emacs.d/packages/org-mode/doc") t)

;; -----------------------------------------------------------------------------
;;; Dependencies

;;(require 'org)
;;(require 'org-faces)
;;(require 'org-latex)
;;(require 'org-html)
;;(require 'org-protocol)

;; (load-library "~/.emacs.d/packages/org-mode/lisp/org")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-macs")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-list")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-compat")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-faces")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-latex")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-html")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-protocol")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-table")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/ob")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/ob-gnuplot")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-beamer")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-exp")
;; (load-library "~/.emacs.d/packages/org-mode/lisp/org-footnote")

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-macs)
(require 'org-list)
(require 'org-compat)
(require 'org-faces)
;(require 'org-latex)
;(require 'org-html)
(require 'org-protocol)
(require 'org-table)
(require 'ob-gnuplot)
(require 'ob-latex)
(require 'ox-html)
(require 'ox-beamer)
(require 'ob-exp)
(require 'org-footnote)

;;(require 'org-fstree)
(require 'find-lisp)

(use-package htmlize
  :ensure t)

;; -----------------------------------------------------------------------------
;;; Basic configuration

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-directory (concat user-emacs-directory "Org")
      org-return-follows-link t
      org-tab-follows-link nil
      org-hide-leading-stars t
      org-odd-levels-only t
      org-cycle-include-plain-lists t
      org-cycle-emulate-tab 'exc-hl-bol
      org-agenda-include-diary t ;; Include diary entries into Org-mode agenda
      org-log-done t
      org-file-apps '((t . emacs))
      org-agenda-custom-commands
      '(("A" "Agenda for week and list of all TODOs"
         (
          (agenda)
          (alltodo)
          )
         )
        ("c" "Weekly schedule" agenda ""
         ((org-agenda-ndays 7)
          (org-agenda-start-on-weekday 1)
          (org-agenda-repeating-timestamp-show-all t)
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'deadline 'scheduled)))
         )
        )
      org-columns-skip-arrchived-trees t

      org-emphasis-alist
      (quote
       (("*" bold "<b>" "</b>")
        ("/" italic "<i>" "</i>")
        ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
        ("=" org-code "<code>" "</code>" verbatim)
        ("~" org-verbatim "<code>" "</code>" verbatim)
        ("+"
         (:strike-through t)
         "<del>" "</del>")
        ("@" org-warning "<b>" "</b>")))

      org-export-latex-emphasis-alist
      (quote
       (("*" "\\textbf{%s}" nil)
        ("/" "\\emph{%s}" nil)
        ("_" "\\underline{%s}" nil)
        ("+" "\\st{%s}" nil)
        ("=" "\\protectedtexttt" t)
        ("~" "\\verb" t)
        ("@" "\\alert{%s}" nil)))

      org-latex-pdf-process
      (quote
       ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
      )

;; -----------------------------------------------------------------------------
;;; Avoid problems with [n] being treated as footnotes
(setq org-footnote-definition-re "^\\[fn:[-_[:word:]]+\\]"
      org-footnote-re            (concat "\\[\\(?:fn:\\([-_[:word:]]+\\)?:"
                                         "\\|"
                                         "\\(fn:[-_[:word:]]+\\)\\)"))
;;;  My org-mode hook
(defun my-org-mode-hook ()
  (setq fill-column 80)
  (setq comment-start nil) ; Hack to avoid auto-fill inserting '^#'
  (turn-on-auto-fill)
  (font-lock-mode 1)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  ;; Add org-mode completion
  ;;(add-to-list 'company-backends 'company-org t)

  (define-key org-mode-map [S-iso-lefttab] 'org-cycle)
  (define-key org-mode-map [C-S-iso-lefttab] 'org-global-cycle)
  (define-key org-mode-map "\C-cc" 'org-edit-src-code)
  (define-key org-src-mode-map "\C-cc" 'org-edit-src-exit)
  )
(add-hook 'org-mode-hook 'my-org-mode-hook)

;;;   Highlight current line in agenda
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; -----------------------------------------------------------------------------
;;; General bindings for org-mode

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;;;   Re-bind org-force-cycle-archived to C-S-tab
;;   which is bound to C-tab by default which has been rebound in icicles
;;   to replace S-tab also used by org-mode
(org-defkey org-mode-map [C-backtab] 'org-force-cycle-archived)

;;;   Auto-indent on return
(org-defkey org-mode-map "\C-m"  'org-return-indent)

;;;   Move forward/backward to the next link in the buffer using `C-n' and `C-p'
(org-defkey org-mode-map "\C-n"  'org-next-link)
(org-defkey org-mode-map "\C-p"  'org-previous-link)

;;;   Redraw the calendar centred on today before the agenda if it is present
(org-defkey org-agenda-mode-map "r"
            '(lambda ()
               (interactive)
               (if (get-buffer calendar-buffer)
                   (with-current-buffer calendar-buffer
                     (calendar-redraw)
                     (calendar-goto-today)))
               (org-agenda-redo)
               (org-agenda-goto-today)))

;; -----------------------------------------------------------------------------
;;; Remember

;;(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key "\C-cr" 'org-remember)

(defun org-show-remember-file-contents (&optional org-force-remember-template-char)
  "Show the sub-headings in a `remember' file,
i.e. the headings under the single top-level heading."
  (interactive)
  (org-remember '(4) org-force-remember-template-char)
  (goto-char (point-min))
  (if (re-search-forward (concat "^" outline-regexp) nil t)
      (progn
        (goto-char (match-beginning 0))
        (show-children))))

(global-set-key "\C-cR" 'org-show-remember-file-contents)

;; -----------------------------------------------------------------------------
;;; Web-links using org-protocol and remember

;; (defun org-remember-conkeror (url)
;;   (interactive "s")
;;   (org-remember nil ?w)
;;   (save-excursion
;;     (insert "\n\n  [[" url "]]"))
;;   (local-set-key (kbd "C-c C-c")
;;                  (lambda ()
;;                    (interactive)
;;                    (org-remember-finalize)
;;                    (delete-frame nil t))))

;; -----------------------------------------------------------------------------
;;; Org-mode help in org-mode

(org-defkey org-mode-map "\C-ch"
            (lambda ()
              (interactive)
              (find-file (concat org-directory "/Notes/org.org"))))

;; -----------------------------------------------------------------------------
;;; File annotation
(use-package org-pua
  :init
  (setq org-pua-annotations-dir (concat user-emacs-directory "Annotations/"))
  :bind (([(control c) f1] . org-pua-annotate)
         ([(control c) (control f1)] . org-pua-toggle-buttons)))

;; -----------------------------------------------------------------------------
;;; Toggle inline images using C-i

(require 'iimage)

(setq iimage-mode-image-search-path (expand-file-name "~/"))

;;;  Match org file: links
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?"
                           iimage-mode-image-filename-regex
                           "\\)\\]")  1))

(define-key org-mode-map [(control c) ?i] 'iimage-mode)

;; -----------------------------------------------------------------------------
;;; Gnuplot tables

(org-defkey org-mode-map "\M-\C-g" 'org-plot/gnuplot)

;; -----------------------------------------------------------------------------
;;; LaTeX export

(defun org-view-latex-file-name (&optional extension nondirectory)
  (concat (file-name-sans-extension
    (file-name-nondirectory ;sans-extension
     buffer-file-name)) 'extension))

(defun org-view-latex ()
  (interactive)
  (org-export-as-latex 3)
  (let ((file-name-stem
         (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (let ((tex-buffer (concat file-name-stem ".tex")))
      (set-buffer tex-buffer)
      (TeX-PDF-mode-on)
      (TeX-command "LaTeX" 'TeX-master-file 0)
      (select-frame
       (make-frame `((height . 71) (width . 105) (top . 0) (left . 0))))
      (find-file (concat file-name-stem ".pdf"))
      ;(set-buffer tex-buffer)
      ;(TeX-view)
      )))

;;;   Hacked version of org-export-latex-links to remove the "//" from "file://"
(defun org-export-latex-links-save ()
  ;; Make sure to use the LaTeX hyperref and graphicx package
  ;; or send some warnings.
  "Convert links to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-analytic-regexp nil t)
    (org-if-unprotected
     (goto-char (match-beginning 0))
     (let* ((re-radio org-export-latex-all-targets-re)
            (remove (list (match-beginning 0) (match-end 0)))
            (type (match-string 2))
            (raw-path (org-extract-attributes (match-string 3)))
            (full-raw-path (concat (match-string 1) raw-path))
            (desc (match-string 5))
            imgp radiop
            ;; define the path of the link
            (path (cond
                   ((member type '("http" "https" "ftp"))
                    (concat type ":" raw-path))
                   ((and re-radio (string-match re-radio raw-path))
                    (setq radiop t))
                   ((equal type "mailto")
                    (concat type ":" raw-path))
                   ((equal type "file")
                    (if (and (or (org-file-image-p (expand-file-name raw-path))
                                 (string-match "\\.eps$" raw-path))
                             (equal desc full-raw-path))
                        (setq imgp t)
                      (progn (when (string-match "\\(.+\\)::.+" raw-path)
                               (setq raw-path (match-string 1 raw-path)))
                             (if (file-exists-p raw-path)
                                 (concat type ":" (expand-file-name raw-path))
                               (concat type ":"
                                       (org-export-directory
                                        :LaTeX org-export-latex-options-plist)
                                       raw-path))))))))
       ;; process with link inserting
       (apply 'delete-region remove)
       (cond ((and imgp (plist-get
                         org-export-latex-options-plist
                         :inline-images))
              (insert (format "\\includegraphics[%s]{%s}"
                              ;; image option should be set be a comment line
                              org-export-latex-image-default-option
                              (expand-file-name raw-path))))
             (radiop (insert (format "\\hyperref[%s]{%s}" raw-path desc)))
             (path (insert (format "\\href{%s}{%s}" path desc)))
             (t (insert "\\texttt{" desc "}")))))))

;; -----------------------------------------------------------------------------
;;; General support functions

(defun org-tbl-push-props (final values value-prop percent-prop)
  "Calculate the sum of the VALUES and the percentage w.r.t. FINAL,
push these results into the given properties VALUE-PROP and PERCENT-PROP
and return the sum of the VALUES"
  (let ((total (apply '+ values)))
    (org-entry-put (point) value-prop
                   (format "%.1f" total))
    (org-entry-put (point) percent-prop
                   (format "%.0f" (/ (* 100 total) final)))
    (format "%.1f" total)))

;; -----------------------------------------------------------------------------
;;; Functions to create standard layouts

(defun my-agenda (&optional option)
  "Create agenda layout including TODO list, calendar and mp3 browser
in a full-height window"
  (interactive)
  ;;(load-library "org-faces")
  (autoload-init-diary)
  (delete-other-windows)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) 70)
  (setq org-agenda-files
        (append org-agenda-files
                (find-lisp-find-files support-hours-dir "\.org$")))
  (org-agenda nil "A")
  (org-agenda-goto-today)
  (org-agenda-goto-calendar)
  (other-window 1)
  )

;;(add-to-list 'command-switch-alist '("agenda" . my-agenda))

;; -----------------------------------------------------------------------------
;;; Use "minted" LaTeX package for listings

(setq org-export-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))

;; -----------------------------------------------------------------------------
;;; Load personal information

(load "personal/init-org")

;; -----------------------------------------------------------------------------
;;; init-org.el ends here
