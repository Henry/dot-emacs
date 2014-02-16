;;; init-abbrev.el --- Initialize the abbreviations package
;; -----------------------------------------------------------------------------

(setq abbrev-file-name "~/Emacs/abbrevs")

;; Read the abbreviations file on startup
(quietly-read-abbrev-file)

;; Switch on in all modes
;;(setq default-abbrev-mode t)

;; Switch on in text and derived modes
;;(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

;; Switch on in multiple modes
;; (dolist (hook '(erc-mode-hook
;;                 emacs-lisp-mode-hook
;;                 text-mode-hook))
;;   (add-hook hook (lambda () (abbrev-mode 1))))

;; -----------------------------------------------------------------------------
;;; init-abbrev.el ends here
