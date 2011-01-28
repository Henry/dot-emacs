;;; init-autocomplete.el --- Initialize automatic in-buffer completion package
;; -----------------------------------------------------------------------------
;;; Load package

(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (set-face-background 'ac-selection-face "steelblue")
  (set-face-background 'ac-menu-face "skyblue")
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\C-\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-\M-p" 'ac-previous)
  (setq ac-auto-start t)
  (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

  (add-to-list 'ac-modes 'eshell-mode)

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (make-local-variable 'ac-sources)
              (setq ac-sources
                    '(;ac-source-yasnippet
                      ac-source-abbrev
                      ac-source-words-in-buffer
                      ac-source-symbols))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (make-local-variable 'ac-sources)
              (setq ac-sources
                    '(;ac-source-yasnippet
                      ac-source-abbrev
                      ac-source-files-in-current-dir
                      ac-source-words-in-buffer)))))

;; -----------------------------------------------------------------------------
;;; init-autocomplete.el ends here
