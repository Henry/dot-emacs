;;; init-magit.el --- Initialize magit interface to git
;; -----------------------------------------------------------------------------

(use-package ido-completing-read+
  :ensure t)

(use-package magit
  :ensure t
  :defer t
  :init (setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")
  :ensure with-editor
  :ensure git-commit
  :ensure transient
  :config
  (progn
    (add-hook 'magit-mode-hook 'font-lock-mode)
    (add-hook 'git-commit-mode-hook 'font-lock-mode)))

(define-key my-map "g" 'magit-status)

;; -----------------------------------------------------------------------------
;;; init-magit.el ends here
