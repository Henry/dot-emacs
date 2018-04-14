;;; init-ycmd.el --- Initialize You Complete Me
;; -----------------------------------------------------------------------------

(use-package request
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package ycmd
  :ensure t
  :init (setq
         ycmd-server-command `("python" ,(file-truename "~/bin/ycmd/ycmd/"))
         ycmd-extra-conf-whitelist '("~/OpenFOAM-dev")
         ycmd-extra-conf-handler 'load))

(use-package company-ycmd
  :ensure t
  :after ycmd)

(use-package flycheck-ycmd
  :ensure t
  :after ycmd)

(defun my-c++-ycmd-mode-hook ()
  (ycmd-mode)
  (company-ycmd-setup)
  (flycheck-mode)
  (flycheck-ycmd-setup)
  (set (make-local-variable 'company-backends)
       (cons 'company-ycmd company-backends)))

(add-hook 'c++-mode-hook 'my-c++-ycmd-mode-hook)

;; -----------------------------------------------------------------------------
;;; init-ycmd.el ends here
