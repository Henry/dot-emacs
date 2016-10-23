;;; init-e2wm.el --- Initialize haskell-mode
;; -----------------------------------------------------------------------------
(use-package e2wm
  :ensure window-layout)

(autoload 'e2wm:start-management "e2wm-code2" "e2wm-code2" t)

(global-set-key (kbd "M-+")
                '(lambda ()
                   (interactive)
                   (require 'e2wm-code2)
                   (e2wm:start-management '(code2))))

;; -----------------------------------------------------------------------------
;;; init-e2wm.el ends here
