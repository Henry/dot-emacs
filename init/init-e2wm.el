;;; init-e2wm.el --- Initialize haskell-mode
;; -----------------------------------------------------------------------------
(use-package e2wm
  :ensure window-layout
  :init
  (setq tree-widget-image-enable t)
  (require 'e2wm-code2)
  (global-set-key (kbd "M-+")
                '(lambda ()
                   (interactive)
                   (e2wm:start-management '(code2)))))

;; -----------------------------------------------------------------------------
;;; init-e2wm.el ends here
