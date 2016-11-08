;;; init-e2wm.el --- Configure e2wm
;; -----------------------------------------------------------------------------
(use-package e2wm
  :ensure t
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
