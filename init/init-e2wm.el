;;; init-e2wm.el --- Configure e2wm
;; -----------------------------------------------------------------------------
(use-package e2wm
  :ensure t
  :ensure window-layout
  :diminish e2wm:pst-minor-mode
  :init
  (setq e2wm:debug nil
        tree-widget-image-enable t
        e2wm:prefix-key "C-z e")
  (which-key-add-key-based-replacements e2wm:prefix-key "e2wm")
  (require 'e2wm-code2)
  (require 'my-e2wm-vcs)
  (defun e2wm-code2 ()
    (interactive)
    (e2wm:start-management '(code2)))
  :bind ("M-+" . e2wm-code2)
  :config
  (setq e2wm:debug nil))

;; -----------------------------------------------------------------------------
;;; init-e2wm.el ends here
