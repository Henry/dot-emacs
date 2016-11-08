;;; init-speedbar.el --- Speedbar configuration
;; -----------------------------------------------------------------------------
(use-package sr-speedbar
  :ensure t
  :defer t
  :commands (sr-speedbar-open)
  :init
  (setq
   speedbar-hide-button-brackets-flag t
   speedbar-indentation-width 2
   speedbar-show-unknown-files t
   speedbar-update-flag nil
   speedbar-use-images t))

;; -----------------------------------------------------------------------------
;;; init-speedbar.el ends here
