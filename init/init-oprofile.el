;;; init-oprofile.el --- Initialize interface to oprofile
;; -----------------------------------------------------------------------------

(add-hook 'oprofile-mode-hook 'turn-on-font-lock)
(autoload 'oprofile-mode "oprofile-mode" "OPROFILE editing mode." t)

;; Setup autoloading, we'll assume profile files end with .prof
(add-to-list 'auto-mode-alist '("\\.prof$"     . oprofile-mode))

;; -----------------------------------------------------------------------------
;;; init-oprofile.el ends here
