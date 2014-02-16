;;; init-oprofile.el --- Initialize interface to oprofile
;; -----------------------------------------------------------------------------

(setq font-lock-ok-face 'font-lock-ok-face)
(setq font-lock-boldref-face 'font-lock-boldref-face)
(setq font-lock-face-attributes
      '((font-lock-ok-face              "blue3" nil t nil nil)
        (font-lock-boldref-face         "red" nil t nil nil)))
(add-hook 'oprofile-mode-hook 'turn-on-font-lock)
(autoload 'oprofile-mode "oprofile-mode" "OPROFILE editing mode." t)

;; Setup autoloading, we'll assume profile files end with .prof
(add-to-list 'auto-mode-alist '("\\.prof$"     . oprofile-mode))

;; -----------------------------------------------------------------------------
;;; init-oprofile.el ends here
