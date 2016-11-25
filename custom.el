;;; custom.el --- Application generated customizations
;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (code2)))
 '(custom-safe-themes t)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "wmake -q -s" projectile-compilation-cmd-map))
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "makeEemacs" projectile-compilation-cmd-map))
     (header-auto-update-enabled)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -----------------------------------------------------------------------------
;;; custom.el ends here
