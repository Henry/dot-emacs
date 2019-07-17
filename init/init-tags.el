;;; init-tags.el --- Initialise all things tags related
;; -----------------------------------------------------------------------------
;;; Etags-select (use local version)
(use-package etags-select
  :load-path "my-lisp"
  :init
  (add-hook 'etags-select-mode-hook
            '(lambda ()
               (font-lock-mode 1)
               (hl-line-mode 1)))
  :bind (("\M-?" . etags-select-find-tag-at-point)
         ("\M-." . etags-select-find-tag)))

;; -----------------------------------------------------------------------------
;;; Etags-table
(use-package etags-table
  :ensure t
  :init
  (setq tags-add-tables nil
        etags-table-alist
        `((".*\\.el$"
           ,(expand-file-name "TAGS" user-emacs-directory)
           ,(expand-file-name "systemTAGS" user-emacs-directory)))))

;; -----------------------------------------------------------------------------
;;; Gtags
;; (use-package ggtags
;;   :ensure t
;;   :diminish ggtags-mode
;;   :disabled t
;;   :init
;;   (add-hook 'prog-mode-hook 'ggtags-mode))

;; -----------------------------------------------------------------------------
;;; init-tags.el ends here
