;;; init-tags.el --- Initialise all things tags related
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;; Etags
(use-package etags-select
  :init
  (add-hook 'etags-select-mode-hook
            '(lambda ()
               (font-lock-mode 1)
               (hl-line-mode 1)))
  :bind (("\M-?" . etags-select-find-tag-at-point)
         ("\M-." . etags-select-find-tag)))

(use-package etags-table
  :init
  (setq tags-add-tables nil
        etags-table-alist
        `((".*\\.el$"
           ,(expand-file-name "~/.emacs.d/TAGS")
           ,(expand-file-name "~/.emacs.d/systemTAGS"))
          (,(concat OPENFOAM_DIR "/.*\\.[CH]$")
           ,(concat OPENFOAM_TAGS_DIR "/etags"))
          )))

;; -----------------------------------------------------------------------------
;;; Gtags
(use-package ggtags
  :diminish ggtags-mode
  :init
  (add-hook 'prog-mode-hook 'ggtags-mode))

;; -----------------------------------------------------------------------------
;;; init-tags.el ends here
