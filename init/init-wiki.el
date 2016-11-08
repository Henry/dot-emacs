;;; init-wiki.el  --- Initialize wiki modes
;; -----------------------------------------------------------------------------

;;;  Wiki format used by the Emacs Wiki
(use-package yaoddmuse
  :ensure t)

;;;  Markdown format used by GitHub
(use-package markdown-mode
  :ensure t)
(setq auto-mode-alist
      (cons '("\\.mkdn" . markdown-mode) auto-mode-alist))

;;;  Wiki format used by Wikipedia
(use-package mediawiki
  :ensure t)
(setq auto-mode-alist
      (cons '("\\.wiki" . mediawiki-mode) auto-mode-alist))

;; -----------------------------------------------------------------------------
;;; init-wiki.el ends here
