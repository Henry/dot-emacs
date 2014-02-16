;;; init-wiki.el  --- Initialize wiki modes
;; -----------------------------------------------------------------------------

;;;  Wiki format used by the Emacs Wiki
(require 'yaoddmuse)

;;;  Markdown format used by GitHub
(require 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.mkdn" . markdown-mode) auto-mode-alist))

;;;  Wiki format used by Wikipedia
(require 'mediawiki)
(setq auto-mode-alist
      (cons '("\\.wiki" . mediawiki-mode) auto-mode-alist))

;; -----------------------------------------------------------------------------
;;; init-wiki.el ends here
