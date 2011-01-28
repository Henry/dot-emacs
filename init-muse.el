;;; init-muse.el  --- Initialize muse-mode
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/muse/lisp"))

(require 'muse-mode)

;;;  Load publishing styles I use
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)

;;;  Publish files in projects
(require 'muse-project)


;;;  Wiki format used by the Emacs Wiki
;;(require 'oddmuse)
;;(oddmuse-mode-initialize)
(require 'yaoddmuse)

(require 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.mkdn" . markdown-mode) auto-mode-alist))

;;;  Wiki format used by Wikipedia
(require 'mediawiki)
(setq auto-mode-alist
      (cons '("\\.wiki" . mediawiki-mode) auto-mode-alist))

;; -----------------------------------------------------------------------------
;;; init-muse.el ends here
