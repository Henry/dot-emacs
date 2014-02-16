;;; init-yasnippets.el ---  Yet another snippets extension

;; -----------------------------------------------------------------------------
;;; Package path and load

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/yasnippet"))

(require 'yasnippet)

;; -----------------------------------------------------------------------------
;;; Basic initialisation

(yas/initialize)
(yas/load-directory "~/Emacs/YASnippets")

;;;   Switch-off by default
(yas/minor-mode -1)

;;;   Toggle on/off using `C-ty'
(define-key my-map "y" 'yas/minor-mode)

;; -----------------------------------------------------------------------------
;;; init-yasnippets.el ends here
