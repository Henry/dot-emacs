;;; init-yasnippets.el ---  Yet another snippets extension
;; -----------------------------------------------------------------------------
(use-package yasnippet
  :config
  (progn
    (yas/initialize)
    (yas/load-directory "~/Emacs/YASnippets")
    ;;   Switch-off by default
    (yas/minor-mode -1)))

;;;   Toggle on/off using `C-zy'
(define-key my-map "y" 'yas-minor-mode)

;; -----------------------------------------------------------------------------
;;; init-yasnippets.el ends here
