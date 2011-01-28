;;; init-imaxima.el --- Initialize interface to maxima
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/imaxima"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/imaxima") t)

(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(autoload 'imath-mode "imath" "Interactive Math minor mode." t)

;; -----------------------------------------------------------------------------
;;; init-imaxima.el ends here
