;;; init-www.el --- Initialize web-browser interface
;; -----------------------------------------------------------------------------
(setq browse-url-new-window-flag t
      browse-url-generic-program "conkeror"
      mime-view-text/html-previewer 'shr

      ;; Set browse-url to use wget for ftp and conkeror for everything else
      browse-url-browser-function
      '(("^ftp:/.*" . wget)
        ("."    . browse-url-generic)))

(use-package eww-lnum
  :ensure t)

;;;  Dummy mailcap functions to stop URL and other packages complaining
(defun mailcap-parse-mailcaps (&optional path force))
(defun mailcap-parse-mimetypes (&optional path force))

;; -----------------------------------------------------------------------------
;;;  Web searches
(use-package search-web
  :ensure t
  :init
  (setq search-web-default-browser 'browse-url-generic
        search-web-in-emacs-browser 'eww-browse-url
        search-web-external-browser 'browse-url-generic)
  :bind (:map my-map
              ("C-g" . search-web)
              ("M-g" . search-web-dwim)))

;; -----------------------------------------------------------------------------
;;; babel --- interface to web translation services
(use-package babel
  :ensure t)

;; -----------------------------------------------------------------------------
;;; init-www.el ends here
