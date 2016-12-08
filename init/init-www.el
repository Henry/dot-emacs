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
  (setq search-web-default-browser 'eww-browse-url
        search-web-in-emacs-browser 'eww-browse-url
        search-web-external-browser 'browse-url-generic

        search-web-engines
        '(("google" "http://www.google.com/search?q=%s" nil)
          ("duck" "https://duckduckgo.com/?q=%s" nil)
          ("google maps" "http://maps.google.co.uk/maps?&q=%s" External)
          ("google scholar" "https://scholar.google.co.uk/scholar?q=%s" nil)
          ("youtube" "http://www.youtube.com/results?search_type=&search_query=%s&aq=f" External)
          ("twitter" "https://twitter.com/hashtag/%s" External)
          ("OED_ext" "https://en.oxforddictionaries.com/definition/%s" External)
          ("dict" "http://www.dictionary.com/browse/%s?s=t" nil)
          ("answers" "http://www.answers.com/topic/%s" nil)
          ;;("emacswiki" "http://www.google.com/cse?cx=004774160799092323420%%3A6-ff2s0o6yi&q=%s&sa=Search" nil)
          ("amazon" "http://www.amazon.co.uk/s/url=search-alias%%3Daps&field-keywords=%s" External)
          ;;("yahoo" "http://search.yahoo.com/search?p=%s" nil)
          ("wikipedia" "http://www.wikipedia.org/search-redirect.php?search=%s&language=en" External)
          ;;("stackoveflow" "http://stackoverflow.com/search?q=%s" nil)
          ))
  :bind (:map my-map
              ("C-g" . search-web)
              ("M-g" . search-web-dwim)))
weller
;; -----------------------------------------------------------------------------
;;; babel --- interface to web translation services
(use-package babel
  :ensure t)

;; -----------------------------------------------------------------------------
;;; init-www.el ends here
