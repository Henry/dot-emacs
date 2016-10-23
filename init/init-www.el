;;; init-www.el --- Initialize web-browser interface
;; -----------------------------------------------------------------------------

(setq browse-url-new-window-flag t
      browse-url-generic-program "conkeror"

      mime-view-text/html-previewer 'shr

      ;; Set browse-url to use wget for ftp and conkeror for everything else
      browse-url-browser-function
      '(("^ftp:/.*" . wget)
        ("."    . browse-url-generic)))

(use-package eww-lnum)

;;;  Dummy mailcap functions to stop URL and other packages complaining
(defun mailcap-parse-mailcaps (&optional path force))
(defun mailcap-parse-mimetypes (&optional path force))

;; -----------------------------------------------------------------------------
;;;  Web searches
;;   http://www.emacswiki.org/emacs/SebastienRoccaSerraDotEmacs

(defun my-web-search (base-url what)
  (browse-url
   (concat base-url (url-hexify-string (encode-coding-string what 'utf-8)))))

(defun my-google-search (what)
  (interactive "sGoogle search: ")
  (my-web-search "http://www.google.co.uk/search?q=" what))

(define-key my-map "\C-g" 'my-google-search)

(defun my-emacs-wiki-search (what)
  (interactive "sEmacs Wiki search: ")
  (my-web-search "http://www.emacswiki.org/cgi-bin/wiki?search=" what))

(define-key my-map "\C-w" 'my-emacs-wiki-search)

;; -----------------------------------------------------------------------------
;;;  Google definition code
;;   from autoinfo.el by Tamas Patrovics

(defun my-show-definition (&optional query)
  "Fetch definition from Google for QUERY."
  ;;(interactive (list (read-string "Query: " (current-word))))
  ;;(unless query (setq query (read-string "Query: ")))
  (interactive)
  (unless query (setq query (current-word)))
  (message "Fetching definition from Google...")
  (condition-case err
      (url-retrieve
       (concat "http://www.google.com/search?q=define:"
               (url-hexify-string query))
       'my-handle-google-response)
    (error (message "Error when getting info: %s" (error-message-string err)))))

(defun my-handle-google-response (status)
  "Handle response returned by Google."
  (message "")
  (let ((response (buffer-string)))
    (tooltip-help-show-for-point
     (if (string-match "Definitions of <b>\\(.*?\\)</b>" response)
         (let ((query (match-string 1 response)))
           (if (string-match "Definitions of.*?\\(<li>.*?\\)<br>" response)
               (let ((results (match-string 1 response)))
                 (concat "Definitions of `" query "' by Google:\n"
                         (replace-regexp-in-string "<li>" "\n- " results)))
             "No definition found"))
       "Parse error"
       ))))

(define-key my-map [f1] 'my-show-definition)

;; -----------------------------------------------------------------------------
;;;  browse-apropos-url
;;   from http://www.emacswiki.org/emacs/BrowseAproposURL

(setq apropos-url-alist
      '(("^gw?:? +\\(.*\\)" . ;; Google Web
         "http://www.google.com/search?q=\\1")

        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")

        ("^gl:? +\\(.*\\)" .  ;; Google Linux
         "http://www.google.com/linux?q=\\1")

        ("^gi:? +\\(.*\\)" . ;; Google Images
         "http://images.google.com/images?sa=N&tab=wi&q=\\1")

        ("^gg:? +\\(.*\\)" . ;; Google Groups
         "http://groups.google.com/groups?q=\\1")

        ("^gd:? +\\(.*\\)" . ;; Google Directory
         "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=\\1")

        ("^gn:? +\\(.*\\)" . ;; Google News
         "http://news.google.com/news?sa=N&tab=dn&q=\\1")

        ;; Google Translate URL
        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" .
         "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")

        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ;; Google Translate Text
         "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")

        ("^/\\.$" . ;; Slashdot
         "http://www.slashdot.org")

        ("^/\\.:? +\\(.*\\)" . ;; Slashdot search
         "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")

        ("^fm$" . ;; Freshmeat
         "http://www.freshmeat.net")

        ("^ewiki:? +\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")

        ("^ewiki$" . ;; Emacs Wiki
         "http://www.emacswiki.org")

        ))

(defun browse-apropos-url (text &optional new-window)
  (interactive (browse-url-interactive-arg "Location: "))
  (let ((text (replace-regexp-in-string
               "^ *\\| *$" ""
               (replace-regexp-in-string "[ \t\n]+" " " text))))
    (let ((url (assoc-default
                text apropos-url-alist
                '(lambda (a b) (let () (setq __braplast a) (string-match a b)))
                text)))
      (browse-url (replace-regexp-in-string __braplast url text) new-window))))

;; -----------------------------------------------------------------------------
;;;   Here is some wrapper code to facilitate access to the apropos browse using
;;    a region/word at point or a prompt.

(defun rgr/region-then-thing-at-point()
  "Function to return the currently selected region.
If no region is selected then return the word at the cursor."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)(region-end))
    (progn
      (let ((word (current-word)))
        (if (zerop (length word))
            (setq word "default"))
        word))))


(defun rgr/google(term)
  "Call google search for the specified term.
Do not call if string is zero length."
  (if (not (zerop (length term)))
      (browse-apropos-url (concat "gw: " term))
    (browse-apropos-url  "http://www.google.com ")
    ))

(defun rgr/google-search-auto()
  (let ((word (rgr/region-then-thing-at-point)))
    (rgr/google word )))

(defun rgr/google-search-prompt(term)
  "Prompt user to query google search for term.
Term is word at point or the selcted region"
  (interactive (list (unless (eq current-prefix-arg 0)
                       (read-string "Google for word : "
                                    (rgr/region-then-thing-at-point)))))
  (rgr/google term))


(define-key my-map (kbd "<f3>") 'browse-apropos-url)
(define-key my-map (kbd "C-<f3>") (lambda()(interactive)(rgr/google-search-auto)))
(define-key my-map (kbd "M-<f3>") 'rgr/google-search-prompt)

;; -----------------------------------------------------------------------------
;;; init-www.el ends here
