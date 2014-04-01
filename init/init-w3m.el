;;; init-w3m.el --- Initialize w3m web-browser emacs interface
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/w3m"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/w3m/doc") t)

(require 'w3m)
(require 'w3m-search)
(require 'mime-w3m)
(require 'w3m-session)

(setq w3m-profile-directory "~/Emacs/W3m"
      w3m-icon-directory "~/.emacs.d/packages/w3m/icons"
      w3m-use-cookies t
      w3m-session-file "~/.Emacs/W3m/session"
      w3m-session-save-always nil
      w3m-session-load-always nil
      w3m-session-show-titles t
      w3m-session-duplicate-tabs 'ask
      w3m-default-display-inline-images t
      w3m-display-inline-image t
      )

;;;  append cookie option
(when w3m-use-cookies
  (setq w3m-command-arguments (append '("-cookie") w3m-command-arguments)))

;;;  store cookie when exit
(add-hook 'kill-emacs-hook
          '(lambda ()
             (when w3m-use-cookies
               (w3m-cookie-shutdown))))

;;;  Choose Conkeror as the external browser
(add-to-list 'w3m-content-type-alist
             '("text/html" "\\.s?html?\\'"
               ("conkeror" url) nil))

;;;  Use wget do download files asynchronously
(require 'w3m-wget)

;;;  Assign this code to a key to “preview” a buffer full of HTML in w3m.
(defun w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))

;;;  Add 'f' command to number links like Conkeror and visit the selected link
(require 'w3m-lnum)
(defun my-w3m-visit-linknum ()
  "Turn on link numbers and ask for one to go to."
  (interactive)
  (let ((active w3m-link-numbering-mode))
    (when (not active) (w3m-link-numbering-mode))
    (unwind-protect
        (w3m-move-numbered-anchor (read-number "Anchor number: "))
      (w3m-view-url-with-external-browser)
      (when (not active) (w3m-link-numbering-mode)))))

;;;  Change the functions in the w3m-minor-mode-map used in the
;;   mime html viewer to call the external browser

(defun w3m-mouse-view-url-with-external-browser (event)
  "Follow the link under the mouse pointer."
  (interactive "e")
  (mouse-set-point event)
  (w3m-external-view-this-url))

(define-key w3m-minor-mode-map "g" 'w3m-view-url-with-external-browser)
(define-key w3m-minor-mode-map "f" 'my-w3m-visit-linknum)
(define-key w3m-minor-mode-map [mouse-1] 'w3m-mouse-view-url-with-external-browser)
(define-key w3m-minor-mode-map [down-mouse-1] 'mouse-drag-region)
(define-key w3m-minor-mode-map [down-mouse-2] 'mouse-drag-drag)
(define-key w3m-minor-mode-map [down] 'next-line)
(define-key w3m-minor-mode-map [up] 'previous-line)
(define-key w3m-minor-mode-map [right] 'forward-char)
(define-key w3m-minor-mode-map [left] 'backward-char)

;; -----------------------------------------------------------------------------
;;; init-w3m.el ends here
