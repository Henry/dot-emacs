;;; init-anything.el ---  Initialize the anything package
;; -----------------------------------------------------------------------------
;;;  Anything: Show things that are now and have happened for selection

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/icicles"))

(require 'anything-config-ext)

(setq anything-c-adaptive-history-file "~/Emacs/anything-c-adaptive-history")

;; -----------------------------------------------------------------------------
;;; Set active list of sources

(setq anything-sources
      '(
        anything-c-source-buffers
        anything-c-source-files-in-current-dir+
        anything-c-source-file-journal
        anything-c-source-elinit
        anything-c-source-switch-dir
        anything-c-source-bm
        anything-c-source-kill-ring
        anything-c-source-emacs-commands
        anything-c-source-call-source
        anything-c-source-no-exact-match-in-buffers
        ))

(defun anything-files ()
  "Create `anything' buffer for file related sources."
  (interactive)
  (anything '(anything-c-source-buffers
              anything-c-source-files-in-current-dir+
              anything-c-source-file-journal
              anything-c-source-no-exact-match-in-buffers)))

(defun anything-complete-kill-ring ()
  "Kill-ring entry selection and insertion using `anything-complete'."
  (interactive)
  (anything-complete 'anything-c-source-kill-ring ""))

(defun anything-info-at-point ()
  "Create `anything' buffer for elisp related sources
with the current `sexp' as the default pattern."
  (interactive)
  (let ((pattern (thing-at-point 'sexp)))
    (anything '(anything-c-source-info-elisp-new
                anything-c-source-info-cl
                anything-c-source-info-pages)
              pattern)))

(defun anything-bbdb ()
  "Create `anything' buffer for BBDB."
  (interactive)
  (anything 'anything-c-source-bbdb))

(defun anything-man ()
  "Create `anything' buffer for man and info pages."
  (interactive)
  (anything '(anything-c-source-info-pages
              anything-c-source-man-pages)))

(defvar anything-c-google-suggest-url
  "http://www.google.co.uk/complete/search?hl=en&js=true&qu="
  "URL used for looking up suggestions.")

(defvar anything-c-google-suggest-search-url
  "http://www.google.co.uk/search?ie=utf-8&oe=utf-8&q="
  "URL used for searching.")

(defun anything-google ()
  "Create `anything' buffer for google search
with the current `sexp' as the default pattern."
  (interactive)
  (let ((pattern (thing-at-point 'sexp)))
    (anything 'anything-c-source-google-suggest pattern)))

(defun anything-locate ()
  "Create `anything' buffer for file location."
  (interactive)
  (anything 'anything-c-source-locate))

(defun anything-etags ()
  "Create `anything' buffer for etags."
  (interactive)
  (anything 'anything-c-source-etags-select))

(defun anything-gtags ()
  "Create `anything' buffer for gtags."
  (interactive)
  (anything 'anything-c-source-gtags-select))

;; -----------------------------------------------------------------------------
;;; Anything keymap

(defvar my-anything-map (make-sparse-keymap "Anything"))
(define-key my-map "a" my-anything-map)
(define-key my-anything-map "a" 'anything-apropos)
(define-key my-anything-map "b" 'anything-bbdb)
(define-key my-anything-map "f" 'anything-files)
(define-key my-anything-map "G" 'anything-google)
(define-key my-anything-map "i" 'anything-info-at-point)
(define-key my-anything-map "k" 'anything-complete-kill-ring)
(define-key my-anything-map "l" 'anything-locate)
(define-key my-anything-map "m" 'anything-man)
(define-key my-anything-map "o" 'anything-traverse)
(define-key my-anything-map "e" 'anything-etags)
(define-key my-anything-map "g" 'anything-gtags)

;; -----------------------------------------------------------------------------
;;; init-anything.el ends here
