;;; init-tags.el --- Initialise all things tags related

;; -----------------------------------------------------------------------------
;;; Always start a new tags table list
(setq tags-add-tables nil)

;; -----------------------------------------------------------------------------
;;; Etags
(require 'etags-select)
(require 'etags-table)

(setq etags-table-alist
      `((".*\\.el$"
         ,(expand-file-name "~/.emacs.d/TAGS"))
        (,(concat OPENFOAM_DIR "/.*\\.[CH]$")
         ,(concat OPENFOAM_TAGS_DIR "/etags"))
        ))

;; -----------------------------------------------------------------------------
;;;  etags-select altered functions

(defun etags-select-find (tagname)
  "Core tag finding function."
  (let ((tag-files (etags-select-get-tag-files))
        (tag-count 0))
    (setq etags-select-source-buffer (buffer-name))
    (get-buffer-create etags-select-buffer-name)
    (set-buffer etags-select-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Finding tag: " tagname "\n")
    (mapcar (lambda (tag-file)
              (setq tag-count (etags-select-insert-matches tagname tag-file tag-count)))
            tag-files)
    (cond ((= tag-count 0)
           (message (concat "No matches for tag \"" tagname "\""))
           (ding))
          ((and (= tag-count 1) etags-select-no-select-for-one-match)
           (setq etags-select-opened-window nil)
           (set-buffer etags-select-buffer-name)
           (goto-char (point-min))
           (etags-select-next-tag)
           (etags-select-goto-tag))
          (t
           (set-buffer etags-select-buffer-name)
           (goto-char (point-min))
           (etags-select-next-tag)
           (set-buffer-modified-p nil)
           (setq buffer-read-only t)
           ;;(setq etags-select-opened-window (selected-window))
           (select-window (split-window-horizontally-or-vertically))
           (switch-to-buffer etags-select-buffer-name)
           (etags-select-mode tagname)))))

(defun etags-select-highlight (beg end)
  "Highlight the tag using an overlay."
  (if (featurep 'xemacs)
      (let ((extent (make-extent beg end)))
        (set-extent-property extent 'face 'etags-select-highlight-tag-face)
        (sit-for etags-select-highlight-delay)
        (delete-extent extent))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'etags-select-highlight-tag-face))))

;; -----------------------------------------------------------------------------
;;;  etags-select additional functions

(defun etags-select-tag-show-other-window ()
  "Select etag tag other window."
  (interactive)
  (save-selected-window (etags-select-goto-tag 4 t))
  (switch-to-buffer etags-select-buffer-name))

(defun etags-select-next-tag-show ()
  "Move next tag line and show at other window."
  (interactive)
  (etags-select-next-tag)
  (etags-select-tag-show-other-window))

(defun etags-select-prev-tag-show ()
  "Move previous tag line and show at other window."
  (interactive)
  (etags-select-previous-tag)
  (etags-select-tag-show-other-window))

;; -----------------------------------------------------------------------------
;;;  etags-select key-bindings

(define-key etags-select-mode-map [(control return)]
  'etags-select-tag-show-other-window)
(define-key etags-select-mode-map [(control down)]
  'etags-select-next-tag-show)
(define-key etags-select-mode-map [(control up)]
  'etags-select-prev-tag-show)

;; -----------------------------------------------------------------------------
;;; Xgtags
(require 'xgtags-extension)

;; -----------------------------------------------------------------------------
;;;  xgtags altered and additional functions

(defun xgtags-goto-tag (tagname flag)
  (let* ((window (selected-window))
         (file-name (buffer-file-name))
         (buffer-dir (and file-name (file-name-directory file-name)))
         (buffer (xgtags-get-buffer)))
    (message "Searching %s ..." tagname)
    (let* ((stack-entry (xgtags-push-context))
           (tags (xgtags-call-global buffer-dir flag tagname))
           (num-tags (length tags)))
      (xgtags-stack-entry-set-current-tags stack-entry tags)
      (message "Searching %s done" tagname)
      (cond
       ((eq num-tags 0)
        (message (cond
                  ((equal flag "P") "%s: path not found")
                  ((equal flag "g") "%s: pattern not found")
                  ((equal flag "I") "%s: token not found")
                  ((equal flag "s") "%s: symbol not found")
                  (t "%s: tag not found"))
                 tagname)
        (xgtags-pop-context))
       (t
        (set-buffer buffer)
        (goto-char (point-min))
        (pop-to-buffer buffer))))))

;; -----------------------------------------------------------------------------
;;;  xgtags-extension key-bindings

(define-key xgtags-select-mode-map [(control return)] 'xgtags-select-tag-other-window)
(define-key xgtags-select-mode-map [down] 'xgtags-select-next-tag-line)
(define-key xgtags-select-mode-map [up] 'xgtags-select-prev-tag-line)
(define-key xgtags-select-mode-map [(control down)] 'xgtags-select-next-tag-line-show)
(define-key xgtags-select-mode-map [(control up)] 'xgtags-select-prev-tag-line-show)

;; -----------------------------------------------------------------------------
;;; Ectags
(add-to-list 'load-path (expand-file-name  "~/.emacs.d/packages/ectags"))
(require 'ectags)

(setq ectags-window-split-function 'split-window-horizontally-or-vertically
      ectags-select-highlight-delay 0)

;; -----------------------------------------------------------------------------
;;; init-tags.el ends here
