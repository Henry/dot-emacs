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
         ,(expand-file-name "~/.emacs.d/TAGS")
         ,(expand-file-name "~/.emacs.d/systemTAGS"))
        (,(concat OPENFOAM_DIR "/.*\\.[CH]$")
         ,(concat OPENFOAM_TAGS_DIR "/etags"))
        ))

(add-hook 'etags-select-mode-hook
          '(lambda ()
             (font-lock-mode 1)
             (hl-line-mode 1)))

;;binding the key
;; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
;; (global-set-key "\M-." 'etags-select-find-tag)

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

(define-key xgtags-select-mode-map
  [(control return)] 'xgtags-select-tag-other-window)
(define-key xgtags-select-mode-map
  [down] 'xgtags-select-next-tag-line)
(define-key xgtags-select-mode-map
  [up] 'xgtags-select-prev-tag-line)
(define-key xgtags-select-mode-map
  [(control down)] 'xgtags-select-next-tag-line-show)
(define-key xgtags-select-mode-map
  [(control up)] 'xgtags-select-prev-tag-line-show)

;; -----------------------------------------------------------------------------
;;; init-tags.el ends here
