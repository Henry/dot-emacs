;;; init-isearch.el --- Isearch configuration and extensions
;; -----------------------------------------------------------------------------
;;; isearch+, isearch-all --- Isearch enhancements
(use-package isearch+)
(eval-after-load "isearch" '(require 'isearch+))
(eval-after-load "isearch" '(require 'isearch-all))

;; -----------------------------------------------------------------------------
;;; isearch word from beginning of word

(require 'thingatpt)

(defun isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition
   'my-isearch-yank-word-or-char-from-beginning
   'isearch-yank-word-or-char
   isearch-mode-map))

(add-hook 'isearch-mode-hook
          (lambda ()
            "Activate my customized Isearch word yank command."
            (substitute-key-definition
             'isearch-yank-word-or-char
             'isearch-yank-word-or-char-from-beginning
             isearch-mode-map)))

;; -----------------------------------------------------------------------------
;;; isearch symbol or region

(defun isearch-yank-symbol-or-region ()
  "Put region if active otherwise symbol at current point into search string."
  (interactive)
  (let ((sym
         (if mark-active
             (buffer-substring (region-beginning) (region-end))
           (symbol-name (symbol-at-point)))))
    (if sym
        (progn
          (setq isearch-regexp nil
                isearch-string sym
                isearch-message
                (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol-or-region)

;; -----------------------------------------------------------------------------
;;; zap-to-isearch

(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
  the isearch match string. The behaviour is meant to be analogous
  to zap-to-char; let's call it zap-to-isearch. The deleted region
  does not include the isearch word. This is meant to be bound only
  in isearch mode.

  The point of this function is that oftentimes you want to delete
  some portion of text, one end of which happens to be an active
  isearch word. The observation to make is that if you use isearch
  a lot to move the cursor around (as you should, it is much more
  efficient than using the arrows), it happens a lot that you could
  just delete the active region between the mark and the point, not
  include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

(defun isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
  This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; -----------------------------------------------------------------------------
;;; init-isearch.el  ends here.
