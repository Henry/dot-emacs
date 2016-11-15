;;; init-ediff.el --- Initialize ediff
;; -----------------------------------------------------------------------------

(require 'ediff)

(defun ediff-frame-2 ()
  "Create a frame wide enough for two windows side-by-side and set them
so that they scroll together"
  (select-frame
   (make-frame `((height . 60) (width . 165) (top . 0) (left . 0))))
  (scroll-all-mode))

(add-hook 'ediff-before-setup-hook 'ediff-frame-2)
(add-hook 'ediff-quit-hook 'delete-frame t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

; Ignore small changes in the white space
(setq ediff-diff-options "-w")
(setq-default ediff-ignore-similar-regions t)

;; Turn off word wrap
(add-hook 'ediff-mode-hook '(lambda () (setq truncate-lines t)))

;; -----------------------------------------------------------------------------
;;; diff settings

(require 'diff-mode)
(defun diff-2-ediff ()
  "invoke ediff on the context of 2 files in diff-mode"
  (interactive)
  ;; A
  (destructuring-bind (buf-A line-offset pos old new &optional switched)
      (diff-find-source-location 't nil)
    ;; B
    (destructuring-bind (buf-B line-offset pos old new &optional switched)
        (diff-find-source-location nil nil)
      (ediff-buffers buf-A buf-B))))

(define-key diff-mode-map [f11] 'diff-hunk-next)
(define-key diff-mode-map [f12] 'diff-2-ediff)

;; -----------------------------------------------------------------------------
;;; Ediff-trees settings

(require 'ediff-trees)

;(global-set-key (kbd "s-SPC") 'ediff-trees-examine-next)
;(global-set-key (kbd "S-s-SPC") 'ediff-trees-examine-previous)
;(global-set-key (kbd "C-s-SPC") 'ediff-trees-examine-next-regexp)
;(global-set-key (kbd "C-S-s-SPC") 'ediff-trees-examine-previous-regexp)

;; -----------------------------------------------------------------------------
;;; init-ediff ends here
