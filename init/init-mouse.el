;;; init-mouse.el --- Initialize mouse settings
;; -----------------------------------------------------------------------------

;;; Use the X primary selection for copy/paste
;; (setq select-enable-clipboard t
;;       select-enable-primary nil
;;       mouse-drag-copy-region nil)

;;; Set ^C-v to yank
(global-set-key "\C-v" 'yank)

;;; mouse-yank-at-point
;; Set the middle-mouse button to paste at the selected point not at the
;; at the 'click'

(setq mouse-yank-at-point t)

;; -----------------------------------------------------------------------------
;;; Move the mouse out of the way

;;(require 'avoid)
;; Move the mouse to the lower-right corner instead of default upper-right
;;(defun mouse-avoidance-banish-destination ()
;;  (cons (+ 3 (frame-width)) (frame-height)))
;;(mouse-avoidance-mode nil)

;; -----------------------------------------------------------------------------
;;;  Show the text pointer in text areas
(setq void-text-area-pointer nil)

;; -----------------------------------------------------------------------------
;;; Mouse drag text in window

(require 'mouse-drag)

;;;  Grab and move text
(global-set-key [down-mouse-2] 'mouse-drag-drag)

;;;  Move and accelerate according to mouse displacement
(global-set-key [(shift down-mouse-2)] 'mouse-drag-throw)

;;;   Change the direction of the throw-scrolling to be consistent with conkeror
(setq mouse-throw-with-scroll-bar t)

(defun mouse-drag-should-do-col-scrolling ()
  "Disallow horizontal scrolling."
  nil)

;; -----------------------------------------------------------------------------
;;; Better mouse copy functionaity

(require 'mouse-copy)

;;;  Mouse mark-copy-paste in one drag-action
(global-set-key [C-down-mouse-1] 'mouse-drag-secondary-pasting)

;;;  Mouse mark-cut-paste in one drag-action
(global-set-key [C-S-down-mouse-1] 'mouse-drag-secondary-moving)

(defun mouse-drag-secondary-kill (start-event)
  "Sweep out a secondary selection, then kill it."
  (interactive "e")
  ;; HACK:  We assume that mouse-drag-secondary returns nil if
  ;; there's no secondary selection.  This works as of emacs-19.22.
  ;; It's not clear that there's any other way to get this information.
  (if (mouse-drag-secondary start-event)
      (progn
    (mouse-kill-preserving-secondary)))
)

;;;  Mouse mark-cut in one drag-action
(global-set-key [C-down-mouse-3] 'mouse-drag-secondary-kill)

;; -----------------------------------------------------------------------------
;;; init-mouse.el ends here
