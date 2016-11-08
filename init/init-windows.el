;;; init-windows.el --- Window navigation
;; -----------------------------------------------------------------------------

(defvar split-window-threshold 160
  "Frame width above which the frame is split horizontally
rather than vertically.")

(defun split-window-horizontally-or-vertically ()
  "Split the window horizontally if the `frame-width' is larger than
`split-window-threshold' otherwise split it vertically."
  (interactive)
  (if (and (one-window-p t)
           (not (active-minibuffer-window)))
      (if (> (frame-width) split-window-threshold)
          (split-window-horizontally)
        (split-window-vertically))
    (selected-window)))

(add-hook 'temp-buffer-setup-hook 'split-window-horizontally-or-vertically)

;;;  Set the scroll-bar to be on the right
(set-scroll-bar-mode 'right)

;;;  Put column number into modeline
(column-number-mode 1)

;;; Switch buffers between windows

(defun my-rotate-windows ()
   "Switch buffers between windows"
   (interactive)
   (let ((this-buffer (buffer-name)))
     (other-window -1)
     (let ((that-buffer (buffer-name)))
       (switch-to-buffer this-buffer)
       (other-window 1)
       (switch-to-buffer that-buffer)
       (other-window -1))))

;; -----------------------------------------------------------------------------
;;; Bind \M-t<arrow-keys> to navigate between windows

(require 'windmove)

;; Create a keymap for window navigation
(defvar my-win-nav-map
  (let ((map (make-sparse-keymap))
        (M-t (global-key-binding "\M-t")))
    (global-unset-key "\M-t")
    (define-key global-map "\M-t" map)
    (define-key map "\M-t" M-t)
    map))

(define-key my-win-nav-map [(left)]  'windmove-left)
(define-key my-win-nav-map [(right)] 'windmove-right)
(define-key my-win-nav-map [(up)]    'windmove-up)
(define-key my-win-nav-map [(down)]  'windmove-down)

;; -----------------------------------------------------------------------------
;;; Number the windows and bind \M-t[1-9] to switch between them

(use-package window-number
  :ensure t
  :init
  (setq window-number-inactive-background "dark slate blue")
  :config
  ;; Define \M-t 1 to switch to win 1, etc (\M-t 0 = win 10)
  ;; Note: space after M-t is important
  (window-number-define-keys window-number-mode-map "M-t ")
  (window-number-mode 1))

;;; Speed-up rendering on Emacs-24
(setq-default bidi-display-reordering nil)

;; -----------------------------------------------------------------------------
;;; init-windows.el ends here
