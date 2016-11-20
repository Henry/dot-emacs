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

;;;  Put column number into modeline
(column-number-mode 1)

;;;  Fill to column 80
(setq fill-column 80
      word-wrap t)

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
  :disabled t
  :init
  (setq window-number-active-background "grey75"
        window-number-inactive-background "grey75"
        window-number-active-foreground "black"
        window-number-inactive-foreground "black")
  :config
  ;; Define \M-t 1 to switch to win 1, etc (\M-t 0 = win 10)
  ;; Note: space after M-t is important
  (window-number-define-keys window-number-mode-map "M-t ")
  (window-number-mode 1))

(use-package wn-mode
  :ensure t
  :init
  (setq wn-keybinding-format "M-t %s")
  :config
  (wn-mode))

;; -----------------------------------------------------------------------------
;;; Speed-up rendering on Emacs-24
(setq-default bidi-display-reordering nil)

;; -----------------------------------------------------------------------------
;;; Dim other buffers
(use-package auto-dim-other-buffers
  :ensure t
  :diminish auto-dim-other-buffers-mode
  :config
  (auto-dim-other-buffers-mode t))

;; -----------------------------------------------------------------------------
;;; init-windows.el ends here
