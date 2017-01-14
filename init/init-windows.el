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
;;; Quickly switch windows
;;  Jumps to other window if there are two otherwise the windows are numbered
;;  and selected by number.
(use-package ace-window
  :ensure t
  :commands (ace-window aw-flip-window)
  :bind (("C-x o" . ace-window))
  ;; :bind to a local map doesn't work
  ;; See https://github.com/jwiegley/use-package/issues/332#start-of-content
  :init
  (define-key my-nav-map (kbd "w") 'ace-window)
  (define-key my-nav-map (kbd "f") 'aw-flip-window)
  (add-to-list 'which-key-replacement-alist
               '((nil .  "ace-window") . (nil . "switch 🗔")))
  (add-to-list 'which-key-replacement-alist
               '((nil .  "aw-flip-window") . (nil . "flip 🗔"))))

;; -----------------------------------------------------------------------------
;;; Dim other buffers
(use-package auto-dim-other-buffers
  :ensure t
  :diminish auto-dim-other-buffers-mode
  ;; :init
  ;; (defun auto-dim-on-window-configuration-change ()
  ;;   "Highlight selected window with a different background color."
  ;;   (adob--focus-out-hook)
  ;;   (adob--focus-in-hook))
  ;; (add-hook 'window-configuration-change-hook
  ;;           'auto-dim-on-window-configuration-change)
  :config
  (auto-dim-other-buffers-mode t))

;; -----------------------------------------------------------------------------
;;; init-windows.el ends here
