;;; init-window-nav.el --- Window navigation
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
  :init
  (setq window-number-inactive-background "dark slate blue")
  :config
  ;; Define \M-t 1 to switch to win 1, etc (\M-t 0 = win 10)
  ;; Note: space after M-t is important
  (window-number-define-keys window-number-mode-map "M-t ")
  (window-number-mode 1))

;; -----------------------------------------------------------------------------
;;; init-window-nav.el ends here
