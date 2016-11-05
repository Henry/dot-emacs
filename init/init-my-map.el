;;; init-my-map.el --- Personal key-map
;; -----------------------------------------------------------------------------
;;; Make the prefix key `C-z' for my personal keymap.

;; Like `C-x' and `C-c', but the prefix key `C-c' is reserved for mode-specific
;; commands (both user-defined and standard Emacs extensions).

;; On dvorak-keyboards `C-t' is one of the most accessible keys.
;;     but used by stumpwm
;; On qwerty-keyboards `C-z' is one of the most accessible keys.
;;     and good enough on dvorak-keyboards

;; The previous binding of `C-z' is reassigned to double key
;; sequence `C-z C-z'.

(defvar my-map
  (let ((map (make-sparse-keymap))
        (c-z (global-key-binding "\C-z")))
    (global-unset-key "\C-z")
    (define-key global-map "\C-z" map)
    (define-key map "\C-z" c-z)
    map))

;; Set so that my-map can be used from isearch
(define-key isearch-mode-map "\C-t" my-map)

(define-key my-map [(control ?u)] 'my-rotate-windows)
(define-key my-map [(down)] 'duplicate-start-of-line-or-region)
(define-key my-map "i" 'goto-last-change)
(define-key my-map "I" 'goto-last-change-reverse)
(define-key my-map "R" 'refill-mode)
(define-key my-map "f" 'ffap)
(define-key my-map "h" 'hl-line-mode)
(define-key my-map "k" 'kill-buffer-and-frame)
(define-key my-map "l" 'linum-mode)
(define-key my-map "o" 'occur-by-moccur)
(define-key my-map "r" 'revert-buffer)
(define-key my-map "S" 'multishell-pop-to-shell)
(define-key my-map [(control ?s)] 'support)
(define-key my-map "t" 'toggle-truncate-lines)
(define-key my-map "v" 'find-file-other-frame)

(define-key my-map "F" (lambda () (interactive) (font-lock-mode)))
(define-key my-map "H" 'my-goto-home)
(define-key my-map "M" 'menu-bar-mode)
(define-key my-map "T" 'tool-bar-mode)

(define-key my-map "1" 'my-single-frame)
(define-key my-map "2" 'my-double-frame)

;; -----------------------------------------------------------------------------
;;; init-my-map.el  ends here.
