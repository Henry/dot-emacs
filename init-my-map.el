;;; init-my-map.el --- Personal key-map
;; -----------------------------------------------------------------------------
;; Like `C-x' and `C-c', but the prefix key `C-c' is reserved for mode-specific
;; commands (both user-defined and standard Emacs extensions).
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

;; -----------------------------------------------------------------------------
;;; init-my-map.el  ends here.
