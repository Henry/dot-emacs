;;; init-rust.el --- Initialize rust-mode
;; -----------------------------------------------------------------------------
(use-package rust-mode)

;; -----------------------------------------------------------------------------
;;; Mode hook

(defun my-rust-mode-hook ()
  "Hook to apply my setting to the Rust"

  (font-lock-mode 1)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  (show-matching-paren)
  (highlight-parentheses-mode t)

  ;; Switch on fly-spell mode in comments
  (flyspell-prog-mode)
  )

;; -----------------------------------------------------------------------------
;;; Add hook

(add-hook 'rust-mode-hook 'my-rust-mode-hook)

;; -----------------------------------------------------------------------------
;;; init-rust.el ends here
