;;; init-goofie.el --- Initialize settings for GOOFIE development
;; -----------------------------------------------------------------------------

(require 'goofie)

;; -----------------------------------------------------------------------------
;;; Mode hook

(defun my-goofie-mode-hook ()
  "Hook to apply my setting to the GOOFIE"

  (font-lock-mode 1)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  (show-matching-paren)
  (highlight-parentheses-mode t)

  ;; Switch on fly-spell mode in comments
  (flyspell-prog-mode)

  ;; Turn on outline minor mode by default
  (outline-minor-mode +1)
  )

;; -----------------------------------------------------------------------------
;;; Add hook

(add-hook 'goofie-mode-hook 'my-goofie-mode-hook)

;; -----------------------------------------------------------------------------
;;; init-goofie.el ends here
