;;; init-scad.el --- Initialize scad-mode
;; -----------------------------------------------------------------------------
(use-package scad-mode
  :ensure t)

;; -----------------------------------------------------------------------------
;;; Mode hook

(defun my-scad-mode-hook ()
  "Hook to apply my setting to the scad"

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

(add-hook 'scad-mode-hook 'my-scad-mode-hook)

;; -----------------------------------------------------------------------------
;;; init-scad.el ends here
