;;; init-haskell.el --- Initialize haskell-mode
;; -----------------------------------------------------------------------------
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook))

(defun my-haskell-mode-hook ()
  "Hook to apply my setting to the Haskell"

  (font-lock-mode 1)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  (show-matching-paren)
  (highlight-parentheses-mode t)

  ;; Switch on fly-spell mode in comments
  (flyspell-prog-mode)

  (turn-on-haskell-indent)
  (turn-on-haskell-ghci)
  (turn-on-haskell-doc-mode)
  (hs-lint-mode-hook)
  )

;; -----------------------------------------------------------------------------
;;; init-haskell.el ends here
