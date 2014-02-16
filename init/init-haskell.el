;;; init-haskell.el --- Initialize gnuplot-mode
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/haskell-mode"))
(add-to-list 'Info-default-directory-list
             (expand-file-name "~/.emacs.d/packages/haskell-mode/"))

(require 'haskell-mode-autoloads)

;; -----------------------------------------------------------------------------
;;; Mode hook

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
;;; Add hook

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; -----------------------------------------------------------------------------
;;; init-haskell.el ends here
