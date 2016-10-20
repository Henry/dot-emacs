;;; init-nim.el --- Initialize settings for NIM development
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/nim-mode"))
(add-to-list 'Info-default-directory-list
             (expand-file-name "~/.emacs.d/packages/nim-mode/"))
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/emacs-epc"))
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/emacs-deferred"))
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/emacs-ctable"))


(require 'nim-mode)

(setq nim-nimsuggest-path "/home/dm2/henry/.nimble/bin/nimsuggest")

;; -----------------------------------------------------------------------------
;;; Mode hook

(defun my-nim-mode-hook ()
  "Hook to apply my setting to the NIM"

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

(add-hook 'nim-mode-hook 'my-nim-mode-hook)

;; -----------------------------------------------------------------------------
;;; init-nim.el ends here
