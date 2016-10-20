;;; init-magit.el --- Initialize magit interface to git
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name  "~/.emacs.d/packages/magit/lisp"))
(add-to-list 'load-path (expand-file-name  "~/.emacs.d/packages/dash"))
(add-to-list 'load-path (expand-file-name  "~/.emacs.d/packages/with-editor"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/magit/Documentation") t)

(setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")

;;(require 'magit)
;;(require 'magit-blame)

(add-hook 'magit-mode-hook 'font-lock-mode)
(add-hook 'git-commit-mode-hook 'font-lock-mode)

(autoload 'magit-status "magit" "Magit" t)
(autoload 'magit-status "magit-blame" "Magit blame." t)

(define-key my-map "g" 'magit-status)

;; -----------------------------------------------------------------------------
;;; init-magit.el ends here
