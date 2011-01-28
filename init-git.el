;;; init-git.el --- Initialize git interface
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name  "~/.emacs.d/packages/git-emacs"))
(load "git-emacs")

(add-to-list 'load-path (expand-file-name  "~/.emacs.d/packages/gitsum"))
(load "gitsum")

(define-key my-map "g" 'git-status)

(defconst git-command-string "git")

(defun git-push-pull (cmd)
  "Push/Pull the current repository displaying the output in a comint buffer."
  (let* ((args (read-string "Git command: " cmd))
         (gc-name (concat "git-" cmd))
         (gc-buf (concat "*" gc-name "*")))
    (switch-to-buffer gc-buf)
    (insert "Running " git-command-string " " args "...\n\n")
    (switch-to-buffer (make-comint gc-name git-command-string nil args))))

(defun git-push ()
  "Pull the current repository."
  (interactive)
  (git-push-pull "push"))

(defun git-pull ()
  "Pull the current repository."
  (interactive)
  (git-push-pull "pull"))

(define-key git--status-mode-map [C-down] 'git-push)
(define-key git--status-mode-map [C-up] 'git-pull)

(easy-menu-add-item gitemacs-menu nil
  `("Push/Pull"
    ["Push" git-push t]
    ["Pull" git-pull t]
    )
  "Git Command")

;; -----------------------------------------------------------------------------
;;; init-git.el ends here
