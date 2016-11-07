;;; init-projectile.el --- Initialize git project management
;; -----------------------------------------------------------------------------

(use-package projectile
  :ensure counsel-projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode)
  (counsel-projectile-on)

  (defun projectile-select-files (project-files &optional arg)
    "Select a list of files based on filename at point.
The filename is pre-pended with a '/' and searching is case-sensitive.

With a prefix ARG invalidates the cache first."
    (projectile-maybe-invalidate-cache arg)
    (let* ((case-fold-search nil)
           (file (if (region-active-p)
                     (buffer-substring (region-beginning) (region-end))
                   (or (thing-at-point 'filename) "")))
           (file (if (string-match "\\.?\\./" file)
                     (file-relative-name (file-truename file)
                                         (projectile-project-root))
                   file))
           (file (concat "/" file))
           (files (if file
                      (cl-remove-if-not
                       (lambda (project-file)
                         (string-match file project-file))
                       project-files)
                    nil)))
      files)
    ))

;; -----------------------------------------------------------------------------
;;; init-projectile.el ends here
