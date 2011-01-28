;;; init-tramp.el --- Initialize tramp remote file editing
;; -----------------------------------------------------------------------------

(require 'tramp)

(setq tramp-default-method "ssh" ;; "scp"
      tramp-remote-path (split-string (getenv "PATH") ":")
      tramp-process-echos nil
      tramp-auto-save-directory (expand-file-name "~/Emacs/Tramp/autosave")
      tramp-bkup-backup-directory-info
      '(
        (t (expand-file-name "~/Emacs/Tramp/autosave") ok-create full-path)
        ))

(add-to-list 'tramp-default-proxies-alist
             '("well.+" nil "/well:"))

;; -----------------------------------------------------------------------------
;;; init-tramp.el ends here
