;;; init-tramp.el --- Initialize tramp remote file editing
;; -----------------------------------------------------------------------------

(require 'tramp)

(setq tramp-default-method "ssh" ;; "scp"
      password-cache t
      password-cache-expiry 1000
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
;;;  Using Tramp to open files sudo’d to root
;;    From http://www.emacswiki.org/cgi-bin/wiki/TrampMode

(defvar find-file-root-prefix
  "/sudo:root@localhost:"
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
         (space (+ 6 (- (window-width) (length warning))))
         (bracket (make-string (/ space 2) ?-))
         (warning (concat bracket warning bracket)))
    (setq header-line-format
          (propertize  warning 'face 'find-file-root-header-face))))

(add-hook 'find-file-root-hook 'find-file-root-header-warning)

;; Replace ^x^r find-file-read-only binding with find-file-root
(global-set-key [(control x) (control r)] 'find-file-root)

;; -----------------------------------------------------------------------------
;;; init-tramp.el ends here
