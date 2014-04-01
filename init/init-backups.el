;;; init-backups.el --- Initialize controls for backup files
;; -----------------------------------------------------------------------------
;; Reworked version of the standard backup mechanism with now stores backup files
;; with version numbers but without `~'s in a mirror of the original file's
;; directory hierarchy

(setq backup-directory "~/.Emacs/Backups"
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; -----------------------------------------------------------------------------
;;; Local versions of the functions in files.el

(defun make-backup-file-name (file)
  "Return the path of the backup file for the given FILE
with the path of the backup file mirroring that of the given FILE.
If the new path's directories do not exist, create them.
This replaces the version in `files.el'"
  (let ((bfile (concat backup-directory file)))
    (make-directory (file-name-directory bfile) bfile)
    bfile))

(defun find-backup-file-name (fn)
  "Find a file name for a backup file FN, and suggestions for deletions.
Value is a list whose car is the name for the backup file
and whose cdr is a list of old versions to consider deleting now.
If the value is nil, don't make a backup.
Uses `backup-directory-alist' in the same way as does
`make-backup-file-name'.
This replaces the version in `files.el'."
  (let ((handler (find-file-name-handler fn 'find-backup-file-name)))
    ;; Run a handler for this function so that ange-ftp can refuse to do it.
    (if handler
        (funcall handler 'find-backup-file-name fn)
      (if (or (eq version-control 'never)
              ;; We don't support numbered backups on plain MS-DOS
              ;; when long file names are unavailable.
              (and (eq system-type 'ms-dos)
                   (not (msdos-long-file-names))))
          (list (make-backup-file-name fn))
        (let* ((basic-name (make-backup-file-name fn))
               (base-versions (concat (file-name-nondirectory basic-name) "."))
               (backup-extract-version-start (length base-versions))
               (high-water-mark 0)
               (number-to-delete 0)
               possibilities deserve-versions-p versions)
          (condition-case ()
              (setq possibilities (file-name-all-completions
                                   base-versions
                                   (file-name-directory basic-name))
                    versions (sort (mapcar #'backup-extract-version
                                           possibilities)
                                   #'<)
                    high-water-mark (apply 'max 0 versions)
                    deserve-versions-p (or version-control
                                           (> high-water-mark 0))
                    number-to-delete (- (length versions)
                                        kept-old-versions
                                        kept-new-versions
                                        -1))
            (file-error (setq possibilities nil)))
          (if (not deserve-versions-p)
              (list (make-backup-file-name fn))
            (cons (format "%s.%d" basic-name (1+ high-water-mark))
                  (if (and (> number-to-delete 0)
                           ;; Delete nothing if there is overflow
                           ;; in the number of versions to keep.
                           (>= (+ kept-new-versions kept-old-versions -1) 0))
                      (mapcar (lambda (n)
                                (format "%s.%d" basic-name n))
                              (let ((v (nthcdr kept-old-versions versions)))
                                (rplacd (nthcdr (1- number-to-delete) v) ())
                                v))))))))))

(defun file-newest-backup (filename)
  "Return most recent backup file for FILENAME or nil if no backups exist.
This is adapted from `file-newest-backup' in `files.el' with the argument to
`backup-file-name-p' being the full path of the file to allow this to be checked to
see if it contains the `backup-directory' in which case it is a backup.
This replaces the version in `files.el'."
  ;; `make-backup-file-name' will get us the right directory for
  ;; ordinary or numeric backups.  It might create a directory for
  ;; backups as a side-effect, according to `backup-directory-alist'.
  (let* ((filename (file-name-sans-versions
                    (make-backup-file-name (expand-file-name filename))))
         (file (file-name-nondirectory filename))
         (dir  (file-name-directory    filename))
         (comp (file-name-all-completions file dir))
         (newest nil)
         tem)
    (while comp
      (setq tem (concat dir (pop comp)))
      (cond ((and (backup-file-name-p tem)
                  (string= (file-name-sans-versions tem) filename))
             (if (or (null newest)
                     (file-newer-than-file-p tem newest))
                 (setq newest tem)))))
    newest))

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This replaces the version in `files.el'."
  (string-match backup-directory file))

(defun file-name-sans-versions (name &optional keep-backup-version)
  "Return file NAME sans backup versions.
If the optional argument KEEP-BACKUP-VERSION is non-nil,
we do not remove backup version numbers, only true file version numbers.
This replaces the version in `files.el'."
  (let ((handler (find-file-name-handler name 'file-name-sans-versions)))
    (if handler
        (funcall handler 'file-name-sans-versions name keep-backup-version)
         (substring name 0
                    (if keep-backup-version
                       (length name)
                      (or (and (backup-file-name-p name)
                               (string-match "\\.[-[:alnum:]:#@^._]+\\'" name))
                          (length name)))))))

(defun backup-extract-version (fn)
  "Given the name of a numeric backup file, FN, return the backup number.
Uses the free variable `backup-extract-version-start', whose value should be
the index in the name where the version number begins.
This replaces the version in `files.el'."
  (if (and (string-match "[0-9]+/?$" fn backup-extract-version-start)
           (= (match-beginning 0) backup-extract-version-start))
      (string-to-number (substring fn backup-extract-version-start))
    0))

;; -----------------------------------------------------------------------------
;;; init-backups.el ends here
