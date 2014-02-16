;;; init-wget.el --- Initialize wget interface
;;;  Asynchronous download of files
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/wget"))
(require 'wget)

(setq wget-download-directory-filter 'wget-download-dir-filter-regexp
      wget-download-directory
      '(("\\.\\(jpe?g\\|png\\)$" . "~/Download/Pictures")
        ("\\.\\(ps\\|pdf\\|doc\\)$" . "~/Download/Documents")
        ("\\.el$" . "~/Download/lisp")
        ("." . "~/Download")))

(defun wget-open (uri &optional arg)
  "Wget interface to open the file download by `wget'.
If argument ARG is non-nil, ask some options.
Called with prefix argument, turn argument ARG t.

If you are in dired mode which is seeing ftp directory,
`wget' regard current line file name as URI."
  (interactive (list
                (if (string= major-mode "dired-mode")
                    (dired-wget)
                  (read-string "URI: " (thing-at-point-url-at-point)))
                (when current-prefix-arg t)))
  (let ((dir (wget-cd-download-dir arg uri)))
    (when dir
      (if (string= uri "")
          (error "There is no uri")
        (let ((filename (concat dir (file-name-nondirectory uri))))
          (if (file-exists-p filename)
              (find-file-other-frame filename)
            (error "File %s does not exist" filename)))))))

;; -----------------------------------------------------------------------------
;;; init-wget.el ends here
