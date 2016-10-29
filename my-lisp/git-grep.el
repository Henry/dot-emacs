(require 'grep)

(defvar git-grep-history nil)

(defun git-grep (regexp &optional files dir)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `git-grep-find-command'.

Collect output in a buffer.  While find runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error]
in the grep output buffer, to go to the lines where grep found matches."
  (interactive
   (cond
    ((equal current-prefix-arg '(16))
     (list (read-from-minibuffer "Run: " "git grep "
                                 nil nil 'git-grep-history)
           nil))
    (t (let* ((regexp (grep-read-regexp))
              (files (grep-read-files regexp))
              (dir (read-directory-name "Base directory: "
                                        nil default-directory t)))
         (list regexp files dir)))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (if (null files)
        (if (not (string= regexp grep-find-command))
            (compilation-start regexp 'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (let ((command (concat
                      "git --no-pager grep -n --no-color -i "
                      "-e " (shell-quote-argument regexp)
                      (if (string= files "*")
                          ""
                        (concat " -- " (shell-quote-argument files))))))
        (when command
          (if current-prefix-arg
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'git-grep-history))
            (add-to-history 'git-grep-history command))
          (let ((default-directory dir))
            (compilation-start (concat "PAGER= " command) 'grep-mode))
          ;; Set default-directory if we started rgrep in the *grep* buffer.
          (if (eq next-error-last-buffer (current-buffer))
              (setq default-directory dir)))))))

(provide 'git-grep)
;;; git-grep.el ends here
