;;; init-shell.el --- shell-mode settings
;; -----------------------------------------------------------------------------

;;;  Support for multiple shells and convenient switching between them
(use-package multishell)
(require 'sh-script)

;;;  Support for getting the cwd from the prompt
(require 'dirtrack)

;;;  Show colors in shell windows:
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4" "blue3" "magenta4" "cyan4" "white"])

;;;  General settings for `comint'
(setq comint-prompt-read-only t           ; make the prompt read-only
      comint-scroll-to-bottom-on-input t  ; always insert at the bottom
      comint-scroll-to-bottom-on-output t ; always add output at the bottom
      comint-scroll-show-maximum-output t ; scroll to show max possible output
      comint-completion-autolist t        ; show completion list when ambiguous
      comint-input-ignoredups t           ; no duplicates in command history
      comint-completion-addsuffix t       ; insert space/slash after file completion
      comint-buffer-maximum-size 1024     ; maximum size in lines for Comint buffers.
      )

(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

;; Filter to get the cwd from the prompt
(setq-default dirtrack-list '("^|\\([^|]*\\)|" 1 nil))

(defun dirtrack-filter-out-pwd-prompt (string)
  "Remove the CWD from the prompt."
  (if (and (stringp string) (string-match (first dirtrack-list) string))
      (replace-match "" t t string 0)
    string))

(defun my-shell-mode-hook ()

  ;; Switch-on font-lock
  (font-lock-mode 1)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  (ansi-color-for-comint-mode-on)
  (setq toggle-truncate t)

  ;; Get the cwd from the prompt rather than by tracking 'cd' commands
  (shell-dirtrack-mode -1)
  (dirtrack-mode 1)
  (add-hook 'comint-preoutput-filter-functions
            'dirtrack-filter-out-pwd-prompt t t)

  ;; Show the current directory in the mode-line
  (add-to-list 'mode-line-buffer-identification
               '(:propertize (" " default-directory " ") face dired-directory))
  )

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

;;;  Override the global setting for the tab-key
;;   and reset to the shell-completion
(define-key shell-mode-map [(tab)] 'icicle-comint-dynamic-complete)
;;(define-key shell-mode-map [(tab)] 'comint-dynamic-complete)
(define-key shell-mode-map "\M-p" 'comint-previous-matching-input-from-input)
(define-key shell-mode-map "\M-n" 'comint-next-matching-input-from-input)

;; Better commenting/un-commenting
(define-key shell-mode-map "\C-c\C-c" 'comment-dwim-line)
(define-key sh-mode-map "\C-c\C-c" 'comment-dwim-line)

(defun shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart way.
 If a buffer with a running shell process exists, simply switch to
 that buffer.
 If a shell buffer exists, but the shell process is not running,
 restart the shell.
 If already in an active shell buffer, switch to the next one, if
 any.

 With prefix argument CREATE always start a new shell."
  (interactive "P")
  (let* ((next-shell-buffer
          (catch 'found
            (dolist (buffer (reverse (buffer-list)))
              (when (string-match "^\\*shell\\*" (buffer-name buffer))
                (throw 'found buffer)))))
         (buffer (if create
                     (generate-new-buffer-name "*shell*")
                   next-shell-buffer)))
    (shell buffer)))

;;(global-set-key [f4] 'shell-dwim)

(defun jump-to-compilation-error-in-shell()
  "From a shell buffer, copy the output of the last
command (make, ant, etc.) to a temporary compilation output
buffer and jump to any errors cited in the output using
`compilation-minor-mode'."
  (interactive)
  (assert (eq major-mode 'shell-mode)
          "Can only process compilation errors from a shell buffer")
  (goto-char (point-max))
  (let* ((end (save-excursion (forward-line 0)(point)))
         (start (save-excursion
                  (comint-previous-prompt 1)(forward-line 1)(point)))
         (out (get-buffer-create "*shell-compilation-output*"))
         (output (buffer-substring-no-properties start end))
         (shell-window (get-buffer-window (current-buffer) (selected-frame))))
    (with-current-buffer out
      (erase-buffer)
      (insert "Compilation mode skips the first\n2 lines...\n")
      (setq truncate-lines nil)
      (insert output)
      (compilation-minor-mode 1)
      (goto-char (point-min))
      (display-buffer out shell-window)
      (next-error))))

(define-key shell-mode-map [f9] 'jump-to-compilation-error-in-shell)

;; (use-package readline-complete)
;; (setq explicit-shell-file-name "bash")
;; (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
;; (setq comint-process-echoes t)
;; (push 'company-readline company-backends)
;; (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))

;; --------------------------------------------------------------------------
;;; init-shell.el ends here
