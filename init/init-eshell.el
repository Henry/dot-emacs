;;; init-eshell.el --- Initialize eshell
;; -----------------------------------------------------------------------------
(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (require 'em-alias)
  (require 'em-basic)
  (require 'em-cmpl)
  (require 'em-dirs)
  (require 'em-glob)
  (require 'em-hist)
  (require 'em-ls)
  (require 'em-pred)
  (require 'em-prompt)
  (require 'em-script)
  (require 'em-term)
  (require 'em-unix)
  (require 'esh-opt)

  ;; Extra alias functions
  (require 'em-xtra)

  ;;  Additional module which provides access to
  ;;  previous command arguments using `C-c .', or `M-.'
  (require 'em-last)

  (require 'pcomplete)

  (use-package esh-help
    :ensure t)

  :init
  (setq eshell-directory-name (expand-file-name "Eshell/" user-emacs-directory)
        eshell-aliases-file (concat eshell-directory-name "alias")
        eshell-history-file-name (concat eshell-directory-name "history")
        eshell-save-history-on-exit t
        eshell-hist-ignoredups t
        eshell-prefer-lisp-functions t
        eshell-scroll-to-bottom-on-input t
        eshell-buffer-shorthand t
        eshell-no-grep-available nil

        eshell-show-lisp-completions nil
        eshell-cmpl-expand-before-complete t
        eshell-cmpl-cycle-completions t
        eshell-cmpl-cycle-cutoff-length 5

        shell-prompt-pattern "^[^)>]*[)>] "
        eshell-prompt-regexp shell-prompt-pattern
        eshell-prompt-function
        (lambda ()
          (if (or (= (user-uid) 0)
                  (string-match "^/su\\(do\\)?:" default-directory))
              (format "MrBig<%d> " (ring-length eshell-history-ring))
            (format "%s(%d) "
                    (getenv "HOSTNAME") (ring-length eshell-history-ring))
            ))

        ;; Set the list of modules to be loaded
        eshell-modules-list
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          ;; eshell-smart ;; Causes slow redraw
          eshell-term
          eshell-unix
          eshell-last
          eshell-xtra
          ))

  ;; ---------------------------------------------------------------------------
  ;; Setup tooltip-help for the eshell (for elisp expressions only)
  (require 'tooltip-help)
  (setq tooltip-help-max-tooltip-lines 30)

  (defun tooltip-help-eshell-mode-handler ()
    "Set the tooltip-help handler for the eshell mode to that for elisp."
    (tooltip-help-emacs-lisp-mode-handler))

  (require 'elisp-mode)
  (defalias 'eldoc-get-fnsym-args-string 'elisp-get-fnsym-args-string)

  (add-hook 'eshell-mode-hook 'my-eshell-mode-hook)
  (setup-esh-help-eldoc))

;; -----------------------------------------------------------------------------
;;; Set eshell-mode hook
(defun my-eshell-mode-hook ()
  "Hook to apply my setting to the eshell"

  ;; Update the eshell path from the current PATH
  (setq eshell-path-env (getenv "PATH"))

  (font-lock-mode)

  (defadvice eldoc-fnsym-in-current-sexp
      (around eldoc-fnsym-in-current-sexp-or-command activate)
    ad-do-it
    (if (and (not ad-return-value)
             (eq major-mode 'eshell-mode))
        (save-excursion
          (goto-char eshell-last-output-end)
          (setq ad-return-value (eldoc-current-symbol)))))

  (defadvice eldoc-current-symbol (around eldoc-current-symbol activate)
    ad-do-it
    (if (and (not ad-return-value)
             (eq major-mode 'eshell-mode))
        (save-excursion
          (goto-char eshell-last-output-end)
          (let ((esym (eshell-find-alias-function (current-word)))
                (sym (intern-soft (current-word))))
            (setq ad-return-value (or esym sym))))))

  (turn-on-eldoc-mode)

  ;; For some reason use-package :bind :map does not work for eshell
  (local-set-key (kbd "C-a") 'eshell-bol)
  (local-set-key [end] 'eshell-show-maximum-output)
  (local-set-key [home] 'eshell-previous-prompt)
  (local-set-key [f1] 'tooltip-help-mode-show)
  (local-set-key [S-f1] 'describe-variable-or-function)
  (local-set-key [(meta ?.)] 'eshell-insert-previous-argument)

  (local-set-key [up] 'previous-line)
  (local-set-key [down] 'next-line)

  ;; Company needs a pcomplete backend for eshell
  (company-mode 1)
  (local-set-key (kbd "<tab>") 'company-complete)
  (local-set-key (kbd "M-<tab>") 'company-complete)
  (set (make-local-variable 'company-backends) '(company-capf))

  ;; This uses the standard completion-UI which is
  ;; ivy in-region completion when activated
  ;; This is good enough until there is a pcomplete backend for company
  ;; (company-mode -1) ;; Switch-off company to ensure ivy is used
  ;; (local-set-key (kbd "<tab>")
  ;;                (lambda () (interactive) (pcomplete-std-complete)))

  ;; Completion-UI
  ;; (define-key completion-overlay-map [(control ?c)]
  ;;   (lambda ()
  ;;     "For eshell-mode call the `eshell-interrupt-process' command
  ;; after rejecting the completion"
  ;;     (interactive)
  ;;     (completion-reject)
  ;;     (if (eq major-mode 'eshell-mode)
  ;;         (eshell-interrupt-process))))
  )

(use-package multi-eshell
  :ensure t
  :init
  (setq multi-eshell-shell-function '(eshell)
        multi-eshell-name "*eshell*"))

;; -----------------------------------------------------------------------------
;;; Generally useful additional functions
(defun eshell/dired ()
  "Display `dired' on the current directory"
  (dired (eshell/pwd)))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/tsh (machine)
  "Create an ssh connection to MACHINE in the current directory using `tramp'."
  (eshell/cd
   (concat "/" machine ":" (car (last (split-string (eshell/pwd) ":"))))))

(defun eshell/sb (&rest args)
  "Switch to given buffer."
  (funcall 'switch-to-buffer (apply 'eshell-flatten-and-stringify args)))

(defun pcomplete/sb ()
  "Completion for sb (switch-to-buffer)."
  (while (pcomplete-here (mapcar 'buffer-name (buffer-list)))))

(defun eshell/ec (&rest args)
  "Use `compile' to do background makes."
  (if (eshell-interactive-output-p)
      (let ((compilation-process-setup-function
             (list 'lambda nil
                   (list 'setq 'process-environment
                         (list 'quote (eshell-copy-environment))))))
        (compile (eshell-flatten-and-stringify args))
        (pop-to-buffer compilation-last-buffer))
    (throw 'eshell-replace-command
           (let ((l (eshell-stringify-list (eshell-flatten-list args))))
             (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)

(defun eshell-view-file (file)
  "A version of `view-file' which properly respects the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        (buffer (find-file-noselect file)))
    (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file. \"less +42 foo\" will go to line 42 in
the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell-view-file file)
          (goto-line line))
      (eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

(eval-after-load "em-ls"
  '(progn
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<return>") 'find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'ffap-at-mouse)
       (defvar eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

(defun pcomplete/eshell-mode/bmk ()
  "Completion for `bmk'"
  (pcomplete-here (bookmark-all-names)))

(defun eshell/bmk (&rest args)
  "Integration between EShell and bookmarks.
For usage, execute without arguments."
  (setq args (eshell-flatten-list args))
  (let ((bookmark (car args))
        filename name)
    (cond
     ((eq nil args)
      (format "Usage: bmk BOOKMARK to change directory pointed to by BOOKMARK
    or bmk . BOOKMARK to bookmark current directory in BOOKMARK.
Completion is available."))
     ((string= "." bookmark)
      ;; Store current path in EShell as a bookmark
      (if (setq name (car (cdr args)))
          (progn
            (bookmark-set name)
            (bookmark-set-filename name (eshell/pwd))
            (format "Saved current directory in bookmark %s" name))
        (error "You must enter a bookmark name")))
     (t
      ;; Assume the user wants to go to the path pointed out by a bookmark.
      (if (setq filename (cdr (car (bookmark-get-bookmark-record bookmark))))
          (if (file-directory-p filename)
              (eshell/cd filename)
            ;; TODO: Handle this better and offer to go to directory
            ;; where the file is located.
            (error "Bookmark %s points to %s which is not a directory"
                   bookmark filename))
        (error "%s is not a bookmark" bookmark))))))

;; -----------------------------------------------------------------------------
;;; Git, git-grep etc.

(defun eshell/git (&rest args)
  (apply 'eshell-exec-visual (cons "git" args)))

(defun eshell/git-grep (rex)
  (vc-git-grep rex "*" "."))

(defun eshell/greed (regex)
  "Alias \"greed\" to call Emacs `greed' function."
  (let ((ret (apply 'greed (list regex nil))))))

(put 'eshell/greed 'eshell-no-numeric-conversions t)

(defun eshell/mgrep (dir regex &optional files)
  "Alias \"mgrep\" to call Emacs `greed-grep' function."
  (let ((ret (apply 'greed-grep (list dir (list regex files)))))))

(put 'eshell/mgrep 'eshell-no-numeric-conversions t)

(defun eshell/mgrepf (dir regex &optional files)
  "Alias \"mgrepf\" to call Emacs `greed-grep-find' function."
  (let ((ret (apply 'greed-grep-find (list dir (list regex files)))))))

(put 'eshell/mgrepf 'eshell-no-numeric-conversions t)

;; -----------------------------------------------------------------------------
;;; init-eshell.el ends here
