;;; init-dired+.el --- Initialize better directory display
;; -----------------------------------------------------------------------------

(use-package dired
  :preface
  ;; Set the face for executable files
  (defface diredp-executable-file-name
    '((t (:foreground "dodger blue" :weight bold)))
    "*Face used for names of executable files in dired buffers."
    :group 'Dired-Plus :group 'font-lock-highlighting-faces)
  (defvar diredp-executable-file-name 'diredp-executable-file-name)

  :init
  (require 'dired-x)

  (use-package dired+
    :ensure t)

  ;; tar and untar (gz) from dired (bound to 'T')
  (require 'dired-tar)

  ;; sort files to go to different places in dired
  (require 'fsdired)

  (setq
   ;; Set the ls options to put directories first
   dired-listing-switches "-al --group-directories-first"
   dired-omit-files "^\\.?#\\|^\\."
   diredp-font-lock-keywords-1
     (append
      diredp-font-lock-keywords-1
      (list
       (list dired-re-exe
             `(".+"
               (dired-move-to-filename)
               nil
               (0 diredp-executable-file-name t)))))
     )

  (defun dired-next-file-line ()
    "Move to the next dired line that have a file or directory name on it."
    (interactive)
    (call-interactively 'dired-next-line)
    (if (eobp)
        (dired-previous-line 1)))

  (defun dired-previous-file-line ()
    "Move to the previous dired line that have a file or directory name on it."
    (interactive)
    (call-interactively 'dired-previous-line)
    (if (not (dired-move-to-filename))
        (dired-next-line 1)))

  (defun my-dired-mode-hook ()

    ;; Switch-on font-lock
    (font-lock-mode 1)

    ;; hl-line - highlight current-line
    (hl-line-mode)

    ;; Use the same buffer for visited directories
    (toggle-diredp-find-file-reuse-dir 1)

    ;; Set omit-mode by default
    (dired-omit-mode 1)

    (substitute-key-definition
     'dired-next-line 'dired-next-file-line dired-mode-map)
    (substitute-key-definition
     'dired-previous-line 'dired-previous-file-line dired-mode-map)
    )
  (add-hook 'dired-mode-hook 'my-dired-mode-hook)

  :bind
  (:map dired-mode-map
        ;; wdired: make the file names writable
        ("C-w" . wdired-change-to-wdired-mode)
        ("[" . backward-page)
        ("]" . forward-page)
        ("<M-up>" . dired-up-directory)
        ("/" . dired-omit-expunge)
        ("<down>" . dired-next-file-line)
        ("<up>" . dired-previous-file-line)))

;; -----------------------------------------------------------------------------
;;;  Better cursor movement functions

(defun dired-next-file-line ()
  "Move to the next dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-next-line)
  (if (eobp)
      (dired-previous-line 1)))

(defun dired-previous-file-line ()
  "Move to the previous dired line that have a file or directory name on it."
  (interactive)
  (call-interactively 'dired-previous-line)
  (if (not (dired-move-to-filename))
      (dired-next-line 1)))

;; -----------------------------------------------------------------------------
;;;  Keybindings

(substitute-key-definition
 'dired-next-line 'dired-next-file-line dired-mode-map)
(substitute-key-definition
 'dired-previous-line 'dired-previous-file-line dired-mode-map)

;; -----------------------------------------------------------------------------
;;;  Image-dired

(use-package image-dired
  :ensure t)

;; -----------------------------------------------------------------------------
;;; init-dired+.el ends here
