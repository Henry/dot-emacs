;;; qinit.el --- Init file for quick-start
;; -----------------------------------------------------------------------------
;;; Fix annoyances

;;;  Disable start-up splash screen
(setq inhibit-startup-screen t)  ;; Include in init-eemacs

;;; Set the *scratch* buffer to emacs-lisp-mode
;;  rather than the default lisp-interaction-mode
(setq initial-major-mode 'emacs-lisp-mode)  ;; Include in init-eemacs

;;;  Switch-off the pop-up dialog box
(setq use-file-dialog nil)  ;; Include in init-eemacs

;;;  Switch off the tool-bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))  ;; Include in init-eemacs

;;;  Switch off the tool-bar
(setq tool-bar-mode nil)  ;; Include in init-eemacs

;;;  Switch off the scroll-bar
(setq scroll-bar-mode nil)  ;; Include in init-eemacs

;;;  Use "y or n" answers rather than "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)  ;; Include in init-eemacs

;;;  Remove startup message in the echo area
(fset 'display-startup-echo-area-message 'ignore)  ;; Include in init-eemacs

;;;  Ask for confirmation before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;;;  Disable the bell
(setq visible-bell t)

;;;  Warning only when something went not as expected
(setq ring-bell-function
      (lambda ()
        (unless (memq
                 this-command
                 '(isearch-abort
                   abort-recursive-edit
                   exit-minibuffer
                   keyboard-quit))
          (ding))))

;;;  Never use dialogs for minibuffer input
(setq use-dialog-box nil)

;;;  Point keeps its screen position if the scroll
;;   command moved it vertically out of the window, e.g. when scrolling
;;   by full screens.
(setq scroll-preserve-screen-position t)

;;;  Disallow automatic scrolling windows horizontally.
(setq auto-hscroll-mode nil)

;;;  Set only the safe file-local variables and do not query.
;;(setq enable-local-variables :safe)

;;; Set directory where temporary files are stored
(setq user-emacs-directory "~/Emacs/")

;; -----------------------------------------------------------------------------
;;; Autosave

;;;  Set the location for all the auto-saved files
(defvar user-temporary-file-directory
  (expand-file-name "Autosaves/" user-emacs-directory))
(make-directory user-temporary-file-directory t)
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-")
      auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t))
      auto-save-interval 200)

;; -----------------------------------------------------------------------------
;;; Kill current buffer without confirmation

(defun kill-current-buffer ()
  "Kill current buffer without confirmation."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

;; -----------------------------------------------------------------------------
;;; Global key bindings

(global-set-key "\C-x\C-k"  'kill-current-buffer)
(global-set-key "\M-#"      'query-replace-regexp)
(global-set-key "\C-xf"     'find-file-at-point)

(global-set-key [f9]        'first-error)
(global-set-key [f11]       'next-error)
(global-set-key [f12]       'previous-error)

(global-set-key [S-left]    'backward-word)
(global-set-key [S-right]   'forward-word)
(global-set-key [C-left]    'backward-sentence)
(global-set-key [C-right]   'forward-sentence)
(global-set-key [C-S-left]  'beginning-of-defun)
(global-set-key [C-S-right] 'end-of-defun)

(global-set-key [M-S-left]  'previous-buffer)
(global-set-key [M-S-right] 'next-buffer)

(global-set-key [S-home]    'beginning-of-buffer)
(global-set-key [S-end]     'end-of-buffer)

;; -----------------------------------------------------------------------------
;;;  Scolling while maintaining the cursor position
(defun up-one (N)
  "Scroll the text up one line maintaining the cursor position"
  (interactive "p")
  (scroll-up N)
  (forward-line N))

(defun down-one (N)
  "Scroll the text down one line maintaining the cursor position"
  (interactive "p")
  (scroll-down N)
  (forward-line (- N)))

(global-set-key [S-up]   'down-one)
(global-set-key [S-down] 'up-one)

(defvar block-scroll-size 10)

(defun up-some ()
  (interactive)
  (scroll-up block-scroll-size)
  (forward-line block-scroll-size))

(defun down-some ()
  (interactive)
  (scroll-down block-scroll-size)
  (forward-line (- block-scroll-size)))

(global-set-key [C-up]   'down-some)
(global-set-key [C-down] 'up-some)

;; -----------------------------------------------------------------------------
;;;  Remove tab-to-tab-stop from "\M-i" and bind undo to it
(global-unset-key "\M-i")
(global-set-key "\M-i" 'undo)

;; -----------------------------------------------------------------------------
;;;  More aggressive quit usually bound to M-ESC-ESC and ESC-ESC-ESC
(global-set-key "\C-c\C-g" 'keyboard-escape-quit)

;; -----------------------------------------------------------------------------
;;;  Auto-indent on return
(global-set-key (kbd "RET") 'newline-and-indent)

;;;  Toggle view-mode rather than toggle-read-only.
(define-key ctl-x-map "\C-q" 'view-mode)

;; -----------------------------------------------------------------------------
;;; qinit.el ends here
