;;; qinit.el --- Init file for quick-start
;; -----------------------------------------------------------------------------
;;; Fix annoyances

;;;  Disable start-up splash screen
(setq inhibit-startup-message t)  ;; Include in init-eemacs

;;;  Switch-off the pop-up dialog box
(setq use-file-dialog nil)  ;; Include in init-eemacs

;;;  Switch off the tool-bar
(tool-bar-mode -1)  ;; Include in init-eemacs

;;;  Switch off the scroll-bar
(scroll-bar-mode -1)  ;; Include in init-eemacs

;;;  Use "y or n" answers rather than "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;;  Ask for confirmation before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;;;  Disable the bell
(setq visible-bell t)

;;;  Warning only when something went not as expected
(setq ring-bell-function
      (lambda ()
        (unless (memq
                 this-command
                 '(isearch-abort abort-recursive-edit
                                 exit-minibuffer keyboard-quit))
          (ding))))

;;;  Set smooth scrolling (this seems to make the scrolling sluggish)
;(setq scroll-conservatively 10000)

;;;  Point keeps its screen position if the scroll
;;   command moved it vertically out of the window, e.g. when scrolling
;;   by full screens.
(setq scroll-preserve-screen-position t)

;;;  Disallow automatic scrolling windows horizontally.
(setq auto-hscroll-mode nil)

;;;  Set only the safe file-local variables and do not query.
(setq enable-local-variables :safe)

;; -----------------------------------------------------------------------------
;;; Autosave

;;;  Set the location for all the auto-saved files
(defvar user-temporary-file-directory (expand-file-name "~/Emacs/Autosaves/"))
(make-directory user-temporary-file-directory t)
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(setq auto-save-interval 200)

;; -----------------------------------------------------------------------------
;;; Global key bindings

(global-set-key "\C-x\C-k"  'kill-this-buffer)
;;(global-set-key [f2]        'kill-this-buffer)
(global-set-key "\M-#"      'query-replace-regexp)
(global-set-key "\C-xf"     'find-file-at-point)

(global-set-key [f9]        'first-error)
(global-set-key [f11]       'next-error)
(global-set-key [f12]       'previous-error)

(global-set-key [S-left]    "\M-b")     ;; Backward word
(global-set-key [S-right]   "\M-f")     ;; Forward word
(global-set-key [C-left]    "\M-a")     ;; Backward sentence
(global-set-key [C-right]   "\M-e")     ;; Forward sentence
(global-set-key [C-S-left]  "\C-\M-a")  ;; Backward function
(global-set-key [C-S-right] "\C-\M-e")  ;; Forward function

(global-set-key [M-S-left]  'previous-buffer)
(global-set-key [M-S-right] 'next-buffer)

(global-set-key [S-home]    'beginning-of-buffer)
(global-set-key [S-end]     'end-of-buffer)

;;; Bind x-clipboard-yank to the PrintScr key on the Kinesis keyboard
(global-set-key [print] 'x-clipboard-yank)

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

;;;  Remove tab-to-tab-stop from "\M-i" and bind undo to it
(global-unset-key "\M-i") (global-set-key "\M-i" 'undo)

;;;  Also bind undo to M-up
(global-set-key [M-up] 'undo)

;;;  More aggressive quit usually bound to M-ESC-ESC and ESC-ESC-ESC
(global-set-key "\C-c\C-g" 'keyboard-escape-quit)

;;;  Auto-indent on return
(global-set-key (kbd "RET") 'newline-and-indent)
;(global-set-key [return] 'newline-and-indent)

;;;  Toggle view-mode rather than toggle-read-only.
(define-key ctl-x-map "\C-q" 'view-mode)


;; -----------------------------------------------------------------------------
;;; qinit.el ends here
