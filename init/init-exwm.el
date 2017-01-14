(defun next-code-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (next-buffer))))

(defun previous-code-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (equal bread-crumb (buffer-name))))
      (previous-buffer))))

(use-package exwm
  :ensure t
  :ensure xelb)

(defun start-exwm (&optional option)
  "Start EXWM"

  (defvar exwm-nav-map
    (let ((map (make-sparse-keymap)))
      (global-unset-key "\C-t")
      (define-key global-map "\C-t" map)
      map))

  ;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Shrink fringes to 1 pixel
  (fringe-mode 1)

  ;; Show date and time in the modeline
  (setq display-time-default-load-average nil
        display-time-day-and-date t)
  (display-time-mode t)

  ;; Set the initial number of workspaces.
  (setq exwm-workspace-number 6)

  (defvar exwm-workspace-switch-wrap t
    "Whether `exwm-workspace-next' and `exwm-workspace-prev' should wrap.")

  (defun exwm-workspace-next ()
    "Switch to next EXWM workspace."
    (interactive)
    (let* ((only-workspace? (equal exwm-workspace-number 1))
           (overflow? (= exwm-workspace-current-index
                         (1- exwm-workspace-number))))
      (cond
       (only-workspace? nil)
       (overflow?
        (when exwm-workspace-switch-wrap
          (exwm-workspace-switch 0)))
       (t (exwm-workspace-switch (1+ exwm-workspace-current-index))))))

  (defun exwm-workspace-prev ()
    "Switch to next EXWM workspace."
    (interactive)
    (let* ((only-workspace? (equal exwm-workspace-number 1))
           (overflow? (= exwm-workspace-current-index 0)))
      (cond
       (only-workspace? nil)
       (overflow?
        (when exwm-workspace-switch-wrap
          (exwm-workspace-switch (1- exwm-workspace-number))))
       (t (exwm-workspace-switch (1- exwm-workspace-current-index))))))

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
  ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
  ;; when a new window class name or title is available. Here's some advice on
  ;; this subject:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + Only renaming buffer in one hook and avoid it in the other. There's no
  ;;   guarantee on the order in which they are run.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
  ;;   all windows are probably the same. Using window titles for them makes
  ;;   more sense.
  ;; + Some application change its title frequently (e.g. browser, terminal).
  ;;   Its class name may be more suitable for such case.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))
              (when (string= "Emacs" exwm-class-name)
                (exwm-workspace-rename-buffer exwm-instance-name))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; `exwm-input-set-key' allows you to set a global key binding (available in
  ;; any case). Following are a few examples.

  (setq exwm-input-prefix-keys '(?\C-t)
        ;;'(?\C-c ?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:)
        )

  (exwm-input-set-key (kbd "C-t C-c") (lookup-key global-map (kbd "C-c")))
  (exwm-input-set-key (kbd "C-t C-x") (lookup-key global-map (kbd "C-x")))
  (exwm-input-set-key (kbd "C-t C-u") (lookup-key global-map (kbd "C-u")))
  (exwm-input-set-key (kbd "C-t C-h") (lookup-key global-map (kbd "C-h")))
  (exwm-input-set-key (kbd "C-t M-x") (lookup-key global-map (kbd "M-x")))
  (exwm-input-set-key (kbd "C-t M-`") (lookup-key global-map (kbd "M-`")))
  (exwm-input-set-key (kbd "C-t M-&") (lookup-key global-map (kbd "M-&")))
  (exwm-input-set-key (kbd "C-t M-:") (lookup-key global-map (kbd "M-:")))

  ;; + We always need a way to switch between line-mode from char-mode
  (exwm-input-set-key (kbd "C-t t") #'exwm-input-toggle-keyboard)

  (exwm-input-set-key (kbd "C-t C-t") #'exwm-workspace-next)
  ;;(exwm-input-set-key (kbd "C-t C-t") #'other-window)
  (exwm-input-set-key (kbd "C-t <left>") #'windmove-left)
  (exwm-input-set-key (kbd "C-t <right>") #'windmove-right)
  (exwm-input-set-key (kbd "C-t <up>") #'windmove-up)
  (exwm-input-set-key (kbd "C-t <down>") #'windmove-down)

  (exwm-input-set-key (kbd "C-t C-n") #'next-code-buffer)
  (exwm-input-set-key (kbd "C-t C-p") #'previous-code-buffer)

  ;; + Bind a key to switch workspace interactively
  (exwm-input-set-key (kbd "C-t w") #'exwm-workspace-switch)

  ;; + Bind "C-t 0" to "C-t 9" to switch to the corresponding workspace.
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "C-t %d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i)))
    (exwm-input-set-key (kbd (format "C-t C-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))

  ;; + Application launcher ('C-t c' also works if the output buffer does not
  ;;   bother you). Note that there is no need for processes to be created by
  ;;   Emacs.
  (exwm-input-set-key (kbd "C-t r")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

  (defun exwm-command (command)
    (interactive)
    (start-process-shell-command command nil command))

  (exwm-input-set-key (kbd "C-t c")
                      (lambda ()
                        (interactive)
                        (start-process "eshell" nil "eshell")))

  (exwm-input-set-key (kbd "C-t C-c")
                      (lambda ()
                        (interactive)
                        (start-process "xterm" nil "xterm")))

  (exwm-input-set-key (kbd "C-t e")
                      (lambda ()
                        (interactive)
                        (start-process "edit" nil "edit")))

  (exwm-input-set-key (kbd "C-t x")
                      (lambda ()
                        (interactive)
                        (start-process "conkeror" nil "conkeror")))

  (exwm-input-set-key (kbd "C-t l")
                      (lambda ()
                        (interactive)
                        (start-process "" nil "xlock")))

  ;; The following example demonstrates how to set a key binding only available
  ;; in line mode. It's simply done by first push the prefix key to
  ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
  ;; The example shorten 'C-c q' to 'C-q'.
  ;;(push ?\C-q exwm-input-prefix-keys)
  ;;(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic the
  ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application. Note that SRC must be
  ;; a key sequence (of type vector or string), while DEST can also be a single
  ;; key.
  ;; (exwm-input-set-simulation-keys
  ;;  '(([?\C-b] . left)
  ;;    ([?\C-f] . right)
  ;;    ([?\C-p] . up)
  ;;    ([?\C-n] . down)
  ;;    ([?\C-a] . home)
  ;;    ([?\C-e] . end)
  ;;    ([?\M-v] . prior)
  ;;    ([?\C-v] . next)
  ;;    ([?\C-d] . delete)
  ;;    ([?\C-k] . (S-end delete))))

  ;; You can hide the mode-line of floating X windows by uncommenting the
  ;; following lines
  ;;(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  ;;(add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

  ;; You can hide the minibuffer and echo area when they're not used, by
  ;; uncommenting the following line
  (setq exwm-workspace-minibuffer-position 'bottom)
  ;;(setq echo-keystrokes 0)
  ;;(setq exwm-workspace-minibuffer-position nil)

  (which-key-mode -1)
  ;;(setq which-key-popup-type 'side-window)

  (auto-dim-other-buffers-mode -1)

  (exwm-enable))
