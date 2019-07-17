;;; init-exwm.el --- Initialize EXWM
;; -----------------------------------------------------------------------------

(use-package exwm
  :ensure t
  :ensure xelb)

(defun start-exwm (&optional option)
  "Start EXWM"

  ;; Hacked version of `exwm-input--update-mode-line' for `smart-mode-line'
  ;; Changes the background color of the line/char mode indicator
  ;; (defun exwm-input--update-mode-line (id)
  ;;   "Update the propertized `mode-line-process' for window ID."
  ;;   (let (help-echo cmd mode)
  ;;     (cl-case exwm--on-KeyPress
  ;;       ((exwm-input--on-KeyPress-line-mode)
  ;;        (setq mode "line"
  ;;              help-echo "mouse-1: Switch to char-mode"
  ;;              cmd `(lambda ()
  ;;                     (interactive)
  ;;                     (exwm-input-release-keyboard ,id))))
  ;;       ((exwm-input--on-KeyPress-char-mode)
  ;;        (setq mode "char"
  ;;              help-echo "mouse-1: Switch to line-mode"
  ;;              cmd `(lambda ()
  ;;                     (interactive)
  ;;                     (exwm-input-grab-keyboard ,id)))))
  ;;     (with-current-buffer (exwm--id->buffer id)
  ;;       (setq mode-line-process
  ;;             `(": "
  ;;               (:propertize
  ;;                ,mode
  ;;                help-echo ,help-echo
  ;;                face powerline-active2 ;; Added for `smart-mode-line'
  ;;                mouse-face mode-line-highlight
  ;;                local-map
  ;;                (keymap
  ;;                 (mode-line
  ;;                  keymap
  ;;                  (down-mouse-1 . ,cmd)))))))))

  ;; New functionality
  ;; (setq exwm-manage-configurations
  ;;       `((t
  ;;          floating-mode-line nil
  ;;          tiling-mode-line nil
  ;;          floating-header-line ,mode-line-format
  ;;          tiling-header-line ,mode-line-format)))

  (defun exwm-input-set-global-key (key function)
    "Add KEY to `exwm-input-prefix-keys' and bind FUNCTION to KEY
     in exwm keymap"
    (cl-pushnew (elt key 0) exwm-input-prefix-keys)
    (exwm-input-set-key key function))

  ;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Shrink fringes to 1 pixel
  (fringe-mode 1)

  ;; Set to char-mode by default
  (setq exwm-manage-configurations
        '((t char-mode t)))(setq exwm-manage-configurations '((t char-mode t)))

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
  ;; + Always use `exwm-workspace-rename-buffer' to avoid naming conflict.
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

  ;; ;; `exwm-input-set-key' allows you to set a global key binding
  ;; (setq exwm-input-prefix-keys '(?\s-t))

  ;; (exwm-input-set-key (kbd "s-t C-c") (lookup-key global-map (kbd "C-c")))
  ;; (exwm-input-set-key (kbd "s-t C-x") (lookup-key global-map (kbd "C-x")))
  ;; (exwm-input-set-key (kbd "s-t C-u") (lookup-key global-map (kbd "C-u")))
  ;; (exwm-input-set-key (kbd "s-t C-h") (lookup-key global-map (kbd "C-h")))
  ;; (exwm-input-set-key (kbd "s-t M-x") (lookup-key global-map (kbd "M-x")))
  ;; (exwm-input-set-key (kbd "s-t M-`") (lookup-key global-map (kbd "M-`")))
  ;; (exwm-input-set-key (kbd "s-t M-&") (lookup-key global-map (kbd "M-&")))
  ;; (exwm-input-set-key (kbd "s-t M-:") (lookup-key global-map (kbd "M-:")))

  (setq exwm-input-prefix-keys nil)

  (exwm-input-set-global-key (kbd "s-C-c") (lookup-key global-map (kbd "C-c")))
  (exwm-input-set-global-key (kbd "s-C-x") (lookup-key global-map (kbd "C-x")))
  (exwm-input-set-global-key (kbd "s-x")   (lookup-key global-map (kbd "C-x")))
  (exwm-input-set-global-key (kbd "s-C-u") (lookup-key global-map (kbd "C-u")))
  (exwm-input-set-global-key (kbd "s-C-h") (lookup-key global-map (kbd "C-h")))
  (exwm-input-set-global-key (kbd "s-M-x") (lookup-key global-map (kbd "M-x")))
  (exwm-input-set-global-key (kbd "s-M-`") (lookup-key global-map (kbd "M-`")))
  (exwm-input-set-global-key (kbd "s-M-&") (lookup-key global-map (kbd "M-&")))
  (exwm-input-set-global-key (kbd "s-M-:") (lookup-key global-map (kbd "M-:")))

  ;; 's-r': Reset
  (exwm-input-set-global-key (kbd "s-R") #'exwm-reset)

  ;; We always need a way to switch between line-mode from char-mode
  (exwm-input-set-global-key (kbd "s-T") #'exwm-input-toggle-keyboard)

  ;; 's-w': Interactively switch workspace
  (exwm-input-set-global-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-<Page Down>': Switch to next workspace (cycles)
  (exwm-input-set-global-key (kbd "s-<next>") #'exwm-workspace-next)

  ;; 's-<Page Up>': Switch to previous workspace (cycles)
  (exwm-input-set-global-key (kbd "s-<prior>") #'exwm-workspace-prev)

  ;; 's-N': Switch to specified workspace
  (dotimes (i 10)
    (exwm-input-set-global-key (kbd (format "s-%d" i))
                               `(lambda ()
                                  (interactive)
                                  (exwm-workspace-switch-create ,i))))

  (defun exwm-command (command)
    "Execute the shell COMMAND"
    (start-process-shell-command command nil command))

  (defun exwm-wanderlust ()
    "Start wanderlust server"
    (interactive)
    (exwm-command "wanderlust"))

  (defun exwm-emms ()
    "Start emms server"
    (interactive)
    (exwm-command "emms"))

  ;; 's-r': Interactively select and launch application
  (exwm-input-set-global-key
   (kbd "s-r") (lambda (command)
                 (interactive (list (read-shell-command "$ ")))
                 (exwm-command command)))

  (exwm-input-set-global-key (kbd "s-n") #'next-code-buffer)
  (exwm-input-set-global-key (kbd "s-p") #'previous-code-buffer)

  (exwm-input-set-global-key (kbd "s-<left>") #'windmove-left)
  (exwm-input-set-global-key (kbd "s-<right>") #'windmove-right)
  (exwm-input-set-global-key (kbd "s-<up>") #'windmove-up)
  (exwm-input-set-global-key (kbd "s-<down>") #'windmove-down)

  (exwm-input-set-global-key
   (kbd "s-s") (lambda ()
                 (interactive)
                 (start-process "eshell" nil "eshell")))

  (exwm-input-set-global-key
   (kbd "s-t") (lambda ()
                 (interactive)
                 (start-process "xterm" nil "xterm")
                 (exwm-input-toggle-keyboard)))

  (exwm-input-set-global-key
   (kbd "s-e") (lambda ()
                 (interactive)
                 (start-process "edit" nil "edit")))

  (exwm-input-set-global-key
   (kbd "s-E") (lambda ()
                 (interactive)
                 (start-process "e" nil "e")))

  (exwm-input-set-global-key
   (kbd "s-c") (lambda ()
                 (interactive)
                 (start-process "firefox" nil "firefox")))

  (exwm-input-set-global-key
   (kbd "s-l") (lambda ()
                 (interactive)
                 (start-process "" nil "xlock")))

  (cl-pushnew ?\s-m exwm-input-prefix-keys)

  (defvar exwm-emms-bindings
    '(("n" "emms-next")
      ("p" "emms-previous")
      ("S" "emms-stop")
      ("P" "emms-pause"))
    "List of emms-server commands and exwm bindings")

  ;; Bind emms-server commands to the keys in the `exwm-emms-bindings' list
  (mapc (lambda (x)
          (exwm-input-set-key
           (kbd (concat "s-m " (car x)))
           `(lambda ()
              (interactive)
              (exwm-command
               (concat "emacsclient -s emms -e '(" ,(cadr x) ")'")))))
        exwm-emms-bindings)

  ;; Started named programs in char-mode
  ;; (defun exwm-start-in-char-mode ()
  ;;   (when (or (string= exwm-instance-name "eshell")
  ;;             (string= exwm-instance-name "xterm")
  ;;             (string= exwm-instance-name "emacs")
  ;;             (string= exwm-instance-name "eemacs")
  ;;             (string= exwm-instance-name "emacsclient")
  ;;             (string= exwm-instance-name "edit")
  ;;             (string= exwm-instance-name "Edit")
  ;;             (string= exwm-instance-name "e"))
  ;;     (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
  ;; (add-hook 'exwm-manage-finish-hook 'exwm-start-in-char-mode)


  ;; The following example demonstrates how to set a key binding only available
  ;; in line mode. It's simply done by first push the prefix key to
  ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
  ;; The example shorten 'C-c q' to 'C-q'.
  ;; (push ?\C-q exwm-input-prefix-keys)
  ;; (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

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

  ;; Write the workspace number in the mode-line instead of the frame number
  (setq mode-line-frame-identification
        '(:propertize (:eval (format "%d " exwm-workspace-current-index))
                      face mode-line-buffer-id))

  ;; You can hide the minibuffer and echo area when they're not used, by
  ;; uncommenting the following line
  (setq exwm-workspace-minibuffer-position 'bottom)
  ;;(setq echo-keystrokes 0)

  ;; `which-key-mode' does not work reliably with exwm
  (which-key-mode -1)

  ;; `auto-dim-other-buffers-mode' is not useful with exwm as it only changes
  ;; the color of the underlying buffer which is overlayed by the chosen
  ;; application
  (auto-dim-other-buffers-mode -1)

  ;; Enable EXWM
  (exwm-enable)

  ;; Remap keyboard
  (defun exwm-xmodmap()
    (call-process "xmodmap" nil (get-buffer-create "wm") nil
                  (expand-file-name "~/.xmodmap")))
  (exwm-xmodmap)
  )

;; -----------------------------------------------------------------------------
;;; init-exwm.el ends here
