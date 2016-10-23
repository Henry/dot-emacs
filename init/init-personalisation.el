;;; init-personalisation.el --- General personal settings
;; -----------------------------------------------------------------------------
;;; Enhanced window-splitting rule

(defvar split-window-threshold 160
  "Frame width above which the frame is split horizontally
rather than vertically.")

(defun split-window-horizontally-or-vertically ()
  "Split the window horizontally if the `frame-width' is larger than
`split-window-threshold' otherwise split it vertically."
  (interactive)
  (if (and (one-window-p t)
           (not (active-minibuffer-window)))
      (if (> (frame-width) split-window-threshold)
          (split-window-horizontally)
        (split-window-vertically))
    (selected-window)))

(add-hook 'temp-buffer-setup-hook 'split-window-horizontally-or-vertically)

;; -----------------------------------------------------------------------------
;;; Automatically indent pasted regions in all modes except c++
;; http://www.emacswiki.org/cgi-bin/wiki?action=browse;diff=1;id=AutoIndentation

(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode
                scheme-mode lisp-mode
                c-mode objc-mode
                latex-mode plain-tex-mode ruby-mode nxml-mode nxhtml-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode
                scheme-mode lisp-mode
                c-mode objc-mode
                latex-mode plain-tex-mode ruby-mode nxml-mode nxhtml-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

;; -----------------------------------------------------------------------------
;;; Elisp implementations of rgrep, grep-find, grep, etc...
(require 'traverselisp)

;; -----------------------------------------------------------------------------
;;; babel --- interface to web translation services
(use-package babel)

;; -----------------------------------------------------------------------------
;;; Goto Last Change
;; Goto the point of the most recent edit in the buffer.
;; For key-bindings see my-map
(use-package goto-chg)

;; -----------------------------------------------------------------------------
;;; Support for marking a rectangle of text with highlighting.

(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)
(use-package rect-mark)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)

;; -----------------------------------------------------------------------------
;;; Replace $HOME with ~ in file names in title bars

(defun short-path (path)
  "Shorten PATH by replacing the middle set of elements by `...'."
  (let ((dir-names (split-string path "/")))
    (if (< (length dir-names) 5)
        path
      (let* ((first (nth 0 dir-names))
            (short-path first))
        (when (and (> (length first) 0) (not (equal first "~")))
          (setq short-path (concat "/" short-path)))
        (concat
         short-path
         "/" (nth 1 dir-names)
         "/.../" (car (last dir-names 3))
         "/" (car (last dir-names 2))
         "/" (car (last dir-names))
           )
        )))
  )

(setq frame-title-format
      '(:eval
        (cond
         (buffer-file-name
          (short-path
           (replace-regexp-in-string
            (regexp-quote (getenv "HOME")) "~"
            (convert-standard-filename buffer-file-name))))
         ((eq major-mode 'eshell-mode)
          (concat (getenv "HOSTNAME") ": " (short-path (eshell/pwd))))
         (t
          (buffer-name)))))

;; -----------------------------------------------------------------------------
;;; Kill the whole line
;;  including its terminating newline by typing C-a C-k
;; when used at the beginning of a line
(setq kill-whole-line t)

;; -----------------------------------------------------------------------------
;;; Whole-line-or-region
;; Operate on the current line if they would normally operate on a region
;; and region is currently undefined (doesn't work with Emacs-23)
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode)

;; -----------------------------------------------------------------------------
;;; Battery status
;; If not on AC power line, then display battery status on the mode line
(and (require 'battery nil t)
     (functionp 'battery-status-function)
     (or (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
         (display-battery-mode)))

;; -----------------------------------------------------------------------------
;;; Speedbar settings

(setq
 speedbar-hide-button-brackets-flag t
 speedbar-indentation-width 2
 speedbar-show-unknown-files t
 speedbar-update-flag nil
 speedbar-use-images t)

(use-package sr-speedbar)
;; Same-frame speedbar
(autoload 'sr-speedbar-open "sr-speedbar" "sr-speedbar" t)

;; -----------------------------------------------------------------------------
;;; Duplicate start of line or region

(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

;; -----------------------------------------------------------------------------
;;; toggle-selective-display: display lines starting at given column

(defun my-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column ; disable toggle if column was supplied
       (unless selective-display 1))))

;; -----------------------------------------------------------------------------
;;; Return my home directory string

(defun my-home (&optional arg)
  "Return my home directory string."
  (or (and (boundp 'my-home)
           (cadr (assoc arg my-home)))
      (getenv "HOME")
      (expand-file-name "~")))

;; -----------------------------------------------------------------------------
;;; Find file starting from $HOME

(defun my-goto-home (&optional arg)
  (interactive "c")
  (find-file (my-home arg)))

;; -----------------------------------------------------------------------------
;;; Make scripts executable on save

(defun my-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))

(add-hook 'after-save-hook 'my-make-script-executable)

;; -----------------------------------------------------------------------------
;;; Switch buffers between windows

(defun my-rotate-windows ()
   "Switch buffers between windows"
   (interactive)
   (let ((this-buffer (buffer-name)))
     (other-window -1)
     (let ((that-buffer (buffer-name)))
       (switch-to-buffer this-buffer)
       (other-window 1)
       (switch-to-buffer that-buffer)
       (other-window -1))))

;; -----------------------------------------------------------------------------
;;; Kill the buffer and the frame it is in

(defun kill-buffer-and-frame ()
  "Kill the buffer and the frame it is in"
  (interactive)
  (kill-this-buffer)
  (delete-frame))

;; -----------------------------------------------------------------------------
;;; Enlarge frame and split into two windows

(defun my-double-frame ()
  "Enlarge frame and split into two windows"
  (interactive)
  (delete-other-windows)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) my-default-frame-height)
  (set-frame-width (selected-frame) (+ (* 2 my-default-frame-width) 5))
  (split-window-horizontally))

;; -----------------------------------------------------------------------------
;;; Unsplit frame and return to normal size

(defun my-single-frame ()
  "Enlarge frame and split into two windows"
  (interactive)
  (delete-other-windows)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) my-default-frame-height)
  (set-frame-width (selected-frame) my-default-frame-width))

;; -----------------------------------------------------------------------------
;;; Untabify whole buffer

(defun untabify-buffer ()
  "Untabify whole buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;; -----------------------------------------------------------------------------
;;; Indent whole buffer according to mode

(defun indent-buffer ()
  "Indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

;; -----------------------------------------------------------------------------
;;; Define acceleration of the cursor movement commands.

(require 'accelerate)
(accelerate previous-line 2)
(accelerate next-line 2)
(accelerate backward-char 3)
(accelerate forward-char 3)
(accelerate up-one 2)
(accelerate down-one 2)
(accelerate dired-previous-line 2)
(accelerate dired-next-line 2)
(accelerate speedbar-prev 2)
(accelerate speedbar-next 2)

;; -----------------------------------------------------------------------------
;;; Create an alias for top

(defalias 'top 'proced
  "Alias for `top' as proced, useful when using `shell'.")

;; -----------------------------------------------------------------------------
;;; Define double key-bindings for home and end keys
;;  to move the cursor to the beginning/end of the line on first press and
;;  buffer on second press

(use-package sequential-command
  :config
  (require 'sequential-command-config))
(global-set-key [(home)] 'seq-home)
(global-set-key [(end)] 'seq-end)

;; -----------------------------------------------------------------------------
;;; Make the prefix key `C-z' for my personal keymap.
;; On dvorak-keyboards `C-t' is one of the most accessible keys.
;;     but used by stumpwm
;; On qwerty-keyboards `C-z' is one of the most accessible keys.
;;     and good enough on dvorak-keyboards
(define-key my-map [(control ?u)] 'my-rotate-windows)
(define-key my-map [(down)] 'duplicate-start-of-line-or-region)
(define-key my-map "i" 'goto-last-change)
(define-key my-map "I" 'goto-last-change-reverse)
(define-key my-map "R" 'refill-mode)
(define-key my-map "f" 'ffap)
(define-key my-map "h" 'hl-line-mode)
(define-key my-map "k" 'kill-buffer-and-frame)
(define-key my-map "l" 'linum-mode)
(define-key my-map "o" 'occur-by-moccur)
(define-key my-map "r" 'revert-buffer)
(define-key my-map "S" 'multishell)
(define-key my-map [(control ?s)] 'support)
(define-key my-map "t" 'toggle-truncate-lines)
(define-key my-map "v" 'find-file-other-frame)

(define-key my-map "F" (lambda () (interactive) (font-lock-mode)))
(define-key my-map "H" 'my-goto-home)
(define-key my-map "M" 'menu-bar-mode)
(define-key my-map "T" 'tool-bar-mode)

(define-key my-map "1" 'my-single-frame)
(define-key my-map "2" 'my-double-frame)

;; -----------------------------------------------------------------------------
;;; Enable font-lock for makefiles
(add-hook 'makefile-mode-hook 'font-lock-mode)

;; -----------------------------------------------------------------------------
;;; Load personal information

(load "personal/init-personalisation")

;; -----------------------------------------------------------------------------
;;; init-personalisation.el  ends here.
