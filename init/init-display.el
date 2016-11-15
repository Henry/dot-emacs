;;; init-display.el --- Display configuration
;; -----------------------------------------------------------------------------
;;;  Minibuffer depth indicator
(minibuffer-depth-indicate-mode 99)

;; -----------------------------------------------------------------------------
;;;  Switch off GTK tooltips
(setq  x-gtk-use-system-tooltips nil)

;; -----------------------------------------------------------------------------
;;; toggle-selective-display: display lines starting at given column

(defun my-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column ; disable toggle if column was supplied
       (unless selective-display 1))))

;; -----------------------------------------------------------------------------
;;; linum --- Display line numbers
;;;  for the current buffer
;;;  Toggle display of line numbers with M-x linum-mode
(require 'linum)

;; -----------------------------------------------------------------------------
;;; Show matching parentheses
(defun show-matching-paren ()
  "Show matching parentheses in extra-bold font"
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1))

;; -----------------------------------------------------------------------------
;;; Turn-off font-lock for Postscript files
(add-hook 'postscript-mode-hook 'turn-off-font-lock)

;; -----------------------------------------------------------------------------
;;; Battery status
;; If not on AC power line, then display battery status on the mode line
(and (require 'battery nil t)
     (functionp 'battery-status-function)
     (or (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
         (display-battery-mode)))

;; -----------------------------------------------------------------------------
;;; init-display.el ends here
