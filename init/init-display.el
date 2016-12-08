;;; init-display.el --- Display configuration
;; -----------------------------------------------------------------------------
;;;  Minibuffer depth indicator
(minibuffer-depth-indicate-mode 99)

;; -----------------------------------------------------------------------------
;;;  Switch off GTK tooltips
(setq x-gtk-use-system-tooltips nil)

;; -----------------------------------------------------------------------------
;;;  Put column number into modeline
(column-number-mode 1)

;; -----------------------------------------------------------------------------
;;;  Fill to column 80
(setq fill-column 80
      word-wrap t)

;; -----------------------------------------------------------------------------
;;;  Highlight cursor position in buffer
(use-package beacon
  :ensure t
  :commands (beacon-mode)
  :diminish beacon-mode
  :init
  (beacon-mode 1))

;; -----------------------------------------------------------------------------
;;; toggle-selective-display: display lines starting at given column

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column ; disable toggle if column was supplied
       (unless selective-display 1))))

;; -----------------------------------------------------------------------------
;;; linum --- Display line numbers
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
;;; Modeline
(use-package smart-mode-line
  :ensure t
  :ensure smart-mode-line-powerline-theme
  :init
  (setq sml/theme 'light-powerline
        powerline-default-separator 'curve
        powerline-default-separator-dir '(right . left)
        sml/position-percentage-format " %p")
  (sml/setup)

  (set-face-attribute 'mode-line nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width -1 :color "cornsilk"))
  (set-face-attribute 'sml/position-percentage nil :background "grey85")

  (setq-default
   mode-line-position
   '(sml/position-percentage-format
     (-4
      (:propertize
       (:eval sml/position-percentage-format)
       local-map
       (keymap
        (mode-line
         keymap
         (down-mouse-1
          keymap
          (column-number-mode
           menu-item "Display Column Numbers"
           column-number-mode
           :help "Toggle displaying column numbers in the mode-line"
           :button (:toggle . column-number-mode))
          (line-number-mode
           menu-item "Display Line Numbers"
           line-number-mode
           :help "Toggle displaying line numbers in the mode-line"
           :button (:toggle . line-number-mode))
          "Toggle Line and Column Number Display")))
       mouse-face
       mode-line-highlight
       face
       sml/position-percentage
       help-echo
       "Buffer Relative Position\nmouse-1: Display Line and Column Mode Menu"))))

  (setq-default
   mode-line-format
   '("%e"
     mode-line-front-space
     mode-line-position
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-frame-identification
     mode-line-buffer-identification
     sml/pos-id-separator
     (vc-mode vc-mode)
     sml/pre-modes-separator
     mode-line-modes
     mode-line-misc-info
     mode-line-end-spaces))
  )

;; -----------------------------------------------------------------------------
;;; init-display.el ends here
