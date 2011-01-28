;;; init-ecb.el --- Initialize the emacs code browser
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/ecb"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/ecb/info-help") t)

(require 'ecb)

(setq ecb-tip-of-the-day nil)
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))

(ecb-layout-define "OF-2" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                  |                   |
   |              |                  |                   |
   |              |                  |                   |
   |  Directories |                  |                   |
   |              |                  |                   |
   |              |                  |                   |
   |              |                  |                   |
   |--------------|       Edit       |       Edit        |
   |              |                  |                   |
   |   Sources    |                  |                   |
   |              |                  |                   |
   |              |                  |                   |
   |--------------|                  |                   |
   |  History     |                  |                   |
   |              |                  |                   |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------
  "
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.65)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (ecb-split-hor 0.515)
)

(ecb-layout-define "OF-2-m" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                  |                   |
   |  Directories |                  |                   |
   |              |                  |                   |
   |--------------|                  |                   |
   |              |                  |                   |
   |  Sources     |                  |                   |
   |              |                  |                   |
   |--------------|       Edit       |       Edit        |
   |              |                  |                   |
   |  Methods     |                  |                   |
   |              |                  |                   |
   |              |                  |                   |
   |--------------|                  |                   |
   |  History     |                  |                   |
   |              |                  |                   |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------
  "
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.35)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.65)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (ecb-split-hor 0.515)
)


(setq ecb-windows-width 33)
(setq ecb-compile-window-height 12)
(setq ecb-compile-window-temporally-enlarge nil)

;; Allow shell to use an edit window
(delete '("*shell*" . nil) ecb-compilation-buffer-names)

(add-to-list 'ecb-compilation-buffer-names '("*Kill Ring*" . nil) t)

;; Add the git buffers to those displayed in the compilation window
(add-to-list 'ecb-compilation-buffer-names '("\\*xgit.*\\*" . t) t)
(add-to-list 'ecb-compilation-buffer-names '("\\*dvc.*\\*" . t) t)
(add-to-list 'ecb-compilation-buffer-names '("\\*magit.*\\*" . t) t)

;; This does not work well, bs insists on deleting the window but can't
;(add-to-list 'ecb-compilation-buffer-names '("*buffer-selection*" . nil) t)

(defun ecb-activate-2 ()
  "Activate ECB with a 2 edit window configuration, expanding the frame to
full screen to accommodate it"
  (interactive)
  (if (equal semantic-minor-modes-status nil)
      (ecb-layout-switch "OF-2")
    (ecb-layout-switch "OF-2-m"))
  (delete-other-windows)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) 70)
  (set-frame-width (selected-frame) 195)
  (ecb-activate)
  (set-frame-height (selected-frame) 70)
  (set-frame-width (selected-frame) 195)
  (ecb-redraw-layout-full nil nil nil nil)
  (enlarge-window-horizontally (- 79 (window-width))))

(defun ecb-deactivate-2 ()
  "Deactivate the 2 edit window ECB, shrinking the frame to a single window"
  (interactive)
  (set-frame-height (selected-frame) my-default-frame-height)
  (set-frame-width (selected-frame) my-default-frame-width)
  (ecb-deactivate)
  (set-frame-height (selected-frame) my-default-frame-height)
  (set-frame-width (selected-frame) my-default-frame-width))

;; -----------------------------------------------------------------------------
;;; init-ecb.el ends here
