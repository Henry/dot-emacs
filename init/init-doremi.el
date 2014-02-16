;;; init-doremi.el --- Initialize DoReMi
;;;  Incremental change using arrow keys or mouse wheel
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/doremi"))

(require 'doremi)
(require 'doremi-frm)  ;; Incrementally adjust frame properties
(require 'doremi-cmd)  ;; Other Do Re Mi commands

(defvar my-doremi-map (make-sparse-keymap "Do Re Mi"))
(define-key my-map "d" my-doremi-map)
(define-key my-doremi-map "b" 'doremi-buffers)
(define-key my-doremi-map "g" 'doremi-global-marks)
(define-key my-doremi-map "m" 'doremi-marks)
(define-key my-doremi-map "r" 'doremi-bookmarks)
(define-key my-doremi-map "f" 'doremi-frame-width) ;; Frame resize
(define-key my-doremi-map "w" 'doremi-window-width) ;; Window resize
(define-key my-doremi-map "p" 'doremi-frame-horizontally)
(define-key my-doremi-map [return] 'my-doremi-menu)
(define-key my-doremi-map [mouse-3] 'my-doremi-menu)
(define-key my-doremi-map [C-tab] 'icicle-complete-keys) ;; Show options

(defun my-doremi-menu ()
  (interactive)
  (popup-menu
   '("Do Re Mi"
     ["Buffers" doremi-buffers]
     ["Resize Window" doremi-window-width]
     ["Resize Frame" doremi-frame-width]
     ["Move Frame" doremi-frame-horizontally]
     ["Global Marks" doremi-global-marks]
     ["Marks in Buffer" doremi-marks]
     ["Bookmarks" doremi-bookmarks]
     )))

;; -----------------------------------------------------------------------------
;;; init-doremi.el ends here
