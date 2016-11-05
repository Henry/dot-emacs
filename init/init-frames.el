;;; init-frames.el --- Frame configuration
;; -----------------------------------------------------------------------------
;;;  Default frame size
(defvar my-default-frame-width 80)
(defvar my-default-frame-height 60)

;;;  Default background colour
(defvar my-default-background-color "white")

;;;  Set the default screen sizes
(setq default-frame-alist
      (append
       (list
        '(top . 100)
        '(left . 400)
        (cons 'width my-default-frame-width)
        (cons 'height my-default-frame-height)
        '(cursor-type . bar)
        )
       default-frame-alist))

;;;  Set the initial screen location to be top-left
(setq initial-frame-alist
      (append
       (list
        '(top . 0) '(left . 0)
        '(cursor-type . bar)
        )
       default-frame-alist))

;;; Set the frame title
;;  Replace $HOME with ~ in file names in title bars

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

;;; Kill the buffer and the frame it is in
(defun kill-buffer-and-frame ()
  "Kill the buffer and the frame it is in"
  (interactive)
  (kill-current-buffer)
  (delete-frame))

;;; Enlarge frame and split into two windows
(defun my-double-frame ()
  "Enlarge frame and split into two windows"
  (interactive)
  (delete-other-windows)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) my-default-frame-height)
  (set-frame-width (selected-frame) (+ (* 2 my-default-frame-width) 5))
  (split-window-horizontally))

;;; Unsplit frame and return to normal size
(defun my-single-frame ()
  "Enlarge frame and split into two windows"
  (interactive)
  (delete-other-windows)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) my-default-frame-height)
  (set-frame-width (selected-frame) my-default-frame-width))

;; -----------------------------------------------------------------------------
;;; init-frames.el ends here
