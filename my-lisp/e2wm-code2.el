;;; Code2 / Side-by-Side Code editing perspective
;;;--------------------------------------------------

(require 'e2wm)

;;; Use middle window for source buffers
;;;--------------------------------------------------

(defun e2wm:code2-display-main-buffer (buf alist)
  "Put the source buffers in the bottom-middle window"
  (let ((target-window (window-at (/ (frame-width) 2) (- (frame-height) 4)))
        (pop-up-windows t))
    (set-window-buffer target-window buf)
    (e2wm:history-add buf)
    (e2wm:pst-show-history-main)
    target-window))

;;; Use right window for special buffers
;;;--------------------------------------------------

(defun e2wm:code2-display-special-buffer (buf alist)
  "Put the special buffers in the bottom-right window"
  (let ((target-window (window-at (- (frame-width) 4) (- (frame-height) 4)))
        (pop-up-windows t))
    (set-window-buffer target-window buf)
    target-window))

;;; Use bottom-left window for completion buffer
;;;--------------------------------------------------

(defun e2wm:code2-display-completion-buffer (buf alist)
  "Put the special buffers in the bottom-left window"
  (let ((target-window (window-at 0 (- (frame-height) 4)))
        (pop-up-windows t))
    (set-window-buffer target-window buf)
    target-window))

;;; Dirtree plugin
;;;--------------------------------------------------

(require 'windata)
(require 'tree-mode)
(require 'dirtree)

(defun e2wm:dirtree-select (node &rest ignore)
  "Open file in main window"
  (let ((file (widget-get node :file)))
    (when file
      (e2wm:history-add (find-file-noselect file))
      (e2wm:pst-show-history-main)
      (e2wm:pst-window-select-main))))

(defun e2wm:def-plugin-dirtree (frame wm winfo)
  (let ((wname (wlf:window-name winfo))
        (win (wlf:window-live-window winfo))
        (buf (get-buffer dirtree-buffer))
        tree)
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create dirtree-buffer))
      (with-current-buffer buf
        (unless (eq major-mode 'dirtree-mode)
          (dirtree-mode))
        (dolist (atree tree-mode-list)
          (if (string= (widget-get atree :file) ".")
              (setq tree atree)))
        (or tree
            (setq tree (tree-mode-insert (dirtree-root-widget ".")))))
      (with-selected-window win
        (unless (widget-get tree :open)
          (widget-apply-action tree))
        (goto-char (widget-get tree :from))
        (recenter 1)))
    (wlf:set-buffer wm wname buf)))

(e2wm:plugin-register 'dirtree
                     "dirtree"
                     'e2wm:def-plugin-dirtree)

;;; Code2 perspective
;;;--------------------------------------------------

(defvar e2wm:c-code2-recipe
  '(| (:left-max-size 50)
      (- (:upper-size-ratio 0.7)
         files history)
      (- (:upper-size-ratio 0.7)
         (| (:left-size 81) left right)
         sub)))

(defvar e2wm:c-code2-winfo
  '((:name left)
    (:name right)
    (:name files :plugin dirtree)
    (:name sub :buffer "*Help*" :default-hide t)
    (:name history :plugin history-list)))

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name       'code2
   :extend     'two
   :title      "Coding2"
   :init       'e2wm:dp-code2-init
   :leave      'e2wm:dp-code2-leave
   :popup      'e2wm:dp-code2-popup
   :switch     'e2wm:dp-code2-popup
   :display    'e2wm:dp-code2-popup))

(defun e2wm:dp-code2-init ()
  ;; Set the dirtree-file-widget to us e2wm:dirtree-select
  (define-widget 'dirtree-file-widget 'push-button
    "File widget."
    :format         "%[%t%]\n"
    :button-face    'default
    :notify         'e2wm:dirtree-select)
  ;; Set the window for particular buffers
  (customize-set-variable
   'display-buffer-alist
   '(
     ("*Completions*"
      . (e2wm:code2-display-completion-buffer . nil))
     ("\\*\\(Help\\|grep\\|Compilation\\|magit\\)"
      . (e2wm:code2-display-special-buffer . nil))
     ("COMMIT"
      . (e2wm:code2-display-main-buffer . nil))
     (".*"
      . (e2wm:code2-display-special-buffer . nil))
     ))
  ;; Set the default window to main
  (customize-set-variable
   'display-buffer-base-action '(e2wm:code2-display-main-buffer))
  (let*
      ((code2-wm
        (wlf:no-layout
         e2wm:c-code2-recipe
         e2wm:c-code2-winfo))
       (buf (or e2wm:prev-selected-buffer
                (e2wm:history-get-main-buffer))))

    (wlf:set-buffer code2-wm 'left buf)
    (cond
     ((eq e2wm:c-two-right-default 'left)
      (wlf:set-buffer code2-wm 'right buf))
     ((eq e2wm:c-two-right-default 'prev)
      (wlf:set-buffer code2-wm 'right (e2wm:history-get-prev buf)))
     (t
      (wlf:set-buffer code2-wm 'right (e2wm:history-get-prev buf))))
    code2-wm))

(defun e2wm:dp-code2-leave (wm)
  (kill-buffer (get-buffer-create dirtree-buffer))
  ;; Reset dirtree-file-widget
  (define-widget 'dirtree-file-widget 'push-button
    "File widget."
    :format         "%[%t%]\n"
    :button-face    'default
    :notify         'dirtree-select)
  (customize-set-variable 'display-buffer-alist nil))

(defun e2wm:dp-code2-popup (buf)
  "Show the buffer BUF in sub if it is not recordable or document buffer.
Otherwise show and select it."
  (e2wm:message "#DP CODE2 popup : %s" buf)
  (e2wm:code2-display-special-buffer buf nil))

(defun e2wm:dp-code2 ()
  (interactive)
  (e2wm:pst-change 'code2))

(defun e2wm:start-code2 ()
  (interactive)
  (e2wm:start-management)
  (e2wm:dp-code2))

;;; history-list
;;;--------------------------------------------------

;; Add mouse-1 as a select key
(define-key e2wm:def-plugin-history-list-mode-map [mouse-1]
  'e2wm:def-plugin-history-list-select-command)

(provide 'e2wm-code2)
;;; e2wm-code2.el ends here
