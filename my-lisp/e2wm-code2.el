;;; Code2 / Side-by-Side Code editing perspective
;;;--------------------------------------------------

(require 'e2wm)

;;; Use right window for special buffers
;;;--------------------------------------------------

(defun e2wm:display-special-buffer (buf alist)
  "Put the special buffers in the bottom-right window"
  (let ((target-window (window-at (- (frame-width) 4) (- (frame-height) 4)))
        (pop-up-windows t))
    (set-window-buffer target-window buf)
    target-window))

(defun e2wm:special-buffer (buf alist)
  (if (e2wm:managed-p)
      (string-match "\\(\\*\\(Help\\|grep\\|Compilation\\|magit\\)\\|COMMIT\\)" buf)
    nil))

;;; Use bottom-left window for completion buffer
;;;--------------------------------------------------

(defun e2wm:display-completion-buffer (buf alist)
  "Put the special buffers in the bottom-left window"
  (let ((target-window (window-at 0 (- (frame-height) 4)))
        (pop-up-windows t))
    (set-window-buffer target-window buf)
    target-window))

(defun e2wm:completion-buffer (buf alist)
  (if (e2wm:managed-p)
      (string-match "*Completions*" buf)
    nil))

;;; Speedbar plugin
;;;--------------------------------------------------

;; (require 'sr-speedbar)

;; (defun e2wm:def-plugin-speedbar (frame wm winfo)
;;   (let ((buf (get-buffer "*SPEEDBAR*")))
;;     (unless (and buf (buffer-live-p buf))
;;       (select-window (wlf:get-window wm (wlf:window-name winfo)))
;;       (if (not (sr-speedbar-exist-p))
;;           (sr-speedbar-open)))))

;; (e2wm:plugin-register 'speedbar
;;                      "Speedbar"
;;                      'e2wm:def-plugin-speedbar)

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

;;; Code2
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

(defvar e2wm:c-code2-show-main-regexp
   "\\*\\(vc-diff\\)\\*")

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name       'code2
   :extend     'two
   :title      "Coding2"
   :init       'e2wm:dp-code2-init
   :leave      'e2wm:dp-code2-leave))

(defun e2wm:dp-code2-init ()
  ;; Set the dirtree-file-widget to us e2wm:dirtree-select
  (define-widget 'dirtree-file-widget 'push-button
    "File widget."
    :format         "%[%t%]\n"
    :button-face    'default
    :notify         'e2wm:dirtree-select)
  (customize-set-variable
   'display-buffer-alist
   '((e2wm:completion-buffer e2wm:display-completion-buffer)
     (e2wm:special-buffer e2wm:display-special-buffer)))
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
  ;;(kill-buffer (get-buffer-create "*SPEEDBAR*"))
  (kill-buffer (get-buffer-create dirtree-buffer))
  ;; Reset dirtree-file-widget
  (define-widget 'dirtree-file-widget 'push-button
    "File widget."
    :format         "%[%t%]\n"
    :button-face    'default
    :notify         'dirtree-select)
  (customize-set-variable 'display-buffer-alist nil))

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
