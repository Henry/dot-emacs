;;; Code2 / Side-by-Side Code editing perspective
;;;--------------------------------------------------

(require 'e2wm)
;(setq e2wm:debug t)

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
    (:name history :plugin history-list2)))

(defvar e2wm:c-code2-right-default 'prev)

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name       'code2
   :extend     'base
   :title      "Coding2"
   :init       'e2wm:dp-code2-init
   :main       'left
   :switch     'e2wm:dp-code2-switch
   :popup      'e2wm:dp-code2-popup
   :display    'e2wm:dp-code2-display
   :after-bury 'e2wm:dp-code2-after-bury
   :keymap     'e2wm:dp-code2-minor-mode-map
   :leave      'e2wm:dp-code2-leave))

(defun e2wm:dp-code2-init ()
  ;; Set the dirtree-file-widget to us e2wm:dirtree-select
  (define-widget 'dirtree-file-widget 'push-button
    "File widget."
    :format         "%[%t%]\n"
    :button-face    'default
    :notify         'e2wm:dirtree-select)
  (add-hook 'minibuffer-setup-hook 'e2wm:override-setup-completion)
  (let*
      ((code2-wm
        (wlf:no-layout
         e2wm:c-code2-recipe
         e2wm:c-code2-winfo))
       (buf (or e2wm:prev-selected-buffer
                (e2wm:history-get-main-buffer))))

    (when (e2wm:history-recordable-p e2wm:prev-selected-buffer)
      (e2wm:history-add e2wm:prev-selected-buffer))

    (wlf:set-buffer code2-wm 'left buf)
    (cond
     ((eq e2wm:c-code2-right-default 'left)
      (wlf:set-buffer code2-wm 'right buf))
     ((eq e2wm:c-code2-right-default 'prev)
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
  (customize-set-variable 'display-buffer-alist nil)
  (remove-hook 'minibuffer-setup-hook 'e2wm:override-setup-completion))

(defun e2wm:dp-code2-switch (buf)
  "Switch to the buffer BUF staying in the same window if left or right"
  (e2wm:message "#DP CODE2 switch : %s" buf)
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (cond
     ((eql curwin (wlf:get-window wm 'left)) ; in left window
      (e2wm:pst-update-windows)
      (e2wm:pst-buffer-set 'left buf))
     ((eql curwin (wlf:get-window wm 'right)) ; in right window
      (e2wm:pst-update-windows)
      (e2wm:pst-buffer-set 'right buf))
     (t nil))))

(defvar e2wm:c-code2-show-left-regexp
   "COMMIT_EDITMSG")

(defvar e2wm:c-code2-show-right-regexp
   "\\*\\(Help\\|grep\\|Compilation\\|magit\\)")

(defun e2wm:dp-code2-popup (buf)
  "Show the buffer BUF in sub if it is not recordable, a document buffer or
specifically allocated to either the left or right windows by regexp.
Otherwise show and select it."
  (e2wm:message "#DP CODE2 popup : %s" buf)
  (let ((buf-name (buffer-name buf)))
    (message "buf-name" buf-name)
   (cond
    ((e2wm:document-buffer-p buf)
     (e2wm:pst-buffer-set 'right buf)
     t)
    ((e2wm:history-recordable-p buf)
     (e2wm:pst-show-history-main)
     t)
    ((and e2wm:c-code2-show-left-regexp
          (string-match e2wm:c-code2-show-left-regexp buf-name))
     (e2wm:pst-buffer-set 'left buf t)
     t)
    ((and e2wm:c-code2-show-right-regexp
          (string-match e2wm:c-code2-show-right-regexp buf-name))
     (e2wm:pst-buffer-set 'right buf t)
     t)
    (t
     (e2wm:dp-code2-popup-sub buf)
     t))))

(defun e2wm:dp-code2-popup-sub (buf)
  (let ((wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t not-minibufp))))

(defun e2wm:dp-code2-display (buf)
  "Show the buffer BUF in sub if it is not recordable, a document buffer or
specifically allocated to either the left or right windows by regexp.
Do not select the buffer."
  (e2wm:message "#DP CODE2 display : %s" buf)
  (let ((buf-name (buffer-name buf)))
    (message "buf-name" buf-name)
    (cond
     ((or (e2wm:history-recordable-p buf) ; we don't need to distinguish
          (e2wm:document-buffer-p buf))   ; these two as we don't select
      (let ((wm (e2wm:pst-get-wm))
            (curwin (selected-window)))
        ;; show in the other window, but don't select.
        (if (eql curwin (wlf:get-window wm 'left))
            (e2wm:pst-buffer-set 'right buf)
          (e2wm:pst-buffer-set 'left buf)))
      (e2wm:pst-update-windows)        ; update plugins, etc.
      t)
     ((and e2wm:c-code2-show-left-regexp
           (string-match e2wm:c-code2-show-left-regexp buf-name))
      (e2wm:pst-buffer-set 'left buf t)
      t)
     ((and e2wm:c-code2-show-right-regexp
           (string-match e2wm:c-code2-show-right-regexp buf-name))
      (e2wm:pst-buffer-set 'right buf t)
      t)
     (t
      (e2wm:pst-buffer-set 'sub buf t)
      t))))

(defun e2wm:dp-code2-after-bury (buried-buffer window)
  "Close sub window if it is the current window."
  (e2wm:$pst-class-super)
  (let ((wm (e2wm:pst-get-wm)))
    (when (eq (wlf:get-window-name wm window) 'sub)
      (wlf:hide wm 'sub)
      (wlf:select wm (e2wm:$pst-main (e2wm:pst-get-instance))))))

;; Commands / Keybindings

(defun e2wm:dp-code2 ()
  (interactive)
  (e2wm:pst-change 'code2))

(defun e2wm:dp-code2-history-toggle-command ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'history)
  (e2wm:pst-update-windows))
(defun e2wm:dp-code2-sub-toggle-command ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'sub)
  (e2wm:pst-update-windows))

(defun e2wm:dp-code2-navi-left-command ()
  (interactive)
  (e2wm:pst-window-select 'left))
(defun e2wm:dp-code2-navi-right-command ()
  (interactive)
  (e2wm:pst-window-select 'right))
(defun e2wm:dp-code2-navi-files-command ()
  (interactive)
  (e2wm:pst-window-select 'files))
(defun e2wm:dp-code2-navi-sub-command ()
  (interactive)
  (e2wm:pst-window-select 'sub))
(defun e2wm:dp-code2-navi-history-command ()
  (interactive)
  (e2wm:pst-window-select 'history))

(defun e2wm:dp-code2-update-history-list ()
  (e2wm:plugin-exec-update-by-plugin-name
   (selected-frame) (e2wm:pst-get-wm) 'history-list2))

(defun e2wm:dp-code2-double-column-command ()
  (interactive)
  (e2wm:pst-buffer-set 'right (e2wm:history-get-main-buffer))
  (e2wm:dp-code2-update-history-list))

(defun e2wm:dp-code2-right-history-forward-command ()
  (interactive)
  (e2wm:pst-buffer-set
   'right (e2wm:history-get-next
           (e2wm:pst-buffer-get 'right)))
  (e2wm:dp-code2-update-history-list))

(defun e2wm:dp-code2-right-history-back-command ()
  (interactive)
  (e2wm:pst-buffer-set
   'right (e2wm:history-get-prev
           (e2wm:pst-buffer-get 'right)))
  (e2wm:dp-code2-update-history-list))

(defalias 'e2wm:dp-code2-right-history-up-command
  'e2wm:dp-code2-right-history-forward-command)
(defalias 'e2wm:dp-code2-right-history-down-command
  'e2wm:dp-code2-right-history-back-command)

(defun e2wm:dp-code2-swap-buffers-command ()
  (interactive)
  (let ((left  (e2wm:pst-buffer-get 'left))
        (right (e2wm:pst-buffer-get 'right)))
    (e2wm:pst-buffer-set 'left  right)
    (e2wm:pst-buffer-set 'right left)
  (e2wm:dp-code2-update-history-list)))

(defun e2wm:dp-code2-main-maximize-toggle-command ()
  (interactive)
  (wlf:toggle-maximize (e2wm:pst-get-wm) 'left))

(defvar e2wm:dp-code2-minor-mode-map
  (e2wm:define-keymap
   '(("prefix d" . e2wm:dp-code2-double-column-command)
     ("prefix S" . e2wm:dp-code2-sub-toggle-command)
     ("prefix -" . e2wm:dp-code2-swap-buffers-command)
     ("prefix N" . e2wm:dp-code2-right-history-down-command)
     ("prefix P" . e2wm:dp-code2-right-history-up-command)
     ("prefix H" . e2wm:dp-code2-history-toggle-command)
     ("prefix M" . e2wm:dp-code2-main-maximize-toggle-command))
   e2wm:prefix-key))

;;; history-list2
;;;--------------------------------------------------

;; Add mouse-1 as a select key
(define-key e2wm:def-plugin-history-list2-mode-map [mouse-1]
  'e2wm:def-plugin-history-list2-select-command)

(provide 'e2wm-code2)
;;; e2wm-code2.el ends here
