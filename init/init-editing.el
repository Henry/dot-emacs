;;; init-editing.el --- General editing settings
;; -----------------------------------------------------------------------------
;;; General editing

;;;  Disable tabs; always indent with spaces
(setq-default indent-tabs-mode nil
              tab-width 4)

;;;  Untabify whole buffer
(defun untabify-buffer ()
  "Untabify whole buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;;;  Enable the command `narrow-to-region' ("C-x n n") undo ("C-x n w")
(put 'narrow-to-region 'disabled nil)

;;;  Initialize info path to $INFOPATH
(setq Info-directory-list
      (let ((path (getenv "INFOPATH")))
        (cond
         (path (split-string path path-separator))
         ((and (boundp 'Info-default-directory-list)
               Info-default-directory-list))
         (t '("/usr/local/info" "/usr/local/share/info" "/usr/share/info")))))

;;;  Remove trailing spaces on buffer save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;;;  Typed text replaces the selection if the selection is active
(delete-selection-mode t)

;;;  Query-replace should not preserve case in replacements
(setq case-replace nil)

;;;  Indent whole buffer according to mode
(defun indent-buffer ()
  "Indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

;;;  Kill the whole line
;;  including its terminating newline when used at the beginning of a line
;;  e.g. by typing C-a C-k
(setq kill-whole-line t)

;; -----------------------------------------------------------------------------
;;; Join current and next line

(defun my-fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
            (save-excursion (forward-char -1)
                            (looking-at "$\\|.(\\|\\s'")))
        nil
      (insert ?\s))))

(defun join-next-line()
  "Join current and next line by deleting the white space between
them leaving one space or none according to context"
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (my-fixup-whitespace))

(global-set-key (kbd "M-J") 'join-next-line)

;; -----------------------------------------------------------------------------
;;; Duplicate start of line or region

(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (let ((text (buffer-substring
               (point)
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
;;; browse-kill-ring+ --- Browse kill-ring using M-y
(use-package browse-kill-ring+
  :ensure t
  :bind (("C-M-y" . browse-kill-ring)))

;; -----------------------------------------------------------------------------
;;; Whole-line-or-region
;; Operate on the current line if they would normally operate on a region
;; and region is currently undefined (doesn't work with Emacs-23)
(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode)

;; -----------------------------------------------------------------------------
;;; undo-tree --- maintain and operate on undo/redo as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :bind (("M-i" . undo-tree-undo)
         ("M-u" . undo-tree-redo))
  :config
  (global-undo-tree-mode))

;; -----------------------------------------------------------------------------
;;; Better automatic commenting/un-commenting
;; From http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the
end of the line, then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the
end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(define-key emacs-lisp-mode-map "\C-c\C-c" 'comment-dwim-line)
(define-key lisp-mode-map "\C-c\C-c" 'comment-dwim-line)

;; -----------------------------------------------------------------------------
;;; Automatically indent pasted regions in all modes except c++

(use-package auto-indent-mode
  :ensure t
  :disabled t
  :diminish auto-indent-mode
  :init
  (setq auto-indent-indent-style 'aggressive)
  :config
  (add-to-list 'auto-indent-disabled-modes-list 'c++-mode)
  (auto-indent-global-mode))

;; -----------------------------------------------------------------------------
;;; init-editing.el ends here
