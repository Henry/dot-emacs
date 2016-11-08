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

;;;  Quick access and yank text from the kill ring.
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

;;;  Kill the whole line
;;  including its terminating newline by typing C-a C-k
;; when used at the beginning of a line
(setq kill-whole-line t)

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
  :ensure t)

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
;;; init-editing.el ends here
