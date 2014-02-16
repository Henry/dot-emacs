;;; init-bookmark.el --- Enhance bookmark handling
;; -----------------------------------------------------------------------------

(defadvice bookmark-jump (after bookmark-jump activate)
  "Move the last selected bookmark first in the list."
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;; -----------------------------------------------------------------------------
;;; bm: Visible bookmarks

;; Make sure the repository is loaded as early as possible
(setq bm-restore-repository-on-load t)

(require 'bm)

;; Set the location of the repository file
(setq bm-repository-file (expand-file-name "~/Emacs/BM-repository"))

;;;  Settings for persistency

;; Make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Restore bookmarks when buffer is reverted.
;;(add-hook 'after-revert-hook 'bm-buffer-restore)

;; For automatic saving settings see init-save-all-hist.el

;;;  Additional functionality in the `show' buffer

(defun bm-show-remove-bookmark nil
  "Remove the bookmark on current line in the *bm-bookmarks* buffer."
  (interactive)
  (let ((buffer-name (get-text-property (point) 'bm-buffer))
        (bookmark (get-text-property (point) 'bm-bookmark)))
    (if (null buffer-name)
        (message "No bookmark at this line.")
      (bm-bookmark-remove bookmark)
      (bm-show-all))))

;;;  Key-bindings

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<M-f2>") 'bm-show-all)
(define-key bm-show-mode-map "d" 'bm-show-remove-bookmark)

;; -----------------------------------------------------------------------------
;;; init-bookmark.el ends here
