;;; init-bookmark.el --- Enhance bookmark handling
;; -----------------------------------------------------------------------------

(defadvice bookmark-jump (after bookmark-jump activate)
  "Move the last selected bookmark first in the list."
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;; -----------------------------------------------------------------------------
;;; bm: Visible bookmarks
(use-package bm
  :ensure t
  :init
  (setq
   ;; Make sure the repository is loaded as early as possible
   bm-restore-repository-on-load t
   ;; Set the location of the repository file
   bm-repository-file (expand-file-name "BM-repository" user-emacs-directory))

  ;; Make bookmarks persistent as default
  (setq-default bm-buffer-persistence t)
  :bind (("C-<f2>" . bm-toggle)
         ("<f2>"   . bm-next)
         ("S-<f2>" . bm-previous)
         ("M-<f2>" . bm-show-all)
         :map bm-show-mode-map ("d" . bm-show-remove-bookmark))
  :config
  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)
  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore))

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

;; -----------------------------------------------------------------------------
;;; init-bookmark.el ends here
