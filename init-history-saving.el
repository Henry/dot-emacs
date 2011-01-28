;;; init-save-all-hists.el --- Enable all save all histories function

(defun enable-history-saving ()
  "Switch on the automatic saving of all history files."
  (interactive)

  ;; Save minibuffer command, search-string and kill-ring histories
  (savehist-mode 1)

  ;; Save places in files
  (setq-default save-place t)

  ;;(defvar fj-save-journal-timer
  ;;  (run-with-timer fj-save-timer-interval fj-save-timer-interval 'fj-save-journal))

  (add-hook 'after-save-hook 'fj-save-journal)
  ;;(add-hook 'kill-buffer-hook 'fj-save-journal)
  (add-hook 'kill-emacs-hook 'fj-save-journal)

  ;; Saving bookmark data on killing a buffer
  (add-hook 'kill-buffer-hook 'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))

  ;; Update bookmark repository when saving the file.
  ;;(add-hook 'after-save-hook 'bm-buffer-save)
  )

;; -----------------------------------------------------------------------------
;;; init-save-all-hists.el ends here
