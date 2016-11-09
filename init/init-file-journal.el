;;; init-file-journal.el --- File-journal settings
;; -----------------------------------------------------------------------------
;; Work with files as usual and use M-x fj-show to revisit them later by date.

(use-package file-journal
  :init
  (setq fj-journal-file (expand-file-name "~/Emacs/file-journal"))
  :config
  (defadvice switch-to-buffer-other-frame (after fj-switch-to-buffer activate)
    (fj-record-file)))

;; For automatic saving settings see init-history-saving.el

;; -----------------------------------------------------------------------------
;;; init-file-journal.el ends here
