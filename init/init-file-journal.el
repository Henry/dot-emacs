;;; init-file-journal.el --- File-journal settings
;; -----------------------------------------------------------------------------
;; Work with files as usual and use M-x fj-show to revisit them later by date.

(setq fj-journal-file (expand-file-name "~/Emacs/file-journal"))
(require 'file-journal)
(defadvice switch-to-buffer-other-frame (after fj-switch-to-buffer activate)
  (fj-record-file))

;; For automatic saving settings see init-save-all-hist.el

;; -----------------------------------------------------------------------------
;;; init-file-journal.el ends here
