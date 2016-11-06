;;; init-thingatpt.el --- Enhance thing at point

;; -----------------------------------------------------------------------------
;;; `thing'-based structured editing
(use-package thing-cmds
  :ensure thingatpt+
  :ensure hide-comnt
  :init
  ;; (setq-default
  ;;  thing-types
  ;;  '("word" "line" "sentence" "symbol" "string" "paragraph" "page"))

  ;; Copy thing-at-point into kill-ring
  (defun kill-ring-save-thingatpt ()
    "If region is not active call `mark-thing' to mark then then call
 `kill-ring-save' to save the marked region to the kill-ring.

This command is to be used interactively."
    (interactive)
    (unless (use-region-p)
      (call-interactively 'mark-thing))
    (call-interactively 'kill-ring-save))

  :bind (([(control meta ? )] . mark-thing)  ; replaces mark-sexp
         ([(meta ? )] . cycle-thing-region)  ; replaces just-one-space
         ("M-w" . kill-ring-save-thingatpt)  ; relaces kill-ring-save
         ))

(require 'thingatpt-ext)

;; -----------------------------------------------------------------------------
;;; init-thingatpt.el ends here
