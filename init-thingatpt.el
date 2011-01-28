;;; init-thingatpt.el --- Enhance thing at point
;; -----------------------------------------------------------------------------
(require 'thingatpt-ext)

;; -----------------------------------------------------------------------------
;;; `thing'-based structured editing
(require 'thing-cmds)

(setq-default thing-types
      '("word" "line" "sentence" "symbol" "string" "paragraph" "page"))

(global-set-key [(control meta ? )] 'mark-thing) ; replaces mark-sexp
(global-set-key [(meta ? )] 'cycle-thing-region) ; replaces just-one-space

;; -----------------------------------------------------------------------------
;;; Copy thing-at-point intelligently
;;;  http://www.emacswiki.org/emacs/Leo
;; Make M-w do more intelligently.
;; If region is active, behave like normal. Otherwise, do the following:
;;   * M-w copy url, email or current line, in that order
;;   * M-w followed by key f, l, s or w, copy filename, list, sexp and word
;;   * with numeric prefix, copy that many thing-at-point

(eval-when-compile (require 'cl))

(defun sdl-kill-ring-save-thing-at-point (&optional n)
  "Save THING at point to kill-ring."
  (interactive "p")
  (let ((things '((?l . list) (?f . filename) (?w . word) (?s . sexp)))
        (message-log-max)               ; don't write to *Message*
        beg t-a-p thing event)
    (flet ((get-thing ()
                      (save-excursion
                        (beginning-of-thing thing)
                        (setq beg (point))
                        (if (= n 1)
                            (end-of-thing thing)
                          (forward-thing thing n))
                        (buffer-substring beg (point)))))
      ;; try detecting url email and fall back to 'line'
      (dolist (thing '(url email line))
        (when (bounds-of-thing-at-point thing)
          (setq t-a-p (get-thing))
          ;; remove the last newline character
          (when (and (eq thing 'line)
                     (>= (length t-a-p) 1)
                     (equal (substring t-a-p -1) "\n"))
            (setq t-a-p (substring t-a-p 0 -1)))
          (kill-new t-a-p)
          (message "%s" t-a-p)
          (return nil)))
      (setq event (read-event nil))
      (when (setq thing (cdr (assoc event things)))
        (clear-this-command-keys t)
        (if (not (bounds-of-thing-at-point thing))
            (message "No %s at point" thing)
          (setq t-a-p (get-thing))
          (kill-new t-a-p 'replace)
          (message "%s" t-a-p))
        (setq last-input-event nil))
      (when last-input-event
        (clear-this-command-keys t)
        (setq unread-command-events (list last-input-event))))))

(defun sdl-kill-ring-save-dwim ()
  "This command dwim on saving text.

If region is active, call `kill-ring-save'. Else, call
`sdl-kill-ring-save-thing-at-point'.

This command is to be used interactively."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (call-interactively 'sdl-kill-ring-save-thing-at-point)))

(global-set-key (kbd "M-w") 'sdl-kill-ring-save-dwim)

;; -----------------------------------------------------------------------------
;;; init-thingatpt.el ends here
