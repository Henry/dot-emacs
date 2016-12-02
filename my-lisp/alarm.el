(defvar alarm-timer nil
  "Store timer for `alarm-cancel'")

(defun alarm-sound-message (text)
  "Play alarm sound and then print message in pop-up."
  (play-sound-file "~/Emacs/alarm.wav" 100)
  (message-box text))

(defun alarm-set-internal (time message)
  "Set alarm time and message.
The time format is that used by `run-at-time', e.g. \"3:10pm\"."
  (setq alarm-timer
        (run-at-time time nil 'alarm-sound-message message)))

(defun alarm-set ()
  "Interactively set alarm time and message.
The time format is that used by `run-at-time', e.g. \"3:10pm\"."
  (interactive)
  (let ((time (read-string "Time: "))
        (message (read-string "Message: ")))
    (alarm-set-internal time message)))

(defun alarm-cancel ()
  "Cancel the alarm clock"
  (interactive)
  (cancel-timer alarm-timer))

;; (alarm-set-internal "19:34" "Test")
;; (alarm-cancel)
