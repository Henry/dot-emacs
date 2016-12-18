;;; alarm.el --- Simple audible and visible alarm
;;
;; Filename: alarm.el
;; Description:
;; Author: Henry G. Weller
;; Maintainer:
;; Copyright (C) 2016, Henry G. Weller, all rights reserved.
;; Created: Fri Dec 16 16:06:43 2016 (+0000)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;; -----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;;
;;
;; -----------------------------------------------------------------------------
;;
;;; Change Log:
;;
;;
;; -----------------------------------------------------------------------------
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

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

(provide 'alarm)

;; -----------------------------------------------------------------------------
;;; alarm.el ends here
