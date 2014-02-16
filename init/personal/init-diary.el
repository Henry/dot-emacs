;;; init-diary.el  --- Personal settings
;; -----------------------------------------------------------------------------

;;; calfw settings

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/emacs-calfw"))

(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-org)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    (cfw:cal-create-source "Orange") ; diary source
    ;;(cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    (cfw:ical-create-source "gcal" "https://www.google.com/calendar/ical/sws02hs%40googlemail.com/private-5fe24f55abfc5e576cb9d0eecfaed3b4/basic.ics" "IndianRed") ; google calendar ICS
   )))

;; -----------------------------------------------------------------------------
;;; init-diary.el ends here
