;;; init-diary.el  --- Initialize diary and calendar
;; -----------------------------------------------------------------------------

(setq
 diary-file (concat user-emacs-directory "Diaries/diary")
 my-diary-file-list (list (concat user-emacs-directory "Diaries/anniversaries"))
 diary-number-of-entries 7)

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'my-diary-include-file-list)
(add-hook 'diary-list-entries-hook 'diary-sort-entries)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(add-hook 'diary-mark-entries-hook 'my-diary-mark-included-file-list)

(setq
  calendar-date-style 'european
  calendar-view-diary-initially-flag nil
  calendar-mark-diary-entries-flag t
  calendar-today-marker 'calendar-today
  calendar-mark-holidays-flag t

  cal-tex-holidays t
  cal-tex-diary t

  holiday-general-holidays
  '((holiday-fixed 1 1 "New Year's Day")
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-easter-etc -21 "Mother's Day")
    (holiday-float 6 0 3 "Father's Day"))

  calendar-holidays
  (append
   holiday-general-holidays
   holiday-local-holidays
   holiday-other-holidays
   holiday-christian-holidays)
)

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(require 'cal-tex)

(setq cal-tex-caldate
      "\\fboxsep=0pt
\\long\\def\\caldate#1#2#3#4#5#6{%
    \\fbox{\\hbox to\\cellwidth{%
     \\vbox to\\cellheight{%
       \\hbox to\\cellwidth{%
          {\\hspace*{1mm}\\Large \\bf \\strut #2}\\hspace{.05\\cellwidth}%
          \\raisebox{\\holidaymult\\cellheight}%
                   {\\parbox[t]{.75\\cellwidth}{\\tiny \\raggedright #4}}}
       \\hbox to\\cellwidth{%
           \\hspace*{1mm}\\parbox{.95\\cellwidth}{\\tiny \\raggedright #3}}
       \\hspace*{1mm}%
       \\hbox to\\cellwidth{#6}%
       \\vfill%
       \\hbox to\\cellwidth{\\hfill \\small #5 \\hfill}%
       \\vskip 1.4pt}%
     \\hskip -0.4pt}}}
")


(defun my-diary-include-files (file-name-list)
  "Include the diary entries from the given `file-name-list' of diary files
with those of `diary-file'.
This function is suitable for use in `diary-list-entries-hook';
it enables you to use shared diary files together with your own without
introducing the cyclic dependency possible with the
`diary-include-other-diary-files' mechanism if the other diary files included
contain include statements to yours.
Can be used with `diary-include-other-diary-files'."
  (while file-name-list
    (let* ((diary-file (car file-name-list))
           (diary-list-include-blanks nil)
           (diary-list-entries-hook nil)
           (diary-display-hook 'ignore)
           (diary-hook nil))
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (progn
                (setq diary-entries-list
                      (append diary-entries-list
                              (diary-list-entries original-date number)))
                (with-current-buffer (find-buffer-visiting diary-file)
                  (diary-unhide-everything)))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2)))
    (setq file-name-list (cdr file-name-list))))

(defun my-diary-include-file-list ()
  "Include the diary entries from the file name list `my-diary-file-list'
of diary files with those of diary-file.
See also the documentation for `my-diary-include-files '."
  (my-diary-include-files my-diary-file-list))


(defun my-diary-mark-included-files (file-name-list)
  "Mark the diary entries from the given `file-name-list' of diary files
with those of `diary-file'.
This function is suitable for use in `diary-mark-entries-hook';
it enables you to use shared diary files together with your own without
introducing the cyclic dependency possible with the
`diary-mark-other-diary-files' mechanism if the other diary files included
contain include statements to yours.
Can be used with `diary-mark-other-diary-files'."
  (while file-name-list
    (let* ((diary-file (car file-name-list))
           (diary-mark-entries-hook nil)
           (dbuff (find-buffer-visiting diary-file)))
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (progn
                (diary-mark-entries)
                (unless dbuff
                  (kill-buffer (find-buffer-visiting diary-file))))
            (beep)
            (message "Can't read listed diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2)))
    (setq file-name-list (cdr file-name-list))))

(defun my-diary-mark-included-file-list ()
  "Mark the diary entries from the file name list `my-diary-file-list'
of diary files with those of diary-file.
See also the documentation for `my-diary-mark-included-files'."
  (my-diary-mark-included-files my-diary-file-list))

(defun autoload-init-diary ()
  (interactive)
  "Cause init-diary.el to be loaded via the autoload in init.el.")


;; -----------------------------------------------------------------------------
;;; calfw settings
(use-package calfw
  :ensure t
  :ensure howm
  :config
  ;; Load personal information
  (load "personal/init-diary"))

(use-package calfw-cal :ensure t)
(use-package calfw-org :ensure t)
(use-package calfw-ical :ensure t)

;; -----------------------------------------------------------------------------
;;; init-diary.el ends here
