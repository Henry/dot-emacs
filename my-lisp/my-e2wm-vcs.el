;;; my-e2wm-vcs.el --- VCS perspectives

;; Copyright (C) 2011  SAKURAI Masashi
;; Copyright (C) 2016  Henry G. Weller

;; Authors: SAKURAI Masashi <m.sakurai at kiwanami.net>, Henry G. Weller
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; e2wm perspective for magit.
;; One can change to the magit perspective by M-x e2wm:dp-magit.

;;; Code:

(require 'e2wm)
(require 'magit nil t)

;;; Utilities
;;;--------------------------------------------------

(defface e2wm:face-vcs-na
  '((((class color) (background light))
     :foreground "Chocolate" :height 1.5 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "Chocolate3" :weight bold :height 1.5 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for e2wm:vcs-na title."
  :group 'e2wm)

(defun e2wm:def-plugin-vcs-na-buffer (title)
  (let ((buf (get-buffer-create " *e2wm:vcs-na*")))
    (with-current-buffer buf
      (let (buffer-read-only)
        (buffer-disable-undo buf)
        (erase-buffer)
        (insert (e2wm:rt (substring title 0) 'e2wm:face-vcs-na))
        buf))))


(defun e2wm:def-plugin-vcs-with-window (topdir-func body-func na-buffer-func)
  (let* ((buf (or e2wm:prev-selected-buffer
                  (wlf:get-buffer (e2wm:pst-get-wm)
                                  (e2wm:$pst-main (e2wm:pst-get-instance)))
                  (current-buffer)))
         (file (buffer-file-name buf))
         (dir (or (and file (file-name-directory file))
                  (with-current-buffer buf default-directory)))
         (topdir (and dir (funcall topdir-func dir))))
    (e2wm:with-advice
     (cond
      (topdir
       (with-selected-window (wlf:get-window wm (wlf:window-name winfo))
         (with-current-buffer buf
           (funcall body-func dir topdir))
         (wlf:set-buffer wm (wlf:window-name winfo)
                         (window-buffer (selected-window)))))
      (t
       (wlf:set-buffer wm (wlf:window-name winfo)
                       (funcall na-buffer-func)))))))

(defvar e2wm:c-vcs-select-if-plugin nil
  "If this variable is non-nil, the plugin window is selected
during popping up the plugin buffer.")

(defun e2wm:vcs-select-if-plugin (buf)
  (e2wm:message "#vcs-select-if-plugin")
  (if e2wm:c-vcs-select-if-plugin
      (loop with wm = (e2wm:pst-get-wm)
            for wname in (mapcar 'wlf:window-name (wlf:wset-winfo-list wm))
            if (and (equal buf (wlf:get-buffer wm wname))
                    (e2wm:pst-window-plugin-get wm wname))
            return (progn (wlf:select wm wname)
                          (e2wm:message "#vcs-select-if-plugin wname: %s" wname)
                          t))))

;;; magit / plugins
;;;--------------------------------------------------

(defun e2wm:def-plugin-magit-branches (frame wm winfo)
  (e2wm:def-plugin-vcs-with-window
   'magit-toplevel
   (lambda (dir topdir) (magit-show-refs-head))
   (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

(e2wm:plugin-register 'magit-branches
                      "Magit Branches"
                      'e2wm:def-plugin-magit-branches)

(defun e2wm:def-plugin-magit-logs (frame wm winfo)
  (e2wm:def-plugin-vcs-with-window
   'magit-toplevel
   (lambda (dir topdir)
     (magit-log nil (or (ignore-errors magit-log-section-arguments)
                        '("-n256" "--decorate"))))
   (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

(e2wm:plugin-register 'magit-logs
                      "Magit Logs"
                      'e2wm:def-plugin-magit-logs)

(defun e2wm:def-plugin-magit-status (frame wm winfo)
  (e2wm:def-plugin-vcs-with-window
   'magit-toplevel
   (lambda (dir topdir) (magit-status-internal (file-name-as-directory dir)))
   (lambda () (e2wm:history-get-main-buffer))))

(e2wm:plugin-register 'magit-status
                      "Magit Status"
                      'e2wm:def-plugin-magit-status)

;;; magit / magit perspective
;;;--------------------------------------------------

(defvar e2wm:c-magit-recipe
  '(| (:left-size-ratio 0.3)
      (- (:upper-size-ratio 0.6)
         status branches)
      (| (:left-size-ratio 0.5)
         (- (:upper-size-ratio 0.5)
            logs main)
         (- (:upper-size-ratio 0.5)
            diff sub))))

(defvar e2wm:c-magit-winfo
  '((:name status   :plugin magit-status)
    (:name branches :plugin magit-branches)
    (:name logs     :plugin magit-logs)
    (:name main)
    (:name diff     :buffer nil :default-hide t)
    (:name sub      :buffer nil :default-hide t)))

(defvar e2wm:c-magit-show-main-regexp
   "\\*\\(vc-diff\\)\\*")

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'magit
   :extend 'base
   :title  "Magit"
   :init   'e2wm:dp-magit-init
   :main   'status
   :start  'e2wm:dp-magit-start
   :switch 'e2wm:dp-magit-switch
   :popup  'e2wm:dp-magit-popup
   :leave  'e2wm:dp-magit-leave
   :keymap 'e2wm:dp-magit-minor-mode-map))

(defadvice magit-log-edit-commit (after e2wm:ad-override-magit)
  (e2wm:pst-update-windows))
(ad-deactivate-regexp "^e2wm:ad-override-magit$")

(defun e2wm:dp-magit-leave (wm)
  (ad-deactivate-regexp "^e2wm:ad-override-magit$")
  (setq e2wm:prev-selected-buffer nil))

(defun e2wm:dp-magit-start (wm)
  (ad-activate-regexp "^e2wm:ad-override-magit$"))

(defun e2wm:dp-magit-init ()
  (let* ((magit-wm
          (wlf:no-layout e2wm:c-magit-recipe e2wm:c-magit-winfo))
         (buf (or e2wm:prev-selected-buffer
                  (e2wm:history-get-main-buffer))))
    (wlf:set-buffer magit-wm 'main buf)
    magit-wm))

(defun e2wm:dp-magit-switch (buf)
  (e2wm:message "#DP MAGIT switch : %s" buf)
  (cond ((e2wm:history-recordable-p buf)
         (e2wm:with-advice
          (e2wm:pst-buffer-set 'main buf t t)))
        (t
         (or (e2wm:vcs-select-if-plugin buf)
             (let ((not-minibufp (= 0 (minibuffer-depth))))
               (e2wm:with-advice
                (e2wm:pst-buffer-set 'sub buf t not-minibufp)))))))

(defun e2wm:dp-magit-popup (buf)
  (let ((cb (current-buffer)))
    (e2wm:message "#DP MAGIT popup : %s (current %s / backup %s)"
                  buf cb e2wm:override-window-cfg-backup))
  (unless (e2wm:vcs-select-if-plugin buf)
    (let ((buf-name (buffer-name buf))
          (buf-file-name (ignore-errors
                           (file-name-nondirectory (buffer-file-name buf))))
          (wm (e2wm:pst-get-wm))
          (not-minibufp (= 0 (minibuffer-depth))))
      (e2wm:with-advice
       (cond
        ((equal buf-file-name "COMMIT_EDITMSG")
         ;; displaying commit objects in the main window
         (e2wm:pst-buffer-set 'main buf t t))
        ((equal buf-name "*magit-commit-popup*")
         ;; displaying commit objects in the main window
         (e2wm:pst-buffer-set 'diff buf t t))
        ((string-match "^\\*magit-diff: .*" buf-name)
         ;; displaying diff buffer in the diff window
         (e2wm:pst-buffer-set 'diff buf t nil))
        ((string-match "^\\*magit: .*" buf-name)
         ;; displaying status object in the status window
         (e2wm:pst-buffer-set 'status buf t t))
        ((e2wm:history-recordable-p buf)
         ;; displaying recordable buffer in the main window
         (e2wm:pst-buffer-set 'main buf t t))
        (t
         ;; displaying other objects in the sub window
         (e2wm:pst-buffer-set 'sub buf t not-minibufp)))))))

;; Commands / Keybindings

;;;###autoload
(defun e2wm:dp-magit ()
  (interactive)
  (e2wm:pst-change 'magit))

(defvar e2wm:dp-magit-minor-mode-map
  (e2wm:define-keymap '() e2wm:prefix-key))

(e2wm:add-keymap e2wm:pst-minor-mode-keymap
                 '(("prefix v" . e2wm:dp-magit)) e2wm:prefix-key)


(provide 'my-e2wm-vcs)
;;; my-e2wm-vcs.el ends here
