;;; ioccur.el --- Incremental occur.

;; Author: Thierry Volpiatto <thierry dot volpiatto at gmail dot com>

;; Copyright (C) 2010 Thierry Volpiatto, all rights reserved.

;; Compatibility: GNU Emacs 23.1+

;; X-URL: http://mercurial.intuxication.org/hg/ioccur

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Install:
;;
;; Add this file to your `load-path', BYTE-COMPILE it and
;; add (require 'ioccur) in your .emacs.
;;
;; Start with (C-u) M-x ioccur
;;            or
;;            (C-u) M-x ioccur-find-buffer-matching
;;
;; Do C-h f ioccur or ioccur-find-buffer-matching for more info.

;;; Commentary:
;;
;; To save `ioccur-history', use desktop, adding that to your .emacs:
;; (add-to-list 'desktop-globals-to-save 'ioccur-history)
;;
;; For more info See:
;; [EVAL] (info "(emacs) saving emacs sessions")

;;; Code:
(require 'derived)
(eval-when-compile (require 'cl))
(require 'outline)

(defvar ioccur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")        'ioccur-quit)
    (define-key map (kbd "RET")      'ioccur-jump-and-quit)
    (define-key map (kbd "<left>")   'ioccur-jump-and-quit)
    (define-key map (kbd "<right>")  'ioccur-jump-without-quit)
    (define-key map (kbd "C-z")      'ioccur-jump-without-quit)
    (define-key map (kbd "<C-down>") 'ioccur-scroll-down)
    (define-key map (kbd "<C-up>")   'ioccur-scroll-up)
    (define-key map (kbd "C-v")      'ioccur-scroll-other-window-up)
    (define-key map (kbd "M-v")      'ioccur-scroll-other-window-down)
    (define-key map (kbd "<down>")   'ioccur-next-line)
    (define-key map (kbd "<up>")     'ioccur-precedent-line)
    (define-key map (kbd "C-n")      'ioccur-next-line)
    (define-key map (kbd "C-p")      'ioccur-precedent-line)
    (define-key map (kbd "R")        'ioccur-restart)
    (define-key map (kbd "C-|")      'ioccur-split-window)
    map)
  "Keymap used for ioccur commands.")


(defgroup ioccur nil
  "Mode that provide incremental searching in buffer."
  :prefix "ioccur-"
  :group 'text)

;;; User variables.
(defcustom ioccur-search-delay 0.5
  "*During incremental searching, display is updated all these seconds."
  :group 'ioccur
  :type  'integer)

(defcustom ioccur-search-prompt "Pattern: "
  "*Prompt used for `ioccur-occur'."
  :group 'ioccur
  :type  'string)

(defcustom ioccur-mode-line-string
  (if (window-system)
      " RET:Exit, C-g:Quit, C-k:Kill, C-z:Jump, C-j:Jump&quit, \
C-n/p:Next/Prec-line, M-p/n:Hist, C/M-v:Scroll, C-down/up:Follow, C-w:Yank tap"

      " RET:Exit, C-g:Quit, C-k:Kill, C-z:Jump, C-j:Jump&quit, \
C-n/p:Next/Prec-line, Tab/S-tab:Hist, C-v/t:Scroll, C-d/u:Follow, C-w:Yank tap")

  "*Documentation of `ioccur' prompt displayed in mode-line.
Set it to nil to remove doc in mode-line."
  :group 'ioccur
  :type  'string)

(defcustom ioccur-length-line 80
  "*Length of the line displayed in ioccur buffer."
  :group 'ioccur
  :type 'integer)

(defcustom ioccur-max-length-history 100
  "*Maximum number of element stored in `ioccur-history'."
  :group 'ioccur
  :type 'integer)

(defcustom ioccur-buffer-completion-style 'anything
  "*Type of completing read style you prefer to choose buffers \
in `ioccur-find-buffer-matching'.
It can be one of 'anything or 'ido.
When nil or if anything is not found or `ido-mode' is off,
fallback to classic `completing-read'.
NOTE:
To have anything completion you need a recent version of
`anything-config.el'.
To have ido completion, you have to enable `ido-mode'."
  :group 'ioccur
  :type 'symbol)

;;; Faces.
(defface ioccur-overlay-face
    '((t (:background "Green4" :underline t)))
  "Face for highlight line in ioccur buffer."
  :group 'ioccur-faces)

(defface ioccur-match-overlay-face
    '((t (:background "Indianred4" :underline t)))
  "Face for highlight line in matched buffer."
  :group 'ioccur-faces)

(defface ioccur-title-face
    '((t (:background "Dodgerblue4")))
  "Face for highlight incremental buffer title."
  :group 'ioccur-faces)

(defface ioccur-regexp-face
    '((t (:background "DeepSkyBlue" :underline t)))
  "Face for highlight found regexp in incremental buffer."
  :group 'ioccur-faces)

(defface ioccur-num-line-face
    '((t (:foreground "OrangeRed")))
  "Face for highlight number line in ioccur buffer."
  :group 'ioccur-faces)

;;; Internal variables.
(defvar ioccur-search-pattern "")
(defvar ioccur-search-timer nil)
(defvar ioccur-quit-flag nil)
(defvar ioccur-current-buffer nil)
(defvar ioccur-occur-overlay nil)
(make-variable-buffer-local 'ioccur-occur-overlay)
(defvar ioccur-exit-and-quit-p nil)
(defvar ioccur-history nil)
(defvar ioccur-match-overlay nil)
(defvar ioccur-count-occurences 0)
(defvar ioccur-buffer nil)
(make-variable-buffer-local 'ioccur-buffer)
(defvar ioccur-success nil)

(define-derived-mode ioccur-mode
    text-mode "ioccur"
    "Major mode to search occurences of regexp in current buffer.

Special commands:
\\{ioccur-mode-map}"
    (if ioccur-mode-line-string
        (setq mode-line-format
              '(" " mode-line-buffer-identification " "
                (line-number-mode "%l") " "
                ioccur-mode-line-string "-%-"))
        (kill-local-variable 'mode-line-format)))

(defsubst* ioccur-position (item seq &key (test 'eq))
  "A simple replacement of CL `position'."
  (loop for i in seq for index from 0
     when (funcall test i item) return index))

;;; Iterators.
(defmacro ioccur-iter-list (list-obj)
  "Return an iterator from list LIST-OBJ."
  `(lexical-let ((lis ,list-obj))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (cdr lis))
         elm))))

(defun ioccur-iter-next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(defun ioccur-iter-circular (seq)
  "Infinite iteration on SEQ."
  (lexical-let ((it  (ioccur-iter-list seq))
                (lis seq))
    (lambda ()
      (let ((elm (ioccur-iter-next it)))
        (or elm
            (progn (setq it (ioccur-iter-list lis))
                   (ioccur-iter-next it)))))))

(defun ioccur-butlast (seq pos)
  "Return SEQ from index 0 to POS."
  (butlast seq (- (length seq) pos)))

(defun* ioccur-sub-prec-circular (seq elm &key (test 'eq))
  "Infinite reverse iteration of SEQ starting at ELM."
  (lexical-let* ((rev-seq  (reverse seq))
                 (pos      (ioccur-position elm rev-seq :test test))
                 (sub      (append (nthcdr (1+ pos) rev-seq)
                                   (ioccur-butlast rev-seq pos)))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (let ((elm (ioccur-iter-next iterator)))
         (or elm
             (progn (setq iterator (ioccur-iter-list sub))
                    (ioccur-iter-next iterator)))))))

(defun* ioccur-sub-next-circular (seq elm &key (test 'eq))
  "Infinite iteration of SEQ starting at ELM."
  (lexical-let* ((pos      (ioccur-position elm seq :test test))
                 (sub      (append (nthcdr (1+ pos) seq)
                                   (ioccur-butlast seq pos)))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (let ((elm (ioccur-iter-next iterator)))
         (or elm (progn
                   (setq iterator (ioccur-iter-list sub))
                   (ioccur-iter-next iterator)))))))

(defun ioccur-print-results (regexp)
  "Print in `ioccur-buffer' all lines matching REGEXP found in BUFFER.
BUFFER default value is `ioccur-current-buffer'."
  (setq ioccur-count-occurences 0)
  (with-current-buffer ioccur-current-buffer
    (save-excursion
      (goto-char (point-min))
      (loop
         while (not (eobp))
         ;; We need to read also C-g from here
         ;; Because when loop is started `ioccur-read-search-input'
         ;; will read key only when loop is finished
         ;; and we have no chance to exit loop.
         when quit-flag do (setq ioccur-quit-flag t) and return nil
         for count from 0
         when (re-search-forward regexp (point-at-eol) t)
         do (ioccur-print-line
             (buffer-substring (point-at-bol) (point-at-eol)) count)
         do (forward-line 1)))))

(defun ioccur-print-line (line nline)
  "Prepare and insert a matched LINE at line number NLINE in BUFFER.
BUFFER default value is `ioccur-buffer'."
  (with-current-buffer ioccur-buffer
    (let ((lineno     (int-to-string (1+ nline)))
          (trunc-line (ioccur-truncate-line line)))
      (incf ioccur-count-occurences)
      (insert " " (propertize
                   lineno 'face 'ioccur-num-line-face
                   'help-echo line)
              ":" trunc-line "\n"))))

(defun* ioccur-truncate-line (line &optional (columns ioccur-length-line))
  "Remove indentation and truncate LINE to COLUMNS.
COLUMNS default value is `ioccur-length-line'."
  (let* ((bol-reg (if (string-match "^\t" line)
                      "\\(^\t*\\)" "\\(^ *\\)"))
         (ltp     (replace-regexp-in-string bol-reg "" line)))
    (if (> (length ltp) ioccur-length-line)
        (substring ltp 0 ioccur-length-line) ltp)))

(defun ioccur-buffer-contain (buffer regexp)
  "Return BUFFER if it contain an occurence of REGEXP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp nil t) buffer))))

(defun ioccur-list-buffers-matching (buffer-match regexp buffer-list)
  "Collect all buffers in BUFFER-LIST whose names match BUFFER-MATCH and \
contain lines matching REGEXP."
  (loop
     with ini-buf-list = (loop for buf in buffer-list
                            unless (rassq buf dired-buffers)
                            collect buf)
     for buf in ini-buf-list
     for bname = (buffer-name buf)
     when (and (string-match buffer-match bname)
               (ioccur-buffer-contain buf regexp))
     collect bname))

(defun ioccur-list-buffers-containing (regexp buffer-list)
  "Collect all buffers in BUFFER-LIST containing lines matching REGEXP."
  (loop with buf-list = (loop for i in buffer-list
                           when (buffer-file-name (get-buffer i))
                           collect i)
     for buf in buf-list
     when (ioccur-buffer-contain buf regexp)
     collect (buffer-name buf)))

(defun* ioccur-find-buffer-matching1 (regexp
                                      &optional
                                      match-buf-name
                                      (buffer-list (buffer-list)))
  "Find all buffers containing a text matching REGEXP \
and connect `ioccur' to the selected one.

If MATCH-BUF-NAME is non--nil search is performed only in buffers
with name matching specified expression (prompt).

Hitting C-g in a `ioccur' session will return to completion list.
Hitting C-g in the completion list will jump back to initial buffer.

The buffer completion list is provided by one of:
`anything-comp-read', `ido-completing-read', `completing-read'
depending on which `ioccur-buffer-completion-style' you have choosen."
  ;; Remove doublons maybe added by minibuffer in `ioccur-history'.
  (setq ioccur-history
        (loop with hist for i in ioccur-history
           when (not (member i hist)) collect i into hist
           finally return hist))

  (let ((prompt   (format "Search (%s) in Buffer: " regexp))
        (win-conf (current-window-configuration))
        (buf-list (if match-buf-name
                      (ioccur-list-buffers-matching
                       (read-string "In Buffer names matching: ")
                       regexp buffer-list)
                      (ioccur-list-buffers-containing regexp buffer-list))))

    (labels
        ((find-buffer ()
           (let ((buf (cond ((and (fboundp 'anything-comp-read)
                                  (eq ioccur-buffer-completion-style 'anything))
                             (anything-comp-read prompt buf-list :must-match t))
                            ((and ido-mode
                                  (eq ioccur-buffer-completion-style 'ido))
                             (ido-completing-read prompt buf-list nil t))
                            (t (completing-read prompt buf-list nil t)))))
             (unwind-protect
                  (progn
                    (switch-to-buffer buf)
                    (ioccur regexp)
                    ;; Exit if we jump to this `ioccur-current-buffer',
                    ;; otherwise, if C-g is hitten,
                    ;; go back to buffer completion list.
                    (unless ioccur-success
                      (find-buffer)))
               ;; C-g hit in buffer completion restore window config.
               (unless ioccur-success
                 (set-window-configuration win-conf))))))

      (find-buffer))))

;;;###autoload
(defun ioccur-find-buffer-matching (regexp)
  "Find all buffers containing a text matching REGEXP.
See `ioccur-find-buffer-matching1'."
  (interactive (list (let ((savehist-save-minibuffer-history nil))
                       (read-from-minibuffer "Search for Pattern: "
                                             nil nil nil '(ioccur-history . 0)
                                             (thing-at-point 'symbol)))))
  (ioccur-find-buffer-matching1 regexp current-prefix-arg))

;;; Ioccur dired
;;;###autoload
(defun ioccur-dired (regexp)
  (interactive (list (let ((savehist-save-minibuffer-history nil))
                       (read-from-minibuffer "Search for Pattern: "
                                             nil nil nil '(ioccur-history . 0)
                                             (thing-at-point 'symbol)))))
  (let ((buf-list (loop for f in (dired-get-marked-files)
                     do (find-file-noselect f)
                     unless (file-directory-p f)
                     collect (get-buffer (file-name-nondirectory f)))))
    (ioccur-find-buffer-matching1 regexp nil buf-list)))

;;;###autoload
(defun ioccur-restart ()
  "Restart `ioccur' from `ioccur-buffer'.
`ioccur-buffer' is erased and a new search is started."
  (interactive)
  (when (and (eq major-mode 'ioccur-mode) (eq (count-windows) 2))
    (other-window 1) (ioccur)))

;;;###autoload
(defun ioccur-quit ()
  "Quit and kill ioccur buffer."
  (interactive)
  (when ioccur-match-overlay
    (delete-overlay ioccur-match-overlay))
  (quit-window)
  (other-window 1)
  (delete-other-windows))

(defun ioccur-goto-line (numline)
  "Non--interactive version of `goto-line.'.
Goto NUMLINE."
  (goto-char (point-min)) (forward-line (1- numline)))

(defun ioccur-forward-line (n)
  "Forward N lines but empty one's."
  (let (pos)
    (save-excursion
      (forward-line n) (forward-line 0)
      (when (looking-at "^ [0-9]+")
        (forward-line 0) (setq pos (point))))
  (when pos (goto-char pos) (ioccur-color-current-line))))

;;;###autoload
(defun ioccur-next-line ()
  "Goto next line if it is not an empty line."
  (interactive)
  (ioccur-forward-line 1))

;;;###autoload
(defun ioccur-precedent-line ()
  "Goto precedent line if it is not an empty line."
  (interactive)
  (ioccur-forward-line -1))

(defun ioccur-jump ()
  "Jump to line in other buffer and put an overlay on it.
Move point to first occurence of `ioccur-search-pattern'."
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
         (pos  (string-to-number line)))
    (unless (or (string= line "")
                (string= line "Ioccur"))
      (pop-to-buffer ioccur-current-buffer t)
      (show-all) ; For org and outline enabled buffers.
      (ioccur-goto-line pos)
      ;; Go to beginning of first occurence in this line
      ;; of what match `ioccur-search-pattern'.
      (when (re-search-forward ioccur-search-pattern (point-at-eol) t)
        (re-search-backward ioccur-search-pattern (point-at-bol) t))
      (ioccur-color-matched-line))))

;;;###autoload
(defun ioccur-jump-and-quit ()
  "Jump to line in other buffer and quit search buffer."
  (interactive)
  (when (ioccur-jump)
    (delete-other-windows)
    (sit-for 0.3)
    (when ioccur-match-overlay
      (delete-overlay ioccur-match-overlay))))

;;;###autoload
(defun ioccur-jump-without-quit ()
  "Jump to line in `ioccur-current-buffer' without quiting."
  (interactive)
  (when (ioccur-jump) (other-window 1)))

;;;###autoload
(defun ioccur-scroll-other-window-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window 1))

;;;###autoload
(defun ioccur-scroll-other-window-up ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window -1))

(defun ioccur-scroll (n)
  "Scroll other buffer N lines and move overlay accordingly."
  (ioccur-forward-line n)
  (ioccur-color-current-line)
  (when (ioccur-jump)
    (other-window 1)))

;;;###autoload
(defun ioccur-scroll-down ()
  "Scroll other buffer down."
  (interactive)
  (ioccur-scroll 1))

;;;###autoload
(defun ioccur-scroll-up ()
  "Scroll other buffer up."
  (interactive)
  (ioccur-scroll -1))

;;;###autoload
(defun ioccur-split-window ()
  "Toggle split window, vertically or horizontally."
  (interactive)
  (with-current-buffer ioccur-current-buffer
    (let ((old-size (window-height)))
      (delete-window)
      (set-window-buffer
       (select-window (if (= (window-height) old-size)
                          (split-window-vertically)
                          (split-window-horizontally)))
       (get-buffer ioccur-buffer)))))

(defun ioccur-read-char-or-event (prompt)
  "Replace `read-key' when not available using PROMPT."
  (if (fboundp 'read-key)
      (read-key prompt)
      (let* ((chr (condition-case nil (read-char prompt) (error nil)))
             (evt (unless chr (read-event prompt))))
        (or chr evt))))

(defun ioccur-read-search-input (initial-input start-point)
  "Read each keyboard input and add it to `ioccur-search-pattern'.
INITIAL-INPUT is a string given as default input, generally thing at point.
START-POINT is the point where we start searching in buffer."
  (let* ((prompt         (propertize ioccur-search-prompt
                                     'face 'minibuffer-prompt))
         (inhibit-quit   (or (eq system-type 'windows-nt)
                             (not (fboundp 'read-key))))
         (tmp-list       ())
         (it-prec        nil)
         (it-next        nil)
         (cur-hist-elm   (car ioccur-history))
         (start-hist     nil) ; Flag to notify if cycling history started.
         (old-yank-point start-point)
         yank-point)
    (unless (string= initial-input "")
      (loop for char across initial-input do (push char tmp-list)))
    (setq ioccur-search-pattern initial-input)
    ;; Cycle history function.
    ;;
    (flet ((cycle-hist (arg)
             ;; ARG can be positive or negative depending we call M-p or M-n.
             (if ioccur-history
                 (progn
                   ;; Cycle history will start at second call,
                   ;; at first call just use the car of hist ring.
                   ;; We build a new iterator based on a sublist
                   ;; starting at the current element of history.
                   ;; This is a circular iterator. (no end)
                   (if start-hist ; At first call, start-hist is nil.
                       (progn
                         (if (< arg 0)
                             ;; M-p (move from left to right in hist ring).
                             (unless it-prec ; Don't rebuild iterator if exists.
                               (setq it-prec (ioccur-sub-next-circular
                                              ioccur-history
                                              cur-hist-elm :test 'equal))
                               (setq it-next nil)) ; Kill forward iterator.
                             ;; M-n (move from right to left in hist ring).
                             (unless it-next ; Don't rebuild iterator if exists.
                               (setq it-next (ioccur-sub-prec-circular
                                              ioccur-history
                                              cur-hist-elm :test 'equal))
                               (setq it-prec nil))) ; kill backward iterator.
                         (let ((it (or it-prec it-next)))
                           (setq cur-hist-elm (ioccur-iter-next it))
                           (setq tmp-list nil)
                           (loop for char across cur-hist-elm
                              do (push char tmp-list))
                           (setq ioccur-search-pattern cur-hist-elm)))
                       ;; First call use car of history ring.
                       (setq tmp-list nil)
                       (loop for char across cur-hist-elm
                          do (push char tmp-list))
                       (setq ioccur-search-pattern cur-hist-elm)
                       (setq start-hist t)))
                 (message "No history available.") (sit-for 2) t))
           ;; Insert INITIAL-INPUT.
           ;;
           (insert-initial-input ()
             (unless (string= initial-input "")
               (loop for char across initial-input
                  do (push char tmp-list))))
           ;; Maybe start timer.
           ;;
           (start-timer ()
             (unless ioccur-search-timer
               (ioccur-start-timer)))
           ;; Maybe stop timer.
           ;;
           (stop-timer ()
             (when ioccur-search-timer
               (ioccur-cancel-search))))
      ;; Start incremental loop.
      (while (let ((char (ioccur-read-char-or-event
                          (concat prompt ioccur-search-pattern))))
               (message nil)
               (case char
                 ((not (?\M-p ?\M-n ?\t C-tab)) ; Reset history
                  (setq start-hist nil)
                  (setq cur-hist-elm (car ioccur-history)) t)
                 ((down ?\C-n)                  ; Next line.
                  (stop-timer) (ioccur-next-line)
                  (ioccur-color-current-line) t)
                 ((up ?\C-p)                    ; Precedent line.
                  (stop-timer) (ioccur-precedent-line)
                  (ioccur-color-current-line) t)
                 ((?\C-d C-down)                ; Scroll both windows down.
                  (stop-timer) (ioccur-scroll-down) t)
                 ((?\C-u C-up)                  ; Scroll both windows up.
                  (stop-timer) (ioccur-scroll-up) t)
                 (?\r                           ; RET break and exit code.
                  nil)
                 (?\d                           ; Delete backward with DEL.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    (goto-char old-yank-point)
                    (setq yank-point old-yank-point))
                  (pop tmp-list) t)
                 (?\C-g                         ; Quit and restore buffers.
                  (setq ioccur-quit-flag t) nil)
                 ((right ?\C-z)                 ; Persistent action.
                  (ioccur-jump-without-quit) t)
                 ((left ?\C-j)                  ; Jump and kill search buffer.
                  (setq ioccur-exit-and-quit-p t) nil)
                 ((next ?\C-v)                  ; Scroll down.
                  (ioccur-scroll-other-window-down) t)
                 ((?\C-t ?\M-v prior)           ; Scroll up.
                  (ioccur-scroll-other-window-up) t)
                 (?\C-|                         ; Toggle split window.
                  (ioccur-split-window) t)
                 (?\C-k                         ; Kill input.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    (goto-char old-yank-point)
                    (setq yank-point old-yank-point))
                  (kill-new ioccur-search-pattern) (setq tmp-list ()) t)
                 (?\C-y                         ; Yank from `kill-ring'.
                  (setq initial-input (car kill-ring))
                  (insert-initial-input) t)
                 (?\C-w                         ; Yank stuff at point.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    ;; Start to initial point if C-w have never been hit.
                    (unless yank-point (setq yank-point old-yank-point))
                    ;; After a search `ioccur-print-results' have put point
                    ;; to point-max, so reset position.
                    (when yank-point (goto-char yank-point))
                    (forward-word 1)
                    (setq initial-input (buffer-substring-no-properties
                                         yank-point (point)))
                    (setq yank-point (point))   ; End of last forward-word
                    (insert-initial-input) t))
                 ((?\t ?\M-p)                   ; Precedent history elm.
                  (start-timer)
                  (cycle-hist -1))
                 ((backtab ?\M-n)               ; Next history elm.
                  (start-timer)
                  (cycle-hist 1))
                 (t                             ; Store character.
                  (start-timer)
                  (if (characterp char)
                      (push char tmp-list)
                      (setq unread-command-events
                            (nconc (mapcar 'identity
                                           (this-single-command-raw-keys))
                                   unread-command-events))
                      nil))))
        (setq ioccur-search-pattern (apply 'string (reverse tmp-list)))))))

(defun ioccur-print-buffer (regexp)
  "Pretty Print results matching REGEXP in `ioccur-buffer'."
  (let ((title (propertize "Ioccur" 'face 'ioccur-title-face)))
    (if (string= regexp "")
        (progn (erase-buffer) (insert title "\n\n"))
        (erase-buffer)
        (ioccur-print-results regexp)
        (goto-char (point-min))
        (insert title "\n\n"
                (propertize (format "Found %s occurences of "
                                    ioccur-count-occurences)
                            'face 'underline)
                (propertize regexp 'face 'ioccur-regexp-face)
                (propertize
                 (format " in %s" ioccur-current-buffer)
                 'face 'underline) "\n\n")
        (ioccur-color-current-line))))

(defun ioccur-start-timer ()
  "Start ioccur incremental timer."
  (setq ioccur-search-timer
        (run-with-idle-timer
         ioccur-search-delay 'repeat
         #'(lambda ()
             (ioccur-print-buffer
              ioccur-search-pattern)))))
              
;;;###autoload
(defun ioccur (&optional initial-input)
  "Incremental search of lines in current buffer matching input.
With a prefix arg search symbol at point (INITIAL-INPUT).

While you are incremental searching, commands provided are:

C-n or <down>  next line.
C-p or <up>    precedent line.
C-v and M-v    scroll up and down.
C-z or <right> jump without quitting loop.
C-j or <left>  jump and kill `ioccur-buffer'.
RET            exit keeping `ioccur-buffer'.
DEL            remove last character entered.
C-k            Kill current input.
C-w            Yank stuff at point.
C-g            quit and restore buffer.
C-down         Follow in other buffer.
C-up           Follow in other buffer.
M-p/n          Precedent and next `ioccur-history' element:

M-p ,-->A B C D E F G H I---,
    |                       |
    `---I H G F E D C B A<--'

M-n ,-->I H G F E D C B A---,
    |                       |
    `---A B C D E F G H I<--'

Special NOTE for terms:
=======================
  tab/S-tab are bound to history.
  C-d/u are for following in other buffer.
  Use C-t to Scroll up.
 
When you quit incremental search with RET, see `ioccur-mode'
for commands provided in the `ioccur-buffer'."
  (interactive "P")
  (setq ioccur-exit-and-quit-p nil)
  (setq ioccur-success nil)
  (setq ioccur-current-buffer (buffer-name (current-buffer)))
  (message "Fontifying buffer...Please wait it could be long.")
  (jit-lock-fontify-now) (message nil)
  (setq ioccur-buffer (concat "*ioccur-" ioccur-current-buffer "*"))
  (if (and (not initial-input)
           (get-buffer ioccur-buffer)
           (not (get-buffer-window ioccur-buffer)))
      ;; An hidden `ioccur-buffer' exists jump to it.
      (pop-to-buffer ioccur-buffer t)
      ;; `ioccur-buffer' doesn't exists or is not visible, start searching.
      (let* ((init-str (if initial-input
                           (if (stringp initial-input)
                               initial-input
                               (thing-at-point 'symbol))
                           ""))
             (len      (length init-str))
             (curpos   (point))
             str-no-prop)
        (set-text-properties 0 len nil init-str)
        (setq str-no-prop init-str)
        (pop-to-buffer (get-buffer-create ioccur-buffer) t)
        (ioccur-mode)
        (unwind-protect
             ;; Start incremental search.
             (progn
               (ioccur-start-timer)
               (ioccur-read-search-input str-no-prop curpos))
          ;; At this point incremental search loop is exited.
          (progn
            (ioccur-cancel-search)
            (kill-local-variable 'mode-line-format)
            (when (equal (buffer-substring (point-at-bol) (point-at-eol)) "")
              (setq ioccur-quit-flag t))
            (cond (ioccur-quit-flag       ; C-g hit or empty `ioccur-buffer'.
                   (kill-buffer ioccur-buffer)
                   (switch-to-buffer ioccur-current-buffer)
                   (when ioccur-match-overlay
                     (delete-overlay ioccur-match-overlay))
                   (delete-other-windows) (goto-char curpos)
                   (message nil))
                  (ioccur-exit-and-quit-p ; Jump and kill `ioccur-buffer'.
                   (ioccur-jump-and-quit) (kill-buffer ioccur-buffer)
                   (message nil) (ioccur-save-history))
                  (t                      ; Jump keeping `ioccur-buffer'.
                   (ioccur-jump) (other-window 1) (ioccur-save-history)))
            (setq ioccur-count-occurences 0)
            (setq ioccur-quit-flag nil))))))

(defun ioccur-save-history ()
  "Save last ioccur element found in `ioccur-history'."
  ;; Push elm in history if not already there or empty.
  (unless (or (member ioccur-search-pattern ioccur-history)
              (string= ioccur-search-pattern ""))
    (push ioccur-search-pattern ioccur-history))
  ;; If elm already exists in history ring
  ;; push it on top of stack.
  (let ((pos-hist-elm (ioccur-position
                       ioccur-search-pattern
                       ioccur-history :test 'equal)))
    (unless (string= (car ioccur-history)
                     ioccur-search-pattern)
      (push (pop (nthcdr pos-hist-elm ioccur-history))
            ioccur-history)))
  (when (> (length ioccur-history) ioccur-max-length-history)
    (setq ioccur-history (delete (car (last ioccur-history))
                                 ioccur-history)))
  (setq ioccur-success t))

(defun ioccur-cancel-search ()
  "Cancel timer used for ioccur searching."
  (when ioccur-search-timer
    (cancel-timer ioccur-search-timer)
    (setq ioccur-search-timer nil)))

(defun ioccur-color-current-line ()
  "Highlight and underline current line in `ioccur-buffer'."
  (if ioccur-occur-overlay
      (move-overlay ioccur-occur-overlay
                    (point-at-bol) (1+ (point-at-eol)) ioccur-buffer)
      (setq ioccur-occur-overlay
            (make-overlay (point-at-bol) (1+ (point-at-eol)) ioccur-buffer)))
  (overlay-put ioccur-occur-overlay 'face 'ioccur-overlay-face))

(defun ioccur-color-matched-line ()
  "Highlight and underline current position \
of matched line in `ioccur-current-buffer'."
  (if ioccur-match-overlay
      (move-overlay ioccur-match-overlay
                    (point-at-bol) (1+ (point-at-eol)))
      (setq ioccur-match-overlay
            (make-overlay (point-at-bol) (1+ (point-at-eol)))))
  (overlay-put ioccur-match-overlay 'face 'ioccur-match-overlay-face))


(provide 'ioccur)

;;; ioccur.el ends here
