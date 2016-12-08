;;; init-wl.el --- Configuration file for the Wanderlust Email client
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Thu Jul 17 09:49:48 2008 (+0100)
;; Version: 0.2
;; Last-updated: Fri Nov 18 08:16:50 2016 (+0000)
;;           By: Henry G. Weller
;;     Update #: 4
;; URL: https://github.com/Henry/dot-emacs/blob/master/init/init-wl.el
;; Keywords: convenience
;; Compatibility: GNU Emacs 25.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; -----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; This is my complete configuration file for Wanderlust, the excellent Email
;; client, which has been cobbled together from the examples and documentation
;; supplied with Wanderlust, reading the source-code, and scanning the mailing
;; lists wl-en@lists.airs.net, wl@lists.airs.net and of course from the web.
;; Mailing list postings by Ron Isaacson, Vitaly Mayatskikh, Yoichi NAKAYAMA
;; have been particularly useful as have configuration files posted on the web
;; by Jared Rhine, Angus Lees and YAMASHITA Junji.
;;
;; -----------------------------------------------------------------------------
;;
;;; Change log:
;;
;; Version 0.1
;; * Initial release
;;
;; Version 0.2
;; * Updated using ELPA packages
;; * Configured using `use-package'
;; * bbdb upgraded to v3
;;
;; -----------------------------------------------------------------------------
;;
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
;;
;; -----------------------------------------------------------------------------
;;
;;; Code:

;; -----------------------------------------------------------------------------
(use-package wanderlust
  :ensure t
  :ensure apel
  :ensure flim
  :ensure semi
  :ensure bbdb
  :commands wl

  :init
  ;; Autoload Wanderlust on command "wl"
  (autoload 'wl "wl" "Wanderlust" t)

  (setq elmo-msgdb-directory
        (concat user-emacs-directory "Wanderlust/Elmo")

        elmo-split-log-file
        (concat user-emacs-directory "Wanderlust/Elmo/split-log")

        mime-situation-examples-file
        (concat user-emacs-directory "Wanderlust/mime-example")

        ;; Offline and synchronization
        wl-plugged t
        elmo-imap4-default-authenticate-type 'clear
        elmo-imap4-default-port '993
        elmo-imap4-default-stream-type 'ssl
        elmo-imap4-use-modified-utf7 t
        elmo-imap4-use-cache t
        elmo-nntp-use-cache t
        elmo-pop3-use-cache t
        wl-ask-range nil

        elmo-message-fetch-confirm t
        elmo-message-fetch-threshold 250000
        ;;elmo-network-session-idle-timeout 30

        wl-fcc ".Sent"
        wl-fcc-force-as-read t
        wl-from (concat user-full-name " <" user-mail-address ">")
        wl-organization "***"

        ;; Automatic signature insertion
        mime-setup-use-signature t
        signature-file-name "***"
        signature-insert-at-eof t
        signature-delete-blank-lines-at-eof t

        ;; User Email addresses
        wl-user-mail-address-list nil

        wl-draft-reply-buffer-style 'keep
        wl-interactive-send nil
        wl-interactive-exit nil

        ;; Windows and decoration
        wl-folder-use-frame nil
        wl-highlight-body-too t
        wl-use-highlight-mouse-line nil
        wl-show-plug-status-on-modeline t
        wl-message-window-size '(1 . 4)

        ;; Create draft in a new frame
        wl-draft-use-frame t

        ;; Do not delete temporary files to allow HTML Emails
        ;; to be viewed in Conkeror
        mime-play-delete-file-immediately nil
        )

  ;; ---------------------------------------------------------------------------
  ;; Folders
  (setq wl-stay-folder-window t
        wl-folder-window-width 30
        wl-folder-desktop-name "Email"
        wl-default-folder "%inbox"
        my-wl-default-filing-folder ".***"
        wl-default-spec ".***/***"
        wl-draft-folder ".Drafts"
        wl-trash-folder ".Trash"
        wl-interactive-save-folders nil

        ;;wl-dispose-folder-alist '(("^." . remove))
        wl-use-petname t
        wl-folder-petname-alist nil
        wl-fldmgr-make-backup  t
        wl-fldmgr-sort-group-first  t

        elmo-folder-update-confirm t
        elmo-folder-update-threshold 1000

        wl-folder-check-async  t
        wl-auto-check-folder-name 'none
        wl-auto-check-folder-list '("^\\.")
        wl-auto-uncheck-folder-list nil

        wl-folder-notify-deleted t
        wl-fldmgr-add-complete-with-current-folder-list t
        wl-folder-info-save t
        wl-folder-many-unsync-threshold  100
        wl-highlight-folder-by-numbers 1
        )

  ;; ---------------------------------------------------------------------------
  ;; Local mail directory path
  (setq my-maildir-path "~/Maildir"
        wl-folders-file (concat my-maildir-path "/folders")
        elmo-localdir-folder-path my-maildir-path
        elmo-maildir-folder-path my-maildir-path
        elmo-search-namazu-default-index-path my-maildir-path
        elmo-archive-folder-path my-maildir-path)

  ;; BUILD the folder tree automatically
  ;; Note: if you change the hierarchy and want to rebuild the tree do
  ;; rm -rf ~/Emacs/Wanderlust/Elmo/folder
  (setq wl-folder-hierarchy-access-folders
        '("^.\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)"
          "^-[^.]*\\(:\\|@\\|$\\)"
          "^@$"
          "^'$"))

  ;; ---------------------------------------------------------------------------
  ;; Summary
  (setq wl-auto-select-next 'unread
        wl-summary-width nil
        wl-summary-weekday-name-lang "en"
        wl-summary-showto-folder-regexp ".Sent.*"
        wl-summary-line-format "%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"
        wl-message-mode-line-format (propertize "%f" 'face 'powerline-active1)
        ;; Summary threads
        wl-thread-insert-opened t
        wl-thread-open-reading-thread t
        )

  ;; ---------------------------------------------------------------------------
  ;; Draft
  (setq ;; wl-draft-config-alist ***
        wl-draft-reply-without-argument-list
        '(("Followup-To" .
           (("Mail-Followup-To" "Mail-Reply-To" "Reply-To") nil
            ("Followup-To")))
          ("Mail-Followup-To" .
           (("Mail-Followup-To") nil nil))
          ("Newsgroups" .
           (("Mail-Reply-To" "Reply-To" "To") ("Newsgroups")))
          ("Mail-Reply-To" .
           (("Mail-Reply-To" "Reply-To") nil nil))
          ("Reply-To" .
           (("Reply-To") nil nil))
          (wl-draft-self-reply-p . (("To") nil))
          ("From" . (("From") nil nil)))
        )

  ;; ---------------------------------------------------------------------------
  ;; Message
  (setq mime-view-mailcap-files
        (list (concat user-emacs-directory "Wanderlust/mailcap"))

        wl-message-mode-line-format (propertize "%f/%n %F" 'face 'powerline-active1)

        wl-message-ignored-field-list '("^.*:")
        wl-message-visible-field-list
        '("^\\(To\\|Cc\\):"
          "^Subject:"
          "^\\(From\\|Reply-To\\):"
          "^Organization:"
          "^X-Attribution:"
          "^\\(Posted\\|Date\\):"
          )
        wl-message-sort-field-list
        '("^From"
          "^Organization:"
          "^X-Attribution:"
          "^Subject"
          "^Date"
          "^To"
          "^Cc"))

  ;; ---------------------------------------------------------------------------
  ;; Sending: don't split large messages
  (setq mime-edit-split-message nil)

  ;; ---------------------------------------------------------------------------
  ;; Biff: Check for new mail
  (setq wl-biff-check-interval 180
        wl-biff-check-delay 10
        wl-biff-use-idle-timer nil)

  :config
  (require 'elmo)
  (require 'wl-summary)
  (require 'wl-folder)

  ;;  Initialise bbdb with wanderlust and supercite support
  (bbdb-initialize 'wl 'sc)

  ;;(bbdb-insinuate-wl)
  (bbdb-mua-auto-update-init 'wl 'message)

  ;; Use wanderlust for default compose-mail
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))

  ;; Set mail-icon to be shown universally in the modeline.
  (setq global-mode-string
        (cons
         '(wl-modeline-biff-status
           wl-modeline-biff-state-on
           wl-modeline-biff-state-off)
         global-mode-string))

  (define-key wl-summary-mode-map "\M-q" 'wl-summary-fill-message)
  (define-key wl-summary-mode-map "\C-b" 'bbdb-mua-display-sender)
  (define-key wl-summary-mode-map "\C-f" 'bbdb-mua-edit-field-sender)

  (define-key wl-folder-mode-map [mouse-2] 'wl-folder-jump-to-current-entity)
  )

;; -----------------------------------------------------------------------------
;;;  Update the modeline using powerline colours

(defun my-wl-mode-line-buffer-identification (&optional id)
  "Hacked version of `wl-mode-line-buffer-identification' to
remove clutter from mode-line and apply `powerline' colours."
  (interactive)
  (let ((priorities '(biff plug title)))
    (let ((items (reverse wl-mode-line-display-priority-list))
          item)
      (while items
        (setq item (car items)
              items (cdr items))
        (unless (memq item '(biff plug))
          (setq item 'title))
        (setq priorities (cons item (delq item priorities)))))
    (let (priority result)
      (while priorities
        (setq priority (car priorities)
              priorities (cdr priorities))
        (cond
         ((eq 'biff priority)
          (when wl-biff-check-folder-list
            (setq result (append result '((wl-modeline-biff-status
                                           wl-modeline-biff-state-on
                                           wl-modeline-biff-state-off))))))
         ((eq 'plug priority)
          (when wl-show-plug-status-on-modeline
            (setq result (append result '((wl-modeline-plug-status
                                           wl-modeline-plug-state-on
                                           wl-modeline-plug-state-off))))))
         (t
          result
          ;;Remove clutter
          ;;(setq result (append result (or id '("Wanderlust: %12b"))))
          )))
      (prog1
          (setq mode-line-buffer-identification (if (stringp (car result))
                                                    result
                                                  (cons "" result)))
        ;; Add the powerline background colour
        (setq mode-line-buffer-identification
              `(:propertize (,mode-line-buffer-identification)
                            face powerline-active1))

        (force-mode-line-update t)))))

;; -----------------------------------------------------------------------------
;;;  Re-fill messages that arrive poorly formatted

(require 'filladapt)
(defun wl-summary-fill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (save-excursion
          (set-buffer wl-message-buffer)
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let* ((buffer-read-only nil)
                 (find (lambda (regexp)
                         (save-excursion
                           (if (re-search-forward regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
                 (start (point))
                 (end (if all
                          (point-max)
                        (min (funcall find "^[^>\n]* wrote:[ \n]+")
                             (funcall find "^>>>>>")
                             (funcall find "^ *>.*\n *>")
                             (funcall find "^-----Original Message-----")))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max)))))
        (message "Message re-filled"))
    (message "No message to re-fill")))

;; -----------------------------------------------------------------------------
;;;  Refiling:
;; See also `bbdb-wl-refile-alist' and `wl-init-hook'

(defcustom wl-general-refile-rule-alist nil
  "General rule alist which may be extended to include the `From' `folder'
entries defined in the BBDB by `bbdb-wl-refile-alist'.
e.g.
'((\"From\"
   (\"teranisi@isl.ntt.co.jp\" . \"+teranisi\"))
  (\"x-ml-name\"
   (\"^Wanderlust\"    . \"+wl\")
   (\"^Elips\" . \"+elips\")))"
  :type '(repeat (list (string :tag "Field")
                       (repeat :inline t
                               (cons (regexp :tag "Value")
                                     (string :tag "Folder")))))
  :group 'wl-pref)

;; Set the default value of wl-refile-rule-alist
(setq wl-refile-rule-alist wl-general-refile-rule-alist)

;; -----------------------------------------------------------------------------
;;;  Add hooks

(add-hook
 'wl-init-hook
 '(lambda ()
    (set-frame-position (selected-frame) 663 0)
    (set-frame-height (selected-frame) 70)
    (set-frame-width (selected-frame) 114)
    (my-wl-default-smtp-server)
    (my-bbdb-wl-refile-alist) ;; Add the BBDB refiling folders
    (run-with-idle-timer 30 t 'my-wl-auto-save-draft-buffers)

    ;; Add support for (signature . "filename")
    (unless (assq 'signature wl-draft-config-sub-func-alist)
      (wl-append wl-draft-config-sub-func-alist
                 '((signature . wl-draft-config-sub-signature))))

    (defun mime-edit-insert-signature (&optional arg)
      "Redefine to insert a signature file directly, not as a tag."
      (interactive "P")
      (insert-signature arg))

    ;; Keep track of recently used Email addresses
    ;;(recent-addresses-mode 1)

    (local-set-key "\M-t" my-nav-map)
    ))

(add-hook
 'wl-folder-mode-hook
 '(lambda ()
    (hl-line-mode t)
    (local-set-key "\M-m" 'mairix-search)
    (local-set-key "\M-t" my-nav-map)

    (my-wl-mode-line-buffer-identification)
    ))

(add-hook
 'wl-summary-mode-hook
 '(lambda ()
    (hl-line-mode t)

    ;; Key bindings
    (local-set-key "D" 'wl-thread-delete)
    (local-set-key "b" 'wl-summary-resend-bounced-mail)
    (local-set-key "\C-d" 'my-wl-summary-delete-and-move-prev)
    (local-set-key "\C-cQ" 'my-wl-delete-whole-folder)
    (local-set-key "\C-cb" 'my-bbdb-wl-refile-alist)
    (local-set-key "\C-a"
                   '(lambda ()
                      (interactive)
                      (wl-summary-reply-with-citation 1)))
    (local-set-key "\M-m" 'mairix-search)
    (local-set-key "\M-t" my-nav-map)

    (my-wl-mode-line-buffer-identification)
    ))

(add-hook
 'wl-summary-exec-hook
 '(lambda ()
    ;; Synchronise the folder with the server after executing the summary
    ;; operation
    (wl-summary-sync-update)
    (local-set-key "\M-t" my-nav-map)
    ))

(add-hook
 'wl-message-buffer-created-hook
 '(lambda ()
    (setq truncate-lines nil) ;; Fold over-lenght lines
    ))

(add-hook
 'wl-draft-mode-hook
 '(lambda ()
    ;; Key bindings
    (local-set-key "\C-c\C-k" 'my-wl-draft-kill-force)
    (local-set-key "\M-t" my-nav-map)
    (my-wl-mode-line-buffer-identification)
    ))

;; Add lots of goodies to the mail setup
(add-hook 'wl-mail-setup-hook 'my-mail-setup)

(add-hook
 'mime-view-mode-hook
 '(lambda ()
    "Change [mouse-2] to drag-scroll rather than follow link.
Set [(return)] to execute the mime-button.
Set the `f' key to run `find-file' on the attached entity.
Set the `C-f' key to run `find-file-at-point'.
Set the `w' key to run `wget'.
Set the `j' key to run `mime-preview-quit'."
    ;; Key bindings
    (local-set-key [down-mouse-2] 'mouse-drag-drag)
    (local-set-key [(return)] 'my-mime-button-exec)
    (local-set-key [?f] 'my-mime-find-file-current-entity)
    (local-set-key [(control ?f)] 'find-file-at-point)
    (local-set-key [?w] 'wget)
    (local-set-key [?o] 'wget-open)
    (local-set-key [?j] 'mime-preview-quit)
    (local-set-key [?s] '(lambda ()
                           (interactive)
                           (mime-preview-quit)
                           (wl-summary-sync)))
    (local-set-key "\M-t" my-nav-map)
    ))

(add-hook
 'wl-biff-notify-hook
 '(lambda ()
    (my-wl-update-current-summaries)
    ))

;; Automatically add mailing list fields
(add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)

;; Smilies
(add-hook
 'wl-message-redisplay-hook
 '(lambda () (smiley-region (point-min) (point-max))
    ))

(add-hook
 'wl-message-redisplay-hook 'bbdb-mua-auto-update)

(add-hook
 'wl-draft-cited-hook
 '(lambda ()
     (and (featurep 'smiley-mule)
          (smiley-toggle-buffer -1))
     ))

;; -----------------------------------------------------------------------------
;;;  Configure supercite to manage citations

(use-package supercite
  :ensure t
  :init
  (setq sc-nested-citation-p t
        sc-citation-leader ""
        sc-auto-fill-region-p nil
        sc-confirm-always-p nil
        sc-preferred-header-style 8)
  :config
  (defun my-sc-header ()
    "Insert `Dear <sc-select-attribution>,' at the beginning of replies."
    (let ((sc-mumble "")
          (whofrom (sc-whofrom)))
      (if whofrom
          (insert "Dear " (sc-select-attribution) ","))))

  (add-to-list 'sc-rewrite-header-list '(my-sc-header) t)

  ;; Add supercite support
  (add-hook 'mail-citation-hook 'sc-cite-original))

;; -----------------------------------------------------------------------------
;;;  Extension Functions

(defun wl-draft-config-sub-signature (content)
  "Insert the signature at the end of the MIME message."
  (let ((signature-insert-at-eof nil)
        (signature-file-name content))
    (goto-char (mime-edit-content-end))
    (insert-signature)))

(defun my-bbdb-wl-refile-alist ()
  "Add the `From To' refile to `folder' entries from the BBDB to the
`wl-refile-rule-alist'."
  (interactive)
  (let ((from-rule-alist (list '("From" "To")))
        (records (bbdb-records))
        (record))
    (while records
      (setq record (car records))
      (let ((email-addrs (bbdb-record-mail record))
            (folder (bbdb-record-field record 'folder))
            (email-addr))
        (if folder
            (progn
              (while email-addrs
                (setq email-addr (car email-addrs))
                (setq from-rule-alist
                      (append from-rule-alist (list (cons email-addr folder))))
                (setq email-addrs (cdr email-addrs))))))
      (setq records (cdr records)))
    (setq wl-refile-rule-alist
          (append wl-general-refile-rule-alist (list from-rule-alist)))
    ))

;; -----------------------------------------------------------------------------
;;;  User Functions

(defun my-wl-draft-kill-force ()
  (interactive)
  (wl-draft-kill t))

(defun my-wl-delete-whole-folder ()
  (interactive)
  (wl-summary-target-mark-all)
  (wl-summary-target-mark-delete)
  (wl-summary-exec)
  (wl-summary-exit))

(defun my-wl-check-mail-primary ()
  (interactive)
  (unless (get-buffer wl-folder-buffer-name)
    (wl))
  (delete-other-windows)
  (switch-to-buffer wl-folder-buffer-name)
  (goto-char (point-min))
  (next-line 1)
  (wl-folder-jump-to-current-entity))

(defun my-wl-auto-save-draft-buffers ()
  (let ((buffers (wl-collect-draft)))
    (save-excursion
      (while buffers
        (set-buffer (car buffers))
        (if (buffer-modified-p) (wl-draft-save))
        (setq buffers (cdr buffers))))))

(defun my-wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

(defun my-wl-summary-delete-and-move-prev ()
  (interactive)
  (let (wl-summary-move-direction-downward)
    (call-interactively 'wl-summary-delete)))

(defun my-wl-summary-goto-to-folder (folder)
  "Goto FOLDER from the summary buffer after closing it."
  (wl-summary-exit t)
  (set-buffer (get-buffer wl-folder-buffer-name))
  (wl-folder-goto-folder-subr folder))

(defun my-wl-goto-to-folder (folder)
  "Goto FOLDER from either the folders or summary buffer after closing it."
  (if (string= (buffer-name) wl-summary-buffer-name)
      (my-wl-summary-goto-to-folder folder)
    (wl-folder-goto-folder-subr folder)))

(defun my-clean-mime-reply ()
  "Clean-up the citation in replies, removing unnecessary entities."
  (interactive)
  ;; Find and strip the first tag, indicating the start of the
  ;; cited message
  (when (re-search-forward "^> \\[1" nil t)
    (beginning-of-line)
    (kill-line 1)
    (while (or (looking-at "^> *$")
               (looking-at "^> \\[[1-9]"))
      (kill-line 1))
    (when (re-search-forward "^> \\[[1-9][\\. ]" nil t)
      (beginning-of-line)
      (let ((pt (point)))
        (re-search-forward "^$")
        (delete-region pt (point)))))
  ;; Now find the tag that ends the first section, and strip off
  ;; everything from there to the end of the message (including any
  ;; other sections that got cited)
  (goto-char (point-max))
  (when (re-search-backward "^> +[^ ]" nil t)
    (beginning-of-line)
    (let ((pt (point)))
      (goto-char (point-max))
      (if (re-search-backward "^> *$" pt t)
          (progn
            (beginning-of-line)
            (while (looking-at "^> *$")
              (kill-line 1)
              (forward-line -1))
            (forward-line 1)
            (kill-line 1))
        (goto-char (point-max))
        (re-search-backward "^$")
        (kill-line 1)))))

(defun wl-rehilight ()
  "Re-highlight message."
  (let ((beg (point-min))
        (end (point-max)))
    (put-text-property beg end 'face nil)
    (wl-highlight-message beg end t)))

(defun my-mail-setup ()
  "Set up appropriate modes for writing Email
and clean-up citation for replies."
  (interactive)
   ;; Fold over-lenght lines
  (setq truncate-lines nil)
  (turn-on-auto-fill)
  (flyspell-mode t)
  (wl-draft-config-exec)
  (my-wl-mode-line-buffer-identification)
  ;; Clean up reply citation
  (save-excursion
    ;; Goto the beginning of the message body
    (mail-text)
    ;; If the message body starts with "Dear " assume it is a reply
    ;; and clean the citation
    (when (looking-at "^Dear ")
      (my-clean-mime-reply))))

(defun my-mime-save-content-find-file (entity &optional situation)
  "Save the attached mime ENTITY and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (let* ((name (or (mime-entity-safe-filename entity)
                   (format "%s" (mime-entity-media-type entity))))
         (dir (if (eq t mime-save-directory)
                  default-directory
                mime-save-directory))
         (filename (expand-file-name
                    (file-name-nondirectory name) temporary-file-directory)))
    (mime-write-entity-content entity filename)
    (select-frame (make-frame))
    (find-file filename)
    ))

(defun my-mime-view-emacs-mode (entity &optional situation)
  "Internal method for mime-view to display the mime ENTITY in a buffer with an
appropriate emacs mode."
  (let ((buf (get-buffer-create
              (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mime-insert-text-content entity)
      ;;(mule-caesar-region (point-min) (point-max))
      ;; Set emacs mode here
      (set-buffer-modified-p nil)
      )
    (let ((win (get-buffer-window (current-buffer))))
      (or (eq (selected-window) win)
          (select-window (or win (get-largest-window)))
          ))
    (view-buffer buf)
    (goto-char (point-min))
    ))

(defun my-mime-find-file-current-entity ()
  "Save the current mime entity and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (interactive)
  (let ((entity (get-text-property (point) 'mime-view-entity)))
    (if entity
        (my-mime-save-content-find-file entity)))
  )

(defun my-bbdb-insert-folder ()
  "Interactively select the destination folder and store in BBDB."
  (interactive)
  (let* ((record (or (bbdb-current-record)
                     (error "current record unexists!")))
         (name (wl-summary-read-folder my-wl-default-filing-folder))
         (folder-path (wl-folder-get-elmo-folder name)))
    (bbdb-insert-field
     record 'folder (elmo-folder-name-internal folder-path)))
  ;; Update the wl refiling database
  (my-bbdb-wl-refile-alist))

(defun my-mime-button-exec ()
  "Execute the button under point without using the mouse."
  (interactive)
  (let (buf point func data)
    (save-window-excursion
      (setq buf (current-buffer)
            point (point)
            func (get-text-property (point) 'mime-button-callback)
            data (get-text-property (point) 'mime-button-data)
            ))
    (save-excursion
      (set-buffer buf)
      (goto-char point)
      (if func
          (apply func data))
      )))

;; -----------------------------------------------------------------------------
;;;  Key-bindings

(global-set-key "\C-xm" 'my-wl-check-mail-primary)
(define-key bbdb-mode-map "\C-f" 'my-bbdb-insert-folder)

;; -----------------------------------------------------------------------------
;;;  General mairix interface

(use-package mairix
  :ensure t
  :init
  (setq mairix-file-path "~/Maildir/"
        mairix-search-file "Search"
        mairix-mail-program 'wl
        mairix-wl-search-folder-prefix ".")
  :config
  (add-to-list 'mairix-display-functions '(wl mairix-wl-display))
  (add-to-list 'mairix-get-mail-header-functions '(wl mairix-wl-fetch-field)))

(defun mairix-wl-display (folder)
  "Display FOLDER using Wanderlust."
  ;; If Wanderlust is running (Folder buffer exists)...
  (if (get-buffer wl-folder-buffer-name)
      ;; Check if we are in the summary buffer, close it and
      ;; goto the Folder buffer
      (if (string= (buffer-name) wl-summary-buffer-name)
          (progn
            (wl-summary-exit t)
            (set-buffer (get-buffer wl-folder-buffer-name))))
    ;; Otherwise Wanderlust is not running so start it
    (wl))
  ;; From the Folder buffer goto FOLDER first stripping off mairix-file-path
  ;; to leave the wl folder name
  (when (string-match
         (concat (regexp-quote (expand-file-name mairix-file-path)) "\\(.*\\)")
         folder)
    (wl-folder-goto-folder-subr
     (concat mairix-wl-search-folder-prefix (match-string 1 folder)))))

(defun mairix-wl-fetch-field (field)
  "Get mail header FIELD for current message using Wanderlust."
  (when wl-summary-buffer-elmo-folder
    (elmo-message-field
     wl-summary-buffer-elmo-folder
     (wl-summary-message-number)
     (intern (downcase field)))))

(defvar my-mairix-map
  (let ((map (make-sparse-keymap)))
    (define-key my-map "m" map)
    map)
  "Sub-keymap in my keymap for the mairix commands")

(define-key my-mairix-map "m" 'mairix-search)
(define-key my-mairix-map "w" 'mairix-widget-search)
(define-key my-mairix-map "u" 'mairix-update-database)
(define-key my-mairix-map "f" 'mairix-search-from-this-article)
(define-key my-mairix-map "t" 'mairix-search-thread-this-article)
(define-key my-mairix-map "b" 'mairix-widget-search-based-on-article)
(define-key my-mairix-map "s" 'mairix-save-search)
(define-key my-mairix-map "i" 'mairix-use-saved-search)
(define-key my-mairix-map "e" 'mairix-edit-saved-searches)

(define-key my-map "n" 'wl-biff-check-folders)

;; -----------------------------------------------------------------------------
;;;  Simple mairix interface

(setq my-mairix "mairix")
(setq my-mairix-search-folder ".Search")

(defun my-mairix-search (args)
  "Mairix search"
  (interactive "sSearch: ")
  (let ((buf (get-buffer-create "*mairix*"))
        (msg))
    (set-buffer buf)
    (erase-buffer)
    (apply 'call-process my-mairix nil buf nil (split-string args))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (setq msg (buffer-substring (point-min) (point))))
    (kill-buffer buf)
    (if (string-match "Matched \\([0-9]+\\) messages" msg)
        (let ((cnt (match-string 1 msg)))
          (if (not (string= cnt "0"))
              (my-wl-goto-to-folder my-mairix-search-folder))
          (message msg))
      (error msg))))

;; -----------------------------------------------------------------------------
;;;  Load personal information

(load "personal/init-wl")

;; -----------------------------------------------------------------------------
;;; init-wl.el ends here
