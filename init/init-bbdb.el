;;; init-bbdb.el --- Initialize the Insideous Big Brother Database
;; -----------------------------------------------------------------------------
(use-package bbdb
  :init
  (setq bbdb-file-coding-system 'utf-8
        bbdb-file "~/Emacs/bbdb"
        bbdb-phone-style nil
        bbdb-default-country "UK"
        bbdb-layout 'multi-line
        bbdb-pop-up-window-size 0.15
        ;;bbdb-auto-notes-rules '(("X-ML-Name" (".*$" ML 0)))
        bbdb-add-mails 'query   ;; Query add new addresses

        ;; Make sure we look at every address in a message and not only the
        ;; first one
        bbdb-message-all-addresses t

        bbdb-ignore-message-alist ;; Don't ask about fake addresses
        '(("From" . "github")
          ("To" . "github"))

        bbdb-mail-avoid-redundancy t ;; always use full name
        bbdb-add-name 'query ;; Query add new name

        bbdb-ignore-redundant-mails  t ;; x@foo.bar.cx => x@bar.cx

        bbdb-completion-list t ;; Complete on anything
        bbdb-complete-mail-allow-cycling t ;; Cycle through matches

        ;; What do we do when invoking bbdb interactively
        bbdb-mua-update-interactive-p '(query . create)
        bbdb-mua-pop-up t
        bbdb-mua-pop-up-window-size 0.15
        bbdb-mua-edit-field 'folder
        )
  (setq sc-citation-leader nil
        sc-citation-delimiter ">"
        sc-citation-separator " "

        sc-preferred-attribution-list
        '("sc-lastchoice" "x-attribution" "sc-consult"
          "initials" "firstname" "lastname")

        sc-mail-glom-frame
        '((begin                        (setq sc-mail-headers-start (point)))
          ("^From "                     (sc-mail-check-from) nil nil)
          ("^x-attribution:[ \t]+.*$"   (sc-mail-fetch-field t) nil t)
          ("^\\S +:.*$"                 (sc-mail-fetch-field) nil t)
          ("^$"                         (list 'abort '(step . 0)))
          ("^[ \t]+"                    (sc-mail-append-field))
          (sc-mail-warn-if-non-rfc822-p (sc-mail-error-in-mail-field))
          (end                          (progn
                                          (bbdb-sc-update-from)
                                          (setq sc-mail-headers-end (point))))))
  :config
  (progn
    (require 'bbdb)
    (require 'bbdb-com)
    (require 'bbdb-sc)

    (add-to-list 'sc-attrib-selection-list
                 '("from" ((".*" . (bbdb-sc-get-attrib
                                    (sc-mail-field "from"))))))

    ;;  Initialise with supercite support
    (bbdb-initialize 'sc)))

;; -----------------------------------------------------------------------------
;;; init-bbdb.el ends here
