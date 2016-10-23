;;; init-bbdb.el --- Initialize the Insideous Big Brother Database
;; -----------------------------------------------------------------------------
(use-package bbdb
  :init
  (setq bbdb-file-coding-system 'utf-8
        bbdb-file "~/Emacs/bbdb"
        bbdb-phone-style nil
        bbdb-default-country "UK"
        bbdb-print-require t
        bbdb-layout 'multi-line
        bbdb-pop-up-window-size 1
        bbdb-mua-pop-up t
        bbdb-mua-pop-up-window-size 1
        bbdb-auto-notes-rules '(("X-ML-Name" (".*$" ML 0)))
        bbdb-mail-avoid-redundancy nil
        bbdb-add-mails 'query   ;; Query add new addresses

        ;; What do we do when invoking bbdb interactively
        bbdb-mua-update-interactive-p '(query . create)

        ;; Make sure we look at every address in a message and not only the
        ;; first one
        bbdb-message-all-addresses t

        bbdb-ignore-message-alist ;; Don't ask about fake addresses
        '(("From" . "github")
          ("Reply-To" . "nim-lang/Nim")
          ("To" . "nim-lang/Nim")
          ("To" . "github"))

        bbdb-mail-avoid-redundancy t ;; always use full name
        bbdb-add-name 2 ;; show name-mismatches 2 secs

        bbdb-canonicalize-redundant-mails  t ;; x@foo.bar.cx => x@bar.cx

        bbdb-completion-list t ;; Complete on anything
        bbdb-complete-mail-allow-cycling t ;; Cycle through matches
        )
  (setq sc-citation-leader nil
        sc-citation-delimiter ">"
        sc-citation-separator " "

        sc-preferred-attribution-list
        '("sc-lastchoice" "x-attribution" "sc-consult" "initials"
          "firstname" "lastname")

        sc-attrib-selection-list
        '(("sc-from-address"
           ((".*" . (bbdb-sc-get-attrib
                     (sc-mail-field "sc-from-address"))))))

        sc-mail-glom-frame
        '((begin (setq sc-mail-headers-start (point)))
          ("^x-attribution:[ \t]+.*$" (sc-mail-fetch-field t) nil t)
          ("^\\S +:.*$" (sc-mail-fetch-field) nil t)
          ("^$" (list (quote abort) (quote (step . 0))))
          ("^$"  (progn (bbdb/sc-default) (list 'abort '(step . 0))))
          ("^[ \t]+" (sc-mail-append-field))
          (sc-mail-warn-if-non-rfc822-p (sc-mail-error-in-mail-field))
          (end (setq sc-mail-headers-end (point))))
        )
  :config
  (progn
    (require 'bbdb)
    (require 'bbdb-com)
    (require 'bbdb-sc)

    ;;  Initialise with supercite support
    (bbdb-initialize 'sc)
    (bbdb-insinuate-sc)
    ))

;; -----------------------------------------------------------------------------
;;; init-bbdb.el ends here
