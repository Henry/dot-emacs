;;; init-bbdb.el --- Initialize the Insideous Big Brother Database
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/bbdb/lisp"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/bbdb/texinfo") t)

(setq bbdb-file-coding-system 'utf-8)
(require 'bbdb-autoloads)
(require 'bbdb)
(require 'bbdb-com)

;;;  Initialise with supercite support
(bbdb-initialize 'supercite)

(setq bbdb-file "~/Emacs/bbdb"
      bbdb-north-american-phone-numbers-p nil
      bbdb-default-country "UK"
      bbdb-print-require t
      bbdb-print-net 'all
      bbdb-offer-save 1    ; Always save
      bbdb-display-layout 'multi-line
      bbdb-pop-up-target-lines 1
      )

;; -----------------------------------------------------------------------------
;;;  SuperCite setup

(bbdb-insinuate-sc)

(setq sc-citation-leader nil
      sc-citation-delimiter ">"
      sc-citation-separator " "

      sc-preferred-attribution-list
      '("sc-lastchoice" "x-attribution" "sc-consult" "initials"
        "firstname" "lastname")

      sc-attrib-selection-list
      '(("sc-from-address"
         ((".*" . (bbdb/sc-consult-attr
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

;; -----------------------------------------------------------------------------
;;; init-bbdb.el ends here
