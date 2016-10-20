;;; init-wl.el --- Configuration file for the Wanderlust Email client
;; -----------------------------------------------------------------------------
;;; Basic configuration

;; (defun get-ip-address (&optional dev)
;;   "get the IP-address for device DEV (default: enp5s0)"
;;   (let ((dev (if dev dev "enp5s0")))
;;     (format-network-address (car (network-interface-info dev)) t)))
(defun get-ip-address (&optional dev)
  "get the IP-address for device DEV (default: br0)"
  (let ((dev (if dev dev "br0")))
    (format-network-address (car (network-interface-info dev)) t)))

(setq support-file-path
      (let ((ip (get-ip-address)))
        (cond
         ((string= ip "10.0.0.37")
          "~CFDadmin/Accounts/CFDAdmin/SupportPackages"))))

(setq wl-organization "CFD Direct"

      ;; Automatic signature insertion
      signature-file-name "~/Maildir/Signatures/CFDdirect"

      ;; User Email addresses
      wl-user-mail-address-list nil
      ;; (list wl-from
      ;;      "enquiries@OpenFoam.org"
      ;;      "hweller0@gmail.com")
      )

;; -----------------------------------------------------------------------------
;;; Folders

(setq
      my-wl-default-filing-folder ".CFDdirect"
      wl-default-spec ".CFDdirect/Customers/"
      )

;; -----------------------------------------------------------------------------
;;; Draft:

(setq wl-draft-config-alist
      '(
        ((string-match "hweller0.*@imap\\.gmail\\.com.*" wl-draft-parent-folder)
         ("From" . "Henry Weller <hweller0@gmail.com>")
         ("Organization" . nil)
         ("X-Attribution" . "HGW")
         (signature . "~/Maildir/Signatures/homeAddress"))

        ((string-match "h\\.weller\\+cfd.*" wl-draft-parent-folder)
         ("From" . "Henry Weller <h.weller@cfd.direct>")
         ("Organization" . nil)
         ("X-Attribution" . "CFD")
         (signature . "~/Maildir/Signatures/CFDdirect"))

        ((string-match "enquiries\\+cfd.*" wl-draft-parent-folder)
         ("From" . "Enquiries <enquiries@cfd.direct>")
         ("Bcc" . "Enquiries <enquiries@cfd.direct>")
         ("Organization" . nil)
         ("X-Attribution" . "CFD")
         (signature . "~/Maildir/Signatures/CFDdirectEnquiries"))

        ((string-match "h\\.weller\\+openfoam.*" wl-draft-parent-folder)
         ("From" . "Henry Weller <h.weller@openfoam.org>")
         ("Organization" . nil)
         ("X-Attribution" . "OFF")
         (signature . "~/Maildir/Signatures/OpenFOAMFoundation"))

        ((string-match "enquiries\\+openfoam.*" wl-draft-parent-folder)
         ("From" . "Enquiries <enquiries@openfoam.org>")
         ("Bcc" . "Enquiries <enquiries@openfoam.org>")
         ("Organization" . "OpenFOAM Foundation")
         ("X-Attribution" . "OFF")
         (signature . "~/Maildir/Signatures/OpenFOAMFoundationEnquiries"))

        ((string-match "openfoam\\.foundation.*@imap\\.gmail\\.com.*"
                       wl-draft-parent-folder)
         ("From" . "The OpenFOAM Foundation <openfoam.foundation@gmail.com>")
         ("Organization" . nil)
         ("X-Attribution" . "OFF")
         (signature . "~/Maildir/Signatures/OpenFOAMFoundation"))
        )
      )

;;;  Templates
(setq wl-template-alist
      '(("Commercial Support"
         (body-file . "~/Maildir/Templates/commercialSupport")
         (signature . "~/Maildir/Signatures/OpenFOAMEnquiries"))
        ("Academic Support"
         (body-file . "~/Maildir/Templates/academicSupport")
         (signature . "~/Maildir/Signatures/OpenFOAMEnquiries")
         )))

(defun my-wl-support-gbp ()
  "Insert the standard commercial support contract details."
  (interactive)
  (forward-char (cadr (insert-file-contents
                       "~/Maildir/Templates/commercialSupport")))
  (mime-edit-insert-file
   (concat support-file-path "/CFDDirect-SupportPackage-GBP.pdf")))

(defun my-wl-support-euro ()
  "Insert the standard commercial support contract details."
  (interactive)
  (forward-char (cadr (insert-file-contents
                       "~/Maildir/Templates/commercialSupport")))
  (mime-edit-insert-file
   (concat support-file-path "/CFDDirect-SupportPackage-EUR.pdf")))

(defun my-wl-support-usd ()
  "Insert the standard commercial support contract details."
  (interactive)
  (forward-char (cadr (insert-file-contents
                       "~/Maildir/Templates/commercialSupport")))
  (mime-edit-insert-file
   (concat support-file-path "/CFDDirect-SupportPackage-USD.pdf")))


;; -----------------------------------------------------------------------------
;;; Biff: Check for new mail

;; (setq wl-biff-check-folder-list
;;         ;;"%inbox:hweller0/clear@imap.gmail.com:993!"
;;         ;;"%inbox:openfoam.foundation/clear@imap.gmail.com:993!"
;;         ;;"-gmane.emacs.sources@news.gmane.org"
;;         ;;"-gmane.mail.wanderlust.general@news.gmane.org"
;;         ;;"-gmane.mail.wanderlust.general.japanese@news.gmane.org"
;;         )
;;       )

;; -----------------------------------------------------------------------------
;;; Sending

(defun my-wl-gmail-smtp-server ()
  "Configure the use of the GMail SMTP server for sending"
  (setq wl-smtp-connection-type 'starttls
        wl-smtp-posting-port 587
        wl-smtp-authenticate-type "plain"
        wl-smtp-posting-user "hweller0@gmail.com"
        wl-smtp-posting-server "smtp.gmail.com"
        wl-local-domain "gmail.com"))

(defun my-wl-zen-smtp-server ()
  "Configure the use of the Zen SMTP server for sending"
  (interactive)
  (setq wl-smtp-connection-type nil
        wl-smtp-authenticate-type nil
        wl-smtp-posting-port 25
        wl-smtp-posting-user "zen128620"
        wl-smtp-posting-server "mailhost.zen.co.uk"
        wl-local-domain "zen.co.uk"))

(defun my-wl-default-smtp-server ()
  (let ((ip (get-ip-address)))
    (cond
     ((string= ip "10.0.0.37")
      (my-wl-zen-smtp-server)))))

;; -----------------------------------------------------------------------------
;;; init-wl.el ends here
