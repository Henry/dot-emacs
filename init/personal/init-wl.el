;;; init-wl.el --- Configuration file for the Wanderlust Email client
;; -----------------------------------------------------------------------------
;;; Basic configuration

(defun get-ip-address (&optional dev)
  "get the IP-address for device DEV (default: eth0)"
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

(setq support-file-path
      (let ((ip (get-ip-address)))
        (cond
         ((string= ip "10.0.0.37")
          "~/OpenCFDAdmin/Accounts/Terms/CommercialSupport")
         ((string= ip "10.0.0.7")
          "~OpenCFDAdmin/Accounts/Terms/CommercialSupport"))))

(setq wl-organization "OpenCFD"

      ;; Automatic signature insertion
      signature-file-name "~/Maildir/Signatures/OpenCFDAddress"

      ;; User Email addresses
      wl-user-mail-address-list nil
      ;; (list wl-from
      ;;      "enquiries@OpenCFD.co.uk"
      ;;      "hweller0@gmail.com")
      )

;; -----------------------------------------------------------------------------
;;; Folders

(setq
      my-wl-default-filing-folder ".OpenCFD"
      wl-default-spec ".OpenCFD/Customers/"
      )

;; -----------------------------------------------------------------------------
;;; Draft:

(setq wl-draft-config-alist
      '(
        ((string-match "opencfd\\+h\\.weller.*" wl-draft-parent-folder)
         ("From" . "Henry Weller <H.Weller@OpenCFD.co.uk>")
         ("Organization" . "OpenCFD")
         ("X-Attribution" . "HGW")
         (signature . "~/Maildir/Signatures/OpenCFDAddress"))

        ((string-match "opencfd\\+enquiries.*" wl-draft-parent-folder)
         ("From" . "Enquiries <enquiries@opencfd.co.uk>")
         ("Bcc" . "Enquiries <enquiries@opencfd.co.uk>")
         ("X-Attribution" . "OCFD")
         (signature . "~/Maildir/Signatures/OpenCFDEnquiries"))

        ((string-match "hweller0.*@imap\\.gmail\\.com.*" wl-draft-parent-folder)
         ("From" . "Henry Weller <hweller0@gmail.com>")
         ("Organization" . nil)
         ("X-Attribution" . "HGW")
         (signature . "~/Maildir/Signatures/homeAddress"))

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
         (signature . "~/Maildir/Signatures/OpenCFDEnquiries"))
        ("Academic Support"
         (body-file . "~/Maildir/Templates/academicSupport")
         (signature . "~/Maildir/Signatures/OpenCFDEnquiries")
         )))

(defun my-wl-support-gbp ()
  "Insert the standard commercial support contract details."
  (interactive)
  (forward-char (cadr (insert-file-contents
                       "~/Maildir/Templates/commercialSupport")))
  (mime-edit-insert-file
   (concat support-file-path "/OpenFOAMSupportPackagesGBP.pdf")))

(defun my-wl-support-euro ()
  "Insert the standard commercial support contract details."
  (interactive)
  (forward-char (cadr (insert-file-contents
                       "~/Maildir/Templates/commercialSupport")))
  (mime-edit-insert-file
   (concat support-file-path "/OpenFOAMSupportPackagesEUR.pdf")))

(defun my-wl-support-usd ()
  "Insert the standard commercial support contract details."
  (interactive)
  (forward-char (cadr (insert-file-contents
                       "~/Maildir/Templates/commercialSupport")))
  (mime-edit-insert-file
   (concat support-file-path "/OpenFOAMSupportPackagesUSD.pdf")))


;; -----------------------------------------------------------------------------
;;; Biff: Check for new mail

;; (setq wl-biff-check-folder-list
;;       '("&opencfd+h.weller/user@mail.plus.net:110!direct"
;;         "&opencfd+enquiries/user@mail.plus.net:110!direct"
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

(defun my-wl-plusnet-smtp-server ()
  "Configure the use of the PlusNet SMTP server for sending"
  (interactive)
  (setq wl-smtp-connection-type nil
        wl-smtp-authenticate-type nil
        wl-smtp-posting-port 25
        wl-smtp-posting-user "h.weller@opencfd.co.uk"
        wl-smtp-posting-server "relay.plus.net"
        wl-local-domain "plus.net"))

(defun my-wl-opencfd-smtp-server ()
  "Configure the use of the OpenCFD SMTP server for sending"
  (interactive)
  (setq wl-smtp-connection-type nil
        wl-smtp-authenticate-type nil
        wl-smtp-posting-port 25
        wl-smtp-posting-user "h.weller@opencfd.co.uk"
        ;;wl-smtp-posting-server "mr1.voxclever.net"
        wl-smtp-posting-server "smtp.regusnet.com"
        wl-local-domain "opencfd.co.uk"))

(defun my-wl-default-smtp-server ()
  (let ((ip (get-ip-address)))
    (cond
     ((string= ip "10.0.0.37") (my-wl-zen-smtp-server))
     ((string= ip "10.0.0.7") (my-wl-opencfd-smtp-server)))))

;; -----------------------------------------------------------------------------
;;; init-wl.el ends here
