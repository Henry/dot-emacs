;;; init-openwith.el --- Initialize openwith
;;;  openwith: Open files using specified application
;; -----------------------------------------------------------------------------
(use-package openwith
  :ensure t
  :init
  (setq
   openwith-associations
   '(
     ;;("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)) ;; Now handled by emacs
     ;;("\\.pdf\\'" "acroread" (file)) ;; Now handled by docview
     ("\\.mp3\\'" "mplayer" (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'"
      "mplayer" ("-idx" "-fps" "5" "-loop" "0" file))
     )
   openwith-confirm-invocation t
   )
  :config
  (openwith-mode t))

;; -----------------------------------------------------------------------------
;;; init-openwith.el ends here
