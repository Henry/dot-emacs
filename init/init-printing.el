;;; init-printing.el --- Initialize Print settings
;; -----------------------------------------------------------------------------

(setq ps-paper-type 'a4
      ps-landscape-mode    nil
      ps-number-of-columns 1

      ps-left-margin   (/ (* 72  1.0) 2.54) ;  1.0 cm
      ps-right-margin  (/ (* 72  1.0) 2.54) ;  1.0 cm
      ps-inter-column  (/ (* 72  1.0) 2.54) ;  1.0 cm
      ps-bottom-margin (/ (* 72  1.5) 2.54) ;  1.5 cm
      ps-top-margin    (/ (* 72  1.5) 2.54) ;  1.5 cm
      ps-header-offset (/ (* 72  1.0) 2.54) ;  1.0 cm
      ps-header-line-pad    .15
      ps-print-header       t
      ps-print-header-frame t
      ps-header-lines       2
      ps-show-n-of-n        t
      ps-spool-duplex       t

      ps-font-family             'Courier
      ps-font-size               10
      ps-header-font-family      'Helvetica
      ps-header-font-size        10
      ps-header-title-font-size  10
      )

;; -----------------------------------------------------------------------------
;;; init-printing.el ends here
