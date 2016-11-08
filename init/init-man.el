;;; init-man.el --- Configure man-page readers
;; -----------------------------------------------------------------------------
;;; man --- Man-page reader
(setq Man-see-also-regexp "SEE ALSO\\|RELATED INFORMATION"
      Man-mode-hook (lambda () (local-set-key [f12] 'man-follow)))

;; -----------------------------------------------------------------------------
;;; iman --- man (and info) lookup with completion
(autoload 'iman "iman"
  "Call the viewers of man pages and GNU Info with completion."
  t nil)

(global-set-key "\C-cm" 'iman) ; `control c', then `m' calls `iman'

;; -----------------------------------------------------------------------------
;;; woman ---  Man-page reader without using man
(setq woman-imenu-title "Sections"
      woman-use-own-frame nil)

;; -----------------------------------------------------------------------------
;;; init-man.el ends here
