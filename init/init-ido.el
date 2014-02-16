;;; init-ido.el --- Initialize ido buffer switcher and file finder
;; -----------------------------------------------------------------------------

(require 'ido)

(setq ido-enable-flex-matching t
      ido-case-fold nil
      ido-ignore-buffers (append ido-ignore-buffers (list "\\*BBDB\\*")))
(ido-mode t)

;; -----------------------------------------------------------------------------
;;; mcomplete: Enhanced minibuffer incremental completion preview and completion

(require 'mcomplete)
(load "mcomplete-history")

(set-face-foreground
 'mcomplete-prefix-method-fixed-part-face "blue")
(set-face-foreground
 'mcomplete-prefix-method-alternative-part-face "blue")

(set-face-foreground
 'mcomplete-substr-method-fixed-part-face "blue")
(set-face-foreground
 'mcomplete-substr-method-alternative-part-face "blue")

;(add-hook 'ido-setup-hook 'turn-off-mcomplete-mode)
;(add-hook 'ido-make-file-list-hook 'turn-off-mcomplete-mode)
;(add-hook 'ido-make-dir-list-hook 'turn-off-mcomplete-mode)
;(add-hook 'ido-make-buffer-list-hook 'turn-off-mcomplete-mode)
(add-hook 'ido-minibuffer-setup-hook 'turn-off-mcomplete-mode)
(add-hook 'ido-rewrite-file-prompt-functions 'turn-off-mcomplete-mode)
;(add-hook 'ido-before-fallback-functions 'turn-off-mcomplete-mode)
(add-hook 'post-command-hook 'turn-on-mcomplete-mode)


;; -----------------------------------------------------------------------------
;;; Completing-help: press '?' to display info on possible completions

(require 'completing-help)
(turn-on-completing-help-mode)

;; -----------------------------------------------------------------------------
;;; init-ido.el ends here
