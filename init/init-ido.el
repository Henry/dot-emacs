;;; init-ido.el --- Initialize ido buffer switcher and file finder
;; -----------------------------------------------------------------------------
(use-package ido
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-case-fold nil
        ido-default-file-method 'selected-window
        ido-ignore-buffers (append ido-ignore-buffers (list "\\*BBDB\\*")))
  :config
  (ido-mode t))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :disabled t
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode 0))

(use-package smex
  :init (smex-initialize)
  :bind (("M-u" . smex)
         ("S-M-u" . smex-major-mode-commands)))

;; -----------------------------------------------------------------------------
;;; init-ido.el ends here
