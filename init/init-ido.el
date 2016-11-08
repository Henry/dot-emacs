;;; init-ido.el --- Initialize ido buffer switcher and file finder
;; -----------------------------------------------------------------------------
(use-package ido
  :ensure t
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-case-fold nil
        ido-default-file-method 'selected-window
        ido-ignore-buffers (append ido-ignore-buffers (list "\\*BBDB\\*")))
  :config
  (ido-mode t))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :disabled t
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode 0))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-u" . smex)
         ("S-M-u" . smex-major-mode-commands)))

;; -----------------------------------------------------------------------------
;;; init-ido.el ends here
