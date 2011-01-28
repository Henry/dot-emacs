;;; init-cedet.el --- Initialize the semantic code analyser
;; -----------------------------------------------------------------------------

;; (load "~/.emacs.d/packages/cedet/common/cedet")

;; (add-to-list 'Info-directory-list
;;              (expand-file-name "~/.emacs.d/packages/cedet/common") t)
;; (add-to-list 'Info-directory-list
;;              (expand-file-name "~/.emacs.d/packages/cedet/cogre") t)
;; (add-to-list 'Info-directory-list
;;              (expand-file-name "~/.emacs.d/packages/cedet/ede") t)
;; (add-to-list 'Info-directory-list
;;              (expand-file-name "~/.emacs.d/packages/cedet/eieio") t)
;; (add-to-list 'Info-directory-list
;;              (expand-file-name "~/.emacs.d/packages/cedet/speedbar") t)
;; (add-to-list 'Info-directory-list
;;              (expand-file-name "~/.emacs.d/packages/cedet/semantic/doc") t)

(defvar my-cedet-directory
  (expand-file-name "~/Emacs/CEDET"))

(defvar my-semanticdb-directory
  (concat my-cedet-directory "/semantic.cache"))

(setq ede-project-placeholder-cache-file
      (concat my-cedet-directory "/projects.ede"))
(setq ede-simple-save-directory my-cedet-directory)

(make-directory my-semanticdb-directory t)
(setq semanticdb-default-save-directory my-semanticdb-directory)

(setq senator-minor-mode-name "SN")
(setq semantic-imenu-auto-rebuild-directory-indexes nil)

;(require 'semantic-decorate-include)

;; Gcc setup
;(require 'semantic-gcc)

;; Smart completions
;(require 'semantic-ia)

;(require 'eassist)

;; (setq-mode-local c-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))
;; (setq-mode-local c++-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))

;; customisation of modes
(defun my-cedet-hook ()
  ;; (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c." 'senator-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
;;(add-hook 'c-mode-common-hook 'my-cedet-hook)
;;(add-hook 'lisp-mode-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
;;(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;(require 'semanticdb-global)

(setq semantic-ectag-program "ectags")

;;(semantic-add-system-include "~/exp/include" 'c++-mode)
;;(semantic-add-system-include "~/exp/include" 'c-mode)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 5)
 )

;;;  ede customization
;(require 'semantic-lex-spp)
(global-ede-mode t)

;; -----------------------------------------------------------------------------
;;; init-cedet.el ends here
