;;; init-packge.el --- Initialize package
;; -----------------------------------------------------------------------------
;;; Package initialization
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;;; To recompile packages
;; (byte-recompile-directory (expand-file-name "~/Emacs/elpa") 0)

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(setq load-prefer-newer t)

;;; General packages
(use-package dash
  :ensure t)

;; -----------------------------------------------------------------------------
;;; init-package.el ends here
