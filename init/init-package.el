;;; init-packge.el --- Package initialization
;; -----------------------------------------------------------------------------

(setq user-emacs-directory "~/Emacs/")

;; Don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)

(setq package-user-dir "~/Emacs/elpa/"
      package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(require 'use-package)

;; -----------------------------------------------------------------------------
;;; init-package.el ends here
