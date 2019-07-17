;;; init-packge.el --- Package initialization
;; -----------------------------------------------------------------------------

(setq user-emacs-directory "~/Emacs/"
      package-user-dir (concat user-emacs-directory "elpa/")
      package-enable-at-startup nil
      load-prefer-newer t ; Don't load outdated byte code
      )

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/") t)

(package-initialize)
;;(package-refresh-contents)

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(require 'use-package)

;; Install useful package utilities
(use-package package-utils
  :ensure t)

;; -----------------------------------------------------------------------------
;;; init-package.el ends here
