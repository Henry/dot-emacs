;;; init-company.el --- Initialize enhanced in-buffer completion package
;; -----------------------------------------------------------------------------
;;; Load package

(use-package company
  :diminish company-mode
  :commands company-mode
  :bind (("<M-tab>" . company-complete)
         :map flyspell-mode-map ("C-." . company-ispell))
  :init
  ;;(require 'company-nxml)
  ;;(require 'company-css)
  ;;(require 'company-eclim)
  ;;(require 'company-semantic)
  ;;(require 'company-clang)
  ;;(require 'company-xcode)
  ;;(require 'company-cmake)
  (require 'company-capf)
  (require 'company-files)
  ;;(require 'company-dabbrev-code)
  (require 'company-gtags)
  (require 'company-etags)
  (require 'company-keywords)
  ;;(require 'company-oddmuse)
  (require 'company-dabbrev)
  (require 'company-bbdb)
  (require 'company-elisp)
  (setq
   company-backends
   '(
     company-files
     company-dabbrev
     company-keywords
     company-capf
     company-bbdb
     ))
  (setq company-show-numbers t)

  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (set (make-local-variable 'company-backends)
          (list (cons 'company-elisp company-backends)))))

  (add-to-list 'company-bbdb-modes 'wl-draft-mode)
  (add-hook 'wl-draft-mode-hook '(lambda () (company-mode)))

  (global-company-mode t))

;; -----------------------------------------------------------------------------
;;; init-company.el ends here
