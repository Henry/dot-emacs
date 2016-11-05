;;; init-company.el --- Initialize enhanced in-buffer completion package
;; -----------------------------------------------------------------------------
;;; Load package

(use-package company
  :diminish company-mode
  :commands company-mode
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
  (require 'company-dabbrev-code)
  (require 'company-gtags)
  (require 'company-etags)
  (require 'company-keywords)
  ;;(require 'company-oddmuse)
  (require 'company-dabbrev)
  (require 'company-bbdb)
  (require 'company-elisp)
  (require 'company-flyspell)

  (setq
   company-backends
   '(
     company-bbdb
     company-capf
     company-files
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-dabbrev
     )
   company-show-numbers t
   ;;company-dabbrev-ignore-case nil
   ;;case-replace nil
   ;;company-dabbrev-downcase nil
   )

  ;; Add the Elisp backend in elisp-mode
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (set (make-local-variable 'company-backends)
          (cons 'company-elisp company-backends))))

  ;; Enable company-bbdb in wl-draft-mode
  (add-to-list 'company-bbdb-modes 'wl-draft-mode)
  (add-hook 'wl-draft-mode-hook '(lambda () (company-mode)))

  ;; Enable company-mode in all major model
  (global-company-mode t)

  :bind (("<M-tab>" . company-complete)
         :map flyspell-mode-map
         ("C-." . company-flyspell)
         ("C->" . company-ispell)))

(defun company-bbdb-word (command &optional arg &rest ignore)
  "`company-mode' completion backend for BBDB.
Looks-up the Email addresses corresponding to the the word at the point
 in any mode."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bbdb-word))
    (prefix (company-grab-word))
    (candidates (company-bbdb--candidates arg))
    (sorted t)
    (no-cache t)))

;; -----------------------------------------------------------------------------
;;; init-company.el ends here
