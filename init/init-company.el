;;; init-company.el --- Initialize enhanced in-buffer completion package
;; -----------------------------------------------------------------------------

(use-package company
  :diminish company-mode
  :commands company-mode
  :ensure company-quickhelp
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

  ;; Enable company-bbdb in wl-draft-mode
  (add-to-list 'company-bbdb-modes 'wl-draft-mode)
  (add-hook 'wl-draft-mode-hook '(lambda () (company-mode)))

  :bind (("<M-tab>" . company-complete)
         :map flyspell-mode-map
         ("C-." . company-flyspell)
         ("C->" . company-ispell)
         :map company-active-map
         ("M-h" . company-quickhelp-manual-begin))

  :config

  ;; Enable company-mode in all major model
  (global-company-mode t)

  ;; Tab-completion
  ;; (define-key company-mode-map [remap indent-for-tab-command]
  ;;   'company-indent-for-tab-command)
  ;; (setq tab-always-indent 'complete)

  ;; Popup tooltip help for completion candidates
  (company-quickhelp-mode))

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
;; Add support for tab-completion
;; https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387

(defvar completion-at-point-functions-saved nil)

(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

;; -----------------------------------------------------------------------------
;;; init-company.el ends here
