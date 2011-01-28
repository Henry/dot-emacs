;;; init-slime.el --- Initialize settings for Common Lisp development
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/slime"))
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/slime/contrib"))
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/cl-lookup"))

(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/slime/doc") t)
(add-to-list 'Info-directory-list
             (expand-file-name "~/CL/doc/dpANS/info") t)
(add-to-list 'Info-directory-list
             (expand-file-name "~/CL/doc/Books/OnLisp") t)
(add-to-list 'Info-directory-list
             (expand-file-name "~/CL/doc/Manuals/CLX") t)
(add-to-list 'Info-directory-list
             (expand-file-name "~/CL/Lisp/iterate/doc") t)

(require 'paredit)

;; -----------------------------------------------------------------------------
;;; Basic slime configuration
(setq slime-lisp-implementations
      '((sbclsh ("sbclsh"))
        (sbcl ("sbcl"))
        (clisp ("clisp" "-K full"))
        ,@slime-lisp-implementations)
      slime-startup-animation nil
      cl-path (expand-file-name "~/CL/")
      hyperspec-path (concat cl-path "doc/HyperSpec/")
      common-lisp-hyperspec-root (concat "file://" hyperspec-path)
      common-lisp-hyperspec-symbol-table (concat hyperspec-path "Data/Map_Sym.txt"))

;;;  Recognize ASDF files used by newer CL systems:
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

;;;  Load slime on demand
(require 'slime-autoloads)

;; -----------------------------------------------------------------------------
;;; Slime configuration after loading
(eval-after-load "slime"
  '(progn
    (add-to-list 'load-path "/path/to/slime/contrib")
    (require 'slime-fancy)
    (require 'slime-banner)
    (require 'slime-asdf)
    (slime-banner-init)
    (slime-asdf-init)
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    (setq lisp-indent-function 'common-lisp-indent-function)

    ;;  Set the REPL to use paredit-mode
    (add-hook 'slime-repl-mode-hook
     (lambda ()
       (paredit-mode +1)))

    ;; Start slime automatically when needed
    (setq slime-auto-connect 'always)

    ;; Register info help dpans2texi, sbcl, iterate and clx
    ;; Use C-h S to lookup symbol at point
    (require 'info-look)
    (info-lookup-add-help
     :mode 'lisp-mode
     :regexp "[^][()'\" \t\n]+"
     :ignore-case t
     :doc-spec '(("(ansicl)Symbol Index" nil nil nil)
                 ("(sbcl)Function Index" nil nil nil)
                 ("(sbcl)Variable Index" nil nil nil)
                 ("(sbcl)Type Index" nil nil nil)
                 ("(iterate)Comprehensive Index" nil nil nil)
                 ("(clx)Function Index" nil nil nil)
                 ("(clx)Type Index" nil nil nil)
                 ))

    (slime-setup)

    ;; Load cl-lookup and use default cl-lookup-categories
    (require 'cl-lookup)

    ;; Use a local copy of the cl-ppcre documentation
    (setq cl-lookup-ppcre-root
     "file:///usr/local/lib/sbcl/site/cl-ppcre-2.0.1/doc/index.html")

    ;; Override SLIME hyperspec lookup key bindings
    (define-key slime-mode-map (kbd "C-c C-d h") 'cl-lookup)
    (define-key slime-repl-mode-map (kbd "C-c C-d h") 'cl-lookup)))

;; -----------------------------------------------------------------------------
;;;  Automatically start slime when reading lisp files
;; (add-hook 'lisp-mode-hook (lambda ()
;;                             (cond ((not (featurep 'slime))
;;                                    (require 'slime)
;;                                    (normal-mode)))))

;; -----------------------------------------------------------------------------
;;; Fontify *SLIME Description <sbcl>* buffer for SBCL

(defun slime-description-fontify ()
  "Fontify sections of SLIME Description."
  (with-current-buffer "*SLIME Description <sbcl>*"
    (highlight-regexp
     (concat "^Function:\\|"
             "^Macro-function:\\|"
             "^Its associated name.+?) is\\|"
             "^The .+'s arguments are:\\|"
             "^Function documentation:$\\|"
             "^Its.+\\(is\\|are\\):\\|"
             "^On.+it was compiled from:$")
     'hi-green-b)))

(defadvice slime-show-description (after slime-description-fontify activate)
  "Fontify sections of SLIME Description."
  (slime-description-fontify))

;; -----------------------------------------------------------------------------
;;; Improve usability of slime-apropos: slime-apropos-minor-mode

(defvar slime-apropos-anchor-regexp "^[^ ]")
(defun slime-apropos-next-anchor ()
  (interactive)
  (let ((pt (point)))
    (forward-line 1)
    (if (re-search-forward slime-apropos-anchor-regexp nil t)
        (goto-char (match-beginning 0))
      (goto-char pt)
      (error "anchor not found"))))

(defun slime-apropos-prev-anchor ()
  (interactive)
  (let ((p (point)))
    (if (re-search-backward slime-apropos-anchor-regexp nil t)
        (goto-char (match-beginning 0))
      (goto-char p)
      (error "anchor not found"))))

(defvar slime-apropos-minor-mode-map (make-sparse-keymap))
(define-key slime-apropos-minor-mode-map [return] 'slime-describe-symbol)
(define-key slime-apropos-minor-mode-map "d" 'slime-describe-symbol)
(define-key slime-apropos-minor-mode-map "n" 'slime-apropos-next-anchor)
(define-key slime-apropos-minor-mode-map "p" 'slime-apropos-prev-anchor)
(define-minor-mode slime-apropos-minor-mode "")

(defadvice slime-show-apropos (after slime-apropos-minor-mode activate)
  ""
  (when (get-buffer "*SLIME Apropos*")
    (with-current-buffer "*SLIME Apropos*" (slime-apropos-minor-mode 1))))

;; -----------------------------------------------------------------------------
;;; init-slime.el ends here
