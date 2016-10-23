;;; init-elisp.el --- Settings for emacs lisp mode

;; -----------------------------------------------------------------------------
(require 'newcomment) ; Include the uncomment-region function
(use-package header2)
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode)
(use-package hl-sexp
  :diminish hl-sexp-mode)
(use-package paredit
  :diminish paredit-mode)

;; -----------------------------------------------------------------------------
;;; Add the first line of the documentation to the ElDoc in the minibuffer

(defadvice eldoc-highlight-function-argument
  (around my-formatting (sym args index) compile activate preactivate)
  "Replace original to apply my style of formatting."
  ;; HACK: intercept the call to eldoc-docstring-format-sym-doc at the
  ;; end of the adviced function. This is obviously brittle, but the
  ;; alternative approach of copy/pasting the original also has
  ;; downsides...
  (cl-flet ((eldoc-docstring-format-sym-doc
          (sym doc face)
          (let* ((function-name (propertize (symbol-name sym)
                                            'face face))
                 (spec (format "%s %s" function-name doc))
                 (docstring (or (eldoc-docstring-first-line
                                 (documentation sym t))
                                "Undocumented."))
                 (docstring (propertize docstring
                                        'face 'font-lock-doc-face))
                 ;; TODO: currently it strips from the start of spec by
                 ;; character instead of whole arguments at a time.
                 (fulldoc (format "%s: %s" spec docstring))
                 (ea-width (1- (window-width (minibuffer-window)))))
            (cond ((or (<= (length fulldoc) ea-width)
                       (eq eldoc-echo-area-use-multiline-p t)
                       (and eldoc-echo-area-use-multiline-p
                            (> (length docstring) ea-width)))
                   fulldoc)
                  ((> (length docstring) ea-width)
                   (substring docstring 0 ea-width))
                  ((>= (- (length fulldoc) (length spec)) ea-width)
                   docstring)
                  (t
                   ;; Show the end of the partial symbol name, rather
                   ;; than the beginning, since the former is more likely
                   ;; to be unique given package namespace conventions.
                   (setq spec (substring spec (- (length fulldoc) ea-width)))
                   (format "%s: %s" spec docstring))))))
    ad-do-it))

;; -----------------------------------------------------------------------------
;;; Tooltip-help

(require 'tooltip-help)
(setq tooltip-help-max-lines 40)

(defun describe-variable-or-function ()
  "Display the full documentation of VARIABLE (a symbol) if it is a variable
otherwise Display the full documentation of FUNCTION (a symbol)."
  (interactive)
  (let ((symbol (intern-soft (current-word))))
    (cond ((null symbol)
           (message "Null symbol: no help available."))
          ((and (boundp symbol) (fboundp symbol))
           (let* ((types '(("variable" . describe-variable)
                           ("function" . describe-function)))
                  (name (completing-read
                         "Describe: " types nil t nil nil (car types))))
             (mapc (lambda (type)
                     (if (equal name (car type))
                         (funcall (cdr type) symbol)))
                   types)))
          ((boundp symbol)
           (describe-variable symbol))
          ((fboundp symbol)
           (describe-function symbol))
          (t
           (message "No help available.")))))

(global-set-key [(control shift h)] tooltip-help-map)
(tooltip-help-make-help-keymap)
(define-key emacs-lisp-mode-map [f1] 'tooltip-help-mode-show)
(define-key emacs-lisp-mode-map [S-f1] 'describe-variable-or-function)

;; -----------------------------------------------------------------------------
;;; header2

(defsubst header-end-line ()
  "Insert a divider line."
  (insert ";; " (make-string 77 ?-) "\n"))

(defun make-divider (&optional end-col)
  "Insert a comment divider line"
  (interactive)
  (header-end-line))

(setq header-copyright-notice
      "Copyright (C) 2010, Henry G. Weller, all rights reserved.\n")

;; -----------------------------------------------------------------------------
;;; Mode hook

(defun my-emacs-lisp-mode-hook ()
  "Hook to apply my setting to the lisp and emacs-lisp modes"

  ;; Show the argument list of the function you are in
  (autoload 'turn-on-eldoc-mode "eldoc" nil t)
  (turn-on-eldoc-mode)

  (font-lock-mode 1)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  (show-matching-paren)
  (highlight-parentheses-mode t)

  ;; Switch on fly-spell mode in comments
  (flyspell-prog-mode)

  (paredit-mode +1)

  (setq thing-types
        '("symbol" "string" "sexp" "list" "defun"
          "word" "line" "sentence" "paragraph" "page"))

  ;; Set the regexp used by outline-mode to find the headings
  (set (make-local-variable 'outline-regexp) ";;;[ ]+\\|(......")

  ;; Remove the number of ";;;" from the outline-level
  (set (make-local-variable 'outline-level) '(lambda () (- (outline-level) 3)))

  ;; Set the heading levels for promotion and demotion
  (setq outline-promotion-headings '(";;; " ";;;  " ";;;   "))

  ;; Turn on outline minor mode by default
  (outline-minor-mode +1)
  )

;; -----------------------------------------------------------------------------
;;; Add hook

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook 'my-emacs-lisp-mode-hook)

;; Enable emacs-lisp mode for files with either
;; #!<path>/emacs
;; #!<path>/eemacs
;; on the first line
(add-to-list 'interpreter-mode-alist '("emacs"  . emacs-lisp-mode))
(add-to-list 'interpreter-mode-alist '("eemacs"  . emacs-lisp-mode))

;; Enable lisp mode for files with
;; #!<path>/sbcl
;; on the first line
(add-to-list 'interpreter-mode-alist '("sbcl-shell"  . lisp-mode))

;; -----------------------------------------------------------------------------
;;; init-elisp.el ends here
