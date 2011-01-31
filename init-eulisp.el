;;; init-eulisp.el --- Initialize settings for EuLisp development
;; -----------------------------------------------------------------------------

;;(add-to-list 'load-path
;;             (expand-file-name "~/EuLisp/EuLisp/emacs"))
(require 'eulisp-mode)
(require 'inf-eulisp)

(require 'highlight-parentheses)
(require 'hl-sexp)
(require 'paredit)
(require 'cldoc)
(require 'outline)

(setq inferior-eulisp-program
      (expand-file-name "~/EuLisp/EuLisp/Bin.x86_64/youtoo.sh"))

;; -----------------------------------------------------------------------------
;;; Mode hook

(defun my-eulisp-mode-hook ()
  "Hook to apply my setting to the EuLisp"

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  (show-matching-paren)
  (highlight-parentheses-mode t)

  ;; Switch on fly-spell mode in comments
  (flyspell-prog-mode)

  (paredit-mode +1)

  (turn-on-cldoc-mode)

  (setq thing-types
        '("symbol" "string" "sexp" "list" "defun"
          "word" "line" "sentence" "paragraph" "page"))

  ;; Turn on outline minor mode by default
  (outline-minor-mode +1)
  )

(defun my-inferior-eulisp-mode-hook ()
  (my-eulisp-mode-hook)

  ;; Don't show trailing whitespace
  (whitespace-mode nil)

  ;; Load on start-up
  ;;(eulisp-load-file (expand-file-name "~/EuLisp/Enhancements/stuff.eulisp"))
  )

;; -----------------------------------------------------------------------------
;;; Add hook

(add-hook 'eulisp-mode-hook 'my-eulisp-mode-hook)
(add-hook 'inferior-eulisp-mode-hook 'my-inferior-eulisp-mode-hook)

;; For Eu2C
(add-to-list 'auto-mode-alist '("\\.ecl\\'" . eulisp-mode))
(setq require-final-newline 'visit-save)

;; -----------------------------------------------------------------------------
;;; Makefiles

(defun my-makefile-mode-hook ()

  ;; Set the regexp used by outline-mode to find the headings
  (set (make-local-variable 'outline-regexp) "###[ ]+\\|(......")

  ;; Remove the number of "\\\" from the outline-level
  (set (make-local-variable 'outline-level) '(lambda () (- (outline-level) 3)))

  ;; Set the heading levels for promotion and demotion
  (setq outline-promotion-headings '("### " "###  " "###   "))

  ;; Turn on outline minor mode by default
  (outline-minor-mode +1)

  ;; Switch-on font-lock
  (font-lock-mode 1))

(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

;; -----------------------------------------------------------------------------
;;; init-eulisp.el ends here
