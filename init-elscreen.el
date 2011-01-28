;;; init-elscreen.el --- Initialisation for elscreen

;; -----------------------------------------------------------------------------
;;; Package path and load

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/elscreen"))

(setq elscreen-e21-command-line nil)

(defvar elscreen-prefix-key "\M-t")

(require 'elscreen)
(require 'elscreen-server)
(require 'elscreen-dired)
(require 'elscreen-speedbar)
(require 'elscreen-w3m)
(require 'elscreen-wl)

(defun elscreen-eshell ()
  "Creates a new screen with shell in it"
  (interactive)
  (elscreen-create)
  (multi-eshell 1))

(defun elscreen-shell ()
  "Creates a new screen with shell in it"
  (interactive)
  (elscreen-create)
  (multi-shell-new))

;; -----------------------------------------------------------------------------
;;; Bind \M-t[f1-f9] to switch between the screens

(eval-when-compile (require 'cl)) ; for set-difference and loop

(defmacro elscreen-define-f-keys (mode-map)
  `(progn
     ,@(loop for number from 0 to 9 collect
             `(define-key ,mode-map
                (kbd ,(concat
                       "<f" (number-to-string (+ number 1)) ">"))
                (lambda () (interactive)
                  (elscreen-goto ,number))))))

(elscreen-define-f-keys elscreen-map)

;; -----------------------------------------------------------------------------
;;; Number the windows and bind \M-t[1-9] to switch between them

(require 'window-number)

;; Define \M-t 1 to switch to win 1, etc (\M-t 0 = win 10)
;; space after M-t is important
(window-number-define-keys window-number-mode-map "M-t ")

(window-number-mode 1)

;; -----------------------------------------------------------------------------
;;; Bind \M-t<arrow-keys> to navigate between windows

(require 'windmove)

(global-set-key [(meta t)(left)]  'windmove-left)
(global-set-key [(meta t)(right)] 'windmove-right)
(global-set-key [(meta t)(up)]    'windmove-up)
(global-set-key [(meta t)(down)]  'windmove-down)

;; -----------------------------------------------------------------------------
;;; init-elscreen.el ends here
