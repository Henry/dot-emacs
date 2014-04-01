;;; init-runtime.el --- Initialisations which must be done at run-time
;;;  not built into eemacs.
;;   Windowing system properties which must be read at run-time
;;   Functions involving histories which must be read at run-time

;; -----------------------------------------------------------------------------
;;; Dvorak layout re-bindings
;; Swap the keys C-x <=> C-u and the global bindings M-x <=> M-u
;; To put C-x and M-x under your index finger on the home row.

(cond
 ((or (string= (getenv "HOSTNAME") "dm")
      (not (string= (getenv "DISPLAY") ":0.0")))

  ;; Key translation works for ctrl sequences:
  (keyboard-translate ?\C-x ?\C-u)
  (keyboard-translate ?\C-u ?\C-x)

  ;; ... but not for meta-keys so swap the global bindings instead:
  (global-set-key "\M-x" 'upcase-word)
  (global-set-key "\M-u" 'execute-extended-command)
  ))

;; -----------------------------------------------------------------------------
;;; Character remapping for w3m
;; Display using the original IBM graphics characters instead of
;; plain ASCII +, -, and |

(standard-display-ascii ?\200 [15])
(standard-display-ascii ?\201 [21])
(standard-display-ascii ?\202 [24])
(standard-display-ascii ?\203 [13])
(standard-display-ascii ?\204 [22])
(standard-display-ascii ?\205 [25])
(standard-display-ascii ?\206 [12])
(standard-display-ascii ?\210 [23])
(standard-display-ascii ?\211 [14])
(standard-display-ascii ?\212 [18])
(standard-display-ascii ?\214 [11])
;; (standard-display-ascii ?\221 [?\'])
;; (standard-display-ascii ?\222 [?\'])
;; (standard-display-ascii ?\223 [?\"])
;; (standard-display-ascii ?\224 [?\"])
;; (standard-display-ascii ?\225 [?+])
;; (standard-display-ascii ?\227 " -- ")

;; -----------------------------------------------------------------------------
;;; Font settings

(cond
 ((eq window-system 'x)

  ;; Set up highlighting of special annotations
  (make-face 'coding-special-annotations)
  (set-face-attribute 'coding-special-annotations nil
                      :foreground "White" :background "Red2")

  (let ((pattern "\\<\\(FIXME\\|TODO\\|NOTE\\|WARNING\\|BUGS\\|USE\\):"))
    (mapc
     (lambda (mode)
       (font-lock-add-keywords
        mode
        `((,pattern 1 'coding-special-annotations prepend))))
     '(
       c-mode
       c++-mode
       emacs-lisp-mode
       html-mode
       lisp-mode
       ruby-mode
       sh-mode
       yaqi-mode
       ))
    )

  (mapc
   (lambda (mode)
     (font-lock-add-keywords
      mode
      '(("^;;; [^ ].*" 0 'outline-2 t)))

     (font-lock-add-keywords
      mode
      '(("^;;;  [^ ].*" 0 'outline-3 t)))

     (font-lock-add-keywords
      mode
      '(("^;;;   [^ ].*" 0 'outline-4 t))))
   '(
     emacs-lisp-mode
     lisp-mode
     ))

  (mapc
   (lambda (mode)
     (font-lock-add-keywords
      mode
      '(("^/// [^ ].*" 0 'outline-2 t)))

     (font-lock-add-keywords
      mode
      '(("^///  [^ ].*" 0 'outline-3 t)))

     (font-lock-add-keywords
      mode
      '(("^ */// [^ ].*" 0 'outline-3 t)))

     (font-lock-add-keywords
      mode
      '(("^///   [^ ].*" 0 'outline-4 t)))

     (font-lock-add-keywords
      mode
      '(("^ *//- .*" 0 'font-lock-keyword-face t)))
     )
   '(
     c-mode
     c++-mode
     goofie-mode
     ))

  (mapc
   (lambda (mode)
     (font-lock-add-keywords
      mode
      '(("^%%% [^ ].*" 0 'outline-2 t)))

     (font-lock-add-keywords
      mode
      '(("^%%%  [^ ].*" 0 'outline-3 t)))

     (font-lock-add-keywords
      mode
      '(("^%%%   [^ ].*" 0 'outline-4 t))))
   '(
     TeX-mode
     LaTeX-mode
     ))

  (mapc
   (lambda (mode)
     (font-lock-add-keywords
      mode
      '(("^### [^ ].*" 0 'outline-2 t)))

     (font-lock-add-keywords
      mode
      '(("^###  [^ ].*" 0 'outline-3 t)))

     (font-lock-add-keywords
      mode
      '(("^###   [^ ].*" 0 'outline-4 t))))
   '(
     makefile-mode
     makefile-gmake-mode
     ))

  ;;   (font-lock-add-keywords
  ;;    'prolog-mode
  ;;    '(("^%%% [^ ].*" 0 'outline-2 t)))

  ;;   (font-lock-add-keywords
  ;;    'prolog-mode
  ;;    '(("^%%%  [^ ].*" 0 'outline-3 t)))

  ;;   (font-lock-add-keywords
  ;;    'prolog-mode
  ;;    '(("^%%%   [^ ].*" 0 'outline-4 t)))
  ))

;; -----------------------------------------------------------------------------
;;; Anything faces

(if (facep 'header-line)
    (copy-face 'header-line 'anything-header)

  (defface anything-header
    '((t (:bold t :underline t)))
    "Face for header lines in the anything buffer." :group 'anything))

(defface anything-file-name
  '((t (:foreground "Blue")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'anything)

(defface anything-dir-priv
  '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory privilege indicator (d) in dired buffers."
  :group 'anything)

;; -----------------------------------------------------------------------------
;;; Read saved minibuffer command, search-string and kill-ring histories

(setq savehist-file (expand-file-name "~/.Emacs/history")
      savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
      savehist-autosave-interval nil)
(savehist-mode 1)
(savehist-mode -1)

;; -----------------------------------------------------------------------------
;;; Read saved places in files

(setq-default save-place t)
(setq save-place-file (expand-file-name "~/.Emacs/places")
      save-place-limit 100)
(require 'saveplace)
(setq-default save-place nil)

;; -----------------------------------------------------------------------------
;;; Read the  file-journal
;; Note: (require 'file-journal) is in init-personalisation.el

(if (file-readable-p fj-journal-file)
    (load (expand-file-name fj-journal-file) nil t t))

;; -----------------------------------------------------------------------------
;;; init-runtime.el ends here
