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

(global-set-key "\M-i"      'undo-tree-undo)
(global-set-key "\M-x"      'undo-tree-redo)

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
