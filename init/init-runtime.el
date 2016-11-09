;;; init-runtime.el --- Initialisations which must be done at run-time
;;;  not built into eemacs.
;;   Windowing system properties which must be read at run-time
;;   Functions involving histories which must be read at run-time

;; -----------------------------------------------------------------------------
;;; Undo-tree

(global-set-key "\M-i"      'undo-tree-undo)
(global-set-key "\M-u"      'undo-tree-redo)

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

(setq savehist-file (expand-file-name "history" user-emacs-directory)
      savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
      savehist-autosave-interval nil)
(savehist-mode 1)
(savehist-mode -1)

;; -----------------------------------------------------------------------------
;;; Read saved places in files

(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory)
      save-place-limit 100)
(require 'saveplace)
(setq-default save-place nil)

;; -----------------------------------------------------------------------------
;;; Read the  file-journal
;; Note: (require 'file-journal) is in init-personalisation.el

;; (if (file-readable-p fj-journal-file)
;;    (load (expand-file-name fj-journal-file) nil t t))

;; -----------------------------------------------------------------------------
;;; init-runtime.el ends here
