;;; init-common.el --- Initialize common packages
;; -----------------------------------------------------------------------------
;;; Default frame size and other settings

;;;  Default frame size
(defvar my-default-frame-width 80)
(defvar my-default-frame-height 60)

;;;  Default background colour
(defvar my-default-background-color "white")

;;;  Set the default screen sizes
(setq default-frame-alist
      (append
       (list
        '(top . 100)
        '(left . 400)
        (cons 'width my-default-frame-width)
        (cons 'height my-default-frame-height)
        '(cursor-type . bar)
        ;;(cons 'background-color my-default-background-color)
        )
       default-frame-alist))

;;;  Set the initial screen location to be top-left
(setq initial-frame-alist
      (append
       (list
        '(top . 0) '(left . 0)
        '(cursor-type . bar)
        )
       default-frame-alist))

;;;  Set the scroll-bar to be on the right
(set-scroll-bar-mode 'right)

;;;  Set the frame title format
(setq frame-title-format
      (concat "%b %+%+%+ (%f) - " invocation-name))

;;;  Put column number into modeline
(column-number-mode 1)

;;;  Disable tabs; always indent with spaces
(setq-default indent-tabs-mode nil
              tab-width 4)

;;;  Enable the command `narrow-to-region' ("C-x n n") undo ("C-x n w")
(put 'narrow-to-region 'disabled nil)

;;;  Initialize info path to $INFOPATH
(setq Info-directory-list
      (let ((path (getenv "INFOPATH")))
        (cond
         (path (split-string path path-separator))
         ((and (boundp 'Info-default-directory-list)
               Info-default-directory-list))
         (t '("/usr/local/info" "/usr/local/share/info" "/usr/share/info")))))

;;;  Minibuffer depth indicator
(minibuffer-depth-indicate-mode 99)

;;;  Remove trailing spaces
;;  on buffer save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;;;  Typed text replaces the selection if the selection is active
(delete-selection-mode t)

;; -----------------------------------------------------------------------------
;;; undo-tree --- maintain and operate on undo/redo as a tree
(add-to-list 'load-path (expand-file-name  "~/.emacs.d/packages/undo-tree"))
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key [M-up] 'undo-tree-undo)
(global-set-key [M-down] 'undo-tree-redo)

;; -----------------------------------------------------------------------------
;;; browse-kill-ring+ --- Browse kill-ring using M-y
(require 'browse-kill-ring+)

;;;  Quick access and yank text from the kill ring.
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

;; -----------------------------------------------------------------------------
;;; linum --- Display line numbers
;;;  for the current buffer
;;;  Toggle display of line numbers with M-x linum-mode
(require 'linum)

;; -----------------------------------------------------------------------------
;;; replace+ --- Replace enhancements
(eval-after-load "replace" '(progn (require 'replace+)))

;; -----------------------------------------------------------------------------
;;; find-recursive --- Find-files recursively
(require 'find-recursive)

;; -----------------------------------------------------------------------------
;;; tramp --- Remote file editing
(load "init-tramp")

;; -----------------------------------------------------------------------------
;;; tar-mode --- Read and write archive files automatically
(autoload 'tar-mode "tar-mode")

;; -----------------------------------------------------------------------------
;;; jka-compr --- Read and write compressed files automatically
(require 'jka-compr)

;; -----------------------------------------------------------------------------
;;; dired+ --- Directory display and manipulation
(load "init-dired+")

;; -----------------------------------------------------------------------------
;;; traverselisp --- walk through directories to find regex in files
(require 'traverselisp)
;;(setq max-lisp-eval-depth 40000)

;; -----------------------------------------------------------------------------
;;; eiv --- emacs image viewer
;;;  walk through directories to view images (uses traverselisp)
(require 'eiv)

;; -----------------------------------------------------------------------------
;;; igrep ---  An improved interface to `grep` and `find`
(require 'grep)
;;(require 'igrep)

;; -----------------------------------------------------------------------------
;;; color-grep --- Enhance igrep
;;;  with cursor-following file buffer matching line highlight
(require 'color-grep)

;; -----------------------------------------------------------------------------
;;; grep-edit --- Edit grep buffer and apply the changes to files
(require 'grep-edit)

;; Rebind the up and down keys so they don't automatically select the file
;; the string is on to make it easier to edit the grep buffer.
(define-key grep-mode-map [down] 'next-line)
(define-key grep-mode-map [up] 'previous-line)

;; -----------------------------------------------------------------------------
;;; mgrep --- Grep in a predefined list of directories
(require 'mgrep)

(setq mgrep-list
      '(
        ;; name   directory          mask   option
        ("emacsd" default-directory  "*.el" nil)
        ("lisp" (concat default-directory "/lisp") "*.el" nil)
        ("OpenFOAM" (concat OPENFOAM_DIR "/src/OpenFOAM/lnInclude/") "*.[HC]" nil)
        ))

;; -----------------------------------------------------------------------------
;;; color-moccur --- An improved interface to occur and moccur
;;;  moccur <regexp> shows all occurrences of <regexp>
;;;  in all buffers that refer to files.
(require 'color-moccur)

;; -----------------------------------------------------------------------------
;;; moccur-edit --- Edit *moccur* buffers and apply changes
(require 'moccur-edit)

;; -----------------------------------------------------------------------------
;;; iedit --- Edit multiple regions with the same content simultaneously
(require 'iedit)

;; -----------------------------------------------------------------------------
;;; info+ --- Better info display
(eval-after-load "info" '(load "info+"))

;; -----------------------------------------------------------------------------
;;; finder+ --- Better function finder
(global-set-key (kbd "\C-hK") 'find-function-on-key)
(require 'finder+)

;; -----------------------------------------------------------------------------
;;; man --- Man-page reader

(defface my-Man-overstrike-face '((t (:foreground "blue" :weight bold)))
  "Face used for overstrike in man pages.")

(defface my-Man-underline-face '((t (:foreground "red" :weight bold)))
  "Face used for overstrike in man pages.")

(defface my-Man-reverse-face '((t (:foreground "orange" :weight bold)))
  "Face used for reverse in man pages.")

(setq Man-notify            'bully   ; resize man page to take up whole screen
      Man-overstrike-face   'my-Man-overstrike-face
      Man-underline-face    'my-Man-underline-face
      Man-reverse-face      'my-Man-reverse-face
      Man-see-also-regexp   "SEE ALSO\\|RELATED INFORMATION")

(setq Man-mode-hook (lambda () (local-set-key [f12] 'man-follow)))

;; -----------------------------------------------------------------------------
;;; iman --- man (and info) lookup with completion
(autoload 'iman "iman"
  "Call the viewers of man pages and GNU Info with completion."
  t nil)

(global-set-key "\C-cm" 'iman) ; `control c', then `m' calls `iman'

;; -----------------------------------------------------------------------------
;;; woman ---  Man-page reader without using man
(setq woman-cache-filename (expand-file-name "woman.cache" "~/Emacs")
      woman-bold-headings t
      woman-imenu-title "Sections"
      woman-imenu nil
      woman-use-own-frame nil
      woman-topic-at-point nil
      woman-fill-frame t)

(defun my-woman-pre-format-fn ()
  "Function added to `woman-pre-format-hook'."
  (copy-face 'my-Man-overstrike-face 'woman-bold-face)
  (copy-face 'my-Man-underline-face 'woman-italic-face)
  (face-spec-set 'woman-addition-face '((t (:foreground "orange"))))
  (face-spec-set 'woman-unknown-face  '((t (:foreground "cyan")))))

(add-hook 'woman-pre-format-hook 'my-woman-pre-format-fn)

;; -----------------------------------------------------------------------------
;;; doc-view --- Convert postscript files into images and display them
(add-to-list 'auto-mode-alist '("\\.[eE]?[pP][sS]\\'" . doc-view-mode))

;; -----------------------------------------------------------------------------
;;; antiword --- Convert Word documents into readable form
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
(setq no-word-check-word t
      no-word-ask-coding nil)

;; -----------------------------------------------------------------------------
;;; Show matching parentheses
(defun show-matching-paren ()
  "Show matching parentheses in extra-bold font"
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1)
  (set-face-background 'show-paren-match-face (face-background 'default))
  (set-face-foreground 'show-paren-match-face "black")
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
  (set-face-foreground 'show-paren-mismatch-face "red")
  (set-face-attribute 'show-paren-mismatch-face nil :weight 'extra-bold))

;; -----------------------------------------------------------------------------
;;; Better automatic commenting/un-commenting
;; From http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the
end of the line, then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the
end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(define-key emacs-lisp-mode-map "\C-c\C-c" 'comment-dwim-line)
(define-key lisp-mode-map "\C-c\C-c" 'comment-dwim-line)

;; -----------------------------------------------------------------------------
;;; Turn-off font-lock for Postscript files
;; (turn-on-font-lock)
(add-hook 'postscript-mode-hook 'turn-off-font-lock)

;; -----------------------------------------------------------------------------
;;; Turn-on font-lock for FORTRAN files
;; (turn-on-font-lock)
(add-hook 'fortran-mode-hook 'turn-on-font-lock)

;; -----------------------------------------------------------------------------
;;; Start client server
(defconst remote-display (getenv "DISPLAY")
  "Holds the `DISPLAY' environment variable which is set to the correct
value by the `-server' option for eemacs")

;;;  emacsclient core-dumps whet TCP is used
;;(setq server-use-tcp t
;;      server-host "10.0.0.37")

(defun command-line-server (switch)
  ;; Get the name of the server
  (setq server-name (pop command-line-args-left))

  (add-hook 'server-switch-hook
            (lambda nil
              (let ((server-buf (current-buffer)))
                (bury-buffer)
                (switch-to-buffer-other-frame server-buf))))

  ;; Ensure that all frames are closed when emacs exits
  ;;(add-hook 'server-done-hook 'delete-frame)
  ;;(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))
  (custom-set-variables '(server-kill-new-buffers t))
  (add-hook 'server-done-hook (lambda () (delete-frame)))
  (setq remote-display (getenv "DISPLAY"))
  (server-start))

;;(defun command-line-reset-display (switch)
;;  (setenv "DISPLAY" (car command-line-args-left))
;;  (setq command-line-args-left (cdr command-line-args-left)))

(add-to-list 'command-switch-alist '("-server" . command-line-server))

;; -----------------------------------------------------------------------------
;;; init-common.el ends here
