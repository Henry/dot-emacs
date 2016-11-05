;;; init-common.el --- Initialize common packages
;; -----------------------------------------------------------------------------
;;; Goto Last Change
;; Goto the point of the most recent edit in the buffer.
;; For key-bindings see my-map
(use-package goto-chg)

;; -----------------------------------------------------------------------------
;;; replace+ --- Replace enhancements
(use-package replace+)

;; -----------------------------------------------------------------------------
;;; find-recursive --- Find-files recursively
(require 'find-recursive)

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
;;; grep
(use-package grep
  ;; Rebind the up and down keys so they don't automatically select the file
  ;; the string is on to make it easier to edit the grep buffer.
  :bind (:map grep-mode-map
              ("<down>" . next-line)
              ("<up>" . previous-line))
  :config
  (add-to-list 'grep-files-aliases '("CH" . "*.[CH]")))

;; -----------------------------------------------------------------------------
;;; wgrep --- Edit grep buffer and apply the changes to files
(use-package wgrep)

;; -----------------------------------------------------------------------------
;;; phi-grep --- an Elisp implementation of grep
(use-package phi-grep)

;; -----------------------------------------------------------------------------
;;; color-moccur --- An improved interface to occur and moccur
;;;  moccur <regexp> shows all occurrences of <regexp>
;;;  in all buffers that refer to files.

(use-package color-moccur
  :commands (moccur moccur-grep moccur-grep-find)
  :init
  (require 'moccur-edit)
  (setq isearch-lazy-highlight t)
  :bind (:map moccur-mode-map
              ("<down>" . next-line)
              ("<up>" . previous-line)))

;; -----------------------------------------------------------------------------
;;; iedit --- Edit multiple regions with the same content simultaneously
(use-package iedit)

;; -----------------------------------------------------------------------------
;;; info+ --- Better info display
(use-package info
  :commands (info Info-mode)
  :config
  (use-package info+))

;; -----------------------------------------------------------------------------
;;; finder+ --- Better function finder
(use-package finder+
  :config (global-set-key (kbd "\C-hK") 'find-function-on-key))

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
;;; init-common.el ends here
