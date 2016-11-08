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
