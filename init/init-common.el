;;; init-common.el --- Initialize common packages
;; -----------------------------------------------------------------------------
;;; Goto Last Change
;; Goto the point of the most recent edit in the buffer.
;; For key-bindings see my-map
(use-package goto-chg
  :ensure t)

;; -----------------------------------------------------------------------------
;;; replace+ --- Replace enhancements
(use-package replace+
  :ensure t)

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
  :ensure t
  ;; Rebind the up and down keys so they don't automatically select the file
  ;; the string is on to make it easier to edit the grep buffer.
  :bind (:map grep-mode-map
              ("<down>" . next-line)
              ("<up>" . previous-line))
  :config
  (add-to-list 'grep-files-aliases '("CH" . "*.[CH]")))

;; -----------------------------------------------------------------------------
;;; wgrep --- Edit grep buffer and apply the changes to files
(use-package wgrep
  :ensure t)

;; -----------------------------------------------------------------------------
;;; phi-grep --- an Elisp implementation of grep
(use-package phi-grep
  :ensure t)

;; -----------------------------------------------------------------------------
;;; color-moccur --- An improved interface to occur and moccur
;;;  moccur <regexp> shows all occurrences of <regexp>
;;;  in all buffers that refer to files.

(use-package color-moccur
  :commands (moccur moccur-grep moccur-grep-find)
  :init
  (require 'moccur-edit)

  (defun moccur-show ()
    "Show the current candidate in the buffer in the other window"
    (interactive)
    (setq moccur-mocur-buffer (current-buffer))
    (beginning-of-line)
    (moccur-get-info)
    (if (and moccur-view-other-window
             moccur-view-other-window-nobuf
             moccur-following-mode-toggle)
        (moccur-view-file)))

  (defun my-moccur-mode-hook ()
    (local-set-key (kbd "<down>") 'next-line)
    (local-set-key (kbd "<up>") 'previous-line)
    (local-set-key (kbd "<tab>") 'moccur-show))

  (add-hook 'moccur-mode-hook 'my-moccur-mode-hook))

;; -----------------------------------------------------------------------------
;;; greed --- An improved interface to occur and moccur
;;;  greed <regexp> shows all occurrences of <regexp>
;;;  in all buffers that refer to files.

(use-package greed
  :commands (greed greed-occur greed-grep greed-dir greed-dired greed-grep-find
                   greed-buffer-menu greed-ibuffer greed-isearch))

;; -----------------------------------------------------------------------------
;;; iedit --- Edit multiple regions with the same content simultaneously
(use-package iedit
  :ensure t)

;; -----------------------------------------------------------------------------
;;; info+ --- Better info display
(use-package info
  :ensure t
  :commands (info Info-mode)
  :config
  (use-package info+
  :ensure t))

;; -----------------------------------------------------------------------------
;;; finder+ --- Better function finder
(use-package finder+
  :ensure t
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
;;; key-cat --- Display summary of key-bindings
(require 'key-cat)

;; -----------------------------------------------------------------------------
;;; init-common.el ends here
