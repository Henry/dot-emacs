;;; init-which-key.el --- Display key-bindings in key-map in minibuffer
;; -----------------------------------------------------------------------------

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'minibuffer
        which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order

        which-key-replacement-alist
        '((("<left>")                 "←")
          (("<up>")                   "↑")
          (("<right>")                "→")
          (("<down>")                 "↓")
          (("<left>")                 "←")
          (("<left>")                 "←")
          (("up")                     "↑")
          (("right")                  "→")
          (("down")                   "↓")
          (("left")                   "←")
          (("DEL")                    "⌫")
          (("deletechar")             "⌦")
          (("RET")                    "⏎")
          (("SPC")                    "␣")
          (("TAB")                    "↹")
          (("delete")                 "⌫")
          (("next")                   "PgDn")
          (("prior")                  "PgUp")
          (("ESC")                    "⎋")
          (("<\\([[:alnum:]-]+\\)>")  "\\1")

          ((nil . "Prefix Command")   . (nil . "prefix"))
          ((nil . "\\`\\?\\?\\'")     . (nil . "λ"))
          ((nil . "\\`projectile-vc") . (nil . "magit"))
          ((nil . "\\`projectile-")   . (nil . ""))))

  ;; Change what string to display for a given *complete* key binding
  ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
  (which-key-add-key-based-replacements
    "C-c p"   "projectile"
    "C-c p s" "search"
    "C-c p 4" "other"
    "C-x 8"   "unicode"
    "C-x a"   "abbrev/expand"
    "C-x r"   "rect/reg")

  ;; Highlight certain commands
  (defface my-wk-highlight-face
    '((t . (:inherit which-key-command-description-face
                     :foreground "indian red")))
    "Face for highlighting commands starting with \"my-\".")

  (setq which-key-highlighted-command-list
        '(("\\`counsel-" . which-key-group-description-face)
          ("\\`ivy-" . my-wk-highlight-face)
          ;; Highlight using the default `which-key-highlighted-command-face'
          "\\(rectangle-\\)\\|\\(-rectangle\\)"
          "\\`org-"))

  (which-key-mode 1))

;; -----------------------------------------------------------------------------
;;; init-which-key.el ends here
