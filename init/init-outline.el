;;; init-outline.el --- Initialisation for outline and outline-minor modes
;;;  Outline mode is for simple folding and file organisation

;; ---------------------------------------------------------------------------
;;; Reset the default keybindings

(let ((map outline-minor-mode-map))
  ;; SHOW
  (define-key map "\M-oa" 'show-all)          ; Show (expand) everything
  (define-key map "\M-oe" 'show-entry)        ; Show this heading's body
  (define-key map "\M-oi" 'show-children)     ; Show this heading's immediate child sub-headings
  (define-key map "\M-ok" 'show-branches)     ; Show all sub-headings under this heading
  (define-key map "\M-os" 'show-subtree)      ; Show (expand) everything in this heading & below
  ;; HIDE
  (define-key map "\M-oq" 'hide-sublevels)    ; Hide everything but the top-level headings
  (define-key map "\M-ot" 'hide-body)         ; Hide everything but headings (all body lines)
  (define-key map "\M-oo" 'hide-other)        ; Hide other branches
  (define-key map "\M-oc" 'hide-entry)        ; Hide this entry's body
  (define-key map "\M-ol" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
  (define-key map "\M-od" 'hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; MOVE
  (define-key map "\M-ou" 'outline-up-heading)                ; Up
  (define-key map "\M-on" 'outline-next-visible-heading)      ; Next
  (define-key map "\M-op" 'outline-previous-visible-heading)  ; Previous
  (define-key map "\M-of" 'outline-forward-same-level)        ; Forward - same level
  (define-key map "\M-ob" 'outline-backward-same-level)       ; Backward - same level
  (define-key map [(meta o)(return)] 'outline-insert-heading) ; New heading
  (define-key map [(meta o)(up)] 'outline-move-subtree-up)    ; Move sub-tree up
  (define-key map [(meta o)(down)] 'outline-move-subtree-down); Move sub-tree down
  (define-key map [(meta o)(left)] 'outline-promote)          ; Promote sub-tree up
  (define-key map [(meta o)(right)] 'outline-demote))         ; Demote sub-tree up

;; -----------------------------------------------------------------------------
;;; outline-magic adds visibility cycling, promotion/demotion and
;;; sub-tree movement a la org-mode

(use-package outline-magic)

;;;  Add the outline-magic visibility cycling key bindings

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [S-iso-lefttab]
              'outline-cycle)
            (define-key outline-minor-mode-map [C-S-iso-lefttab]
              (lambda () (interactive) (outline-cycle '(4))))))

(add-hook 'outline-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [S-iso-lefttab]
              'outline-cycle)
            (define-key outline-minor-mode-map [C-S-iso-lefttab]
              (lambda () (interactive) (outline-cycle '(4))))))

;; -----------------------------------------------------------------------------
;;; init-outline.el ends here
