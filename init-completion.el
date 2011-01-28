;;; init-completion.el --- Initialize enhanced in-buffer completion package
;; -----------------------------------------------------------------------------
;;; Load package

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/completion-ui"))

(require 'completion-ui)
(require 'completion-ui-more-sources)

;; -----------------------------------------------------------------------------
;;; General configuration

(setq completion-max-candidates 36
      completion-hotkey-list
      '(?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4 ?\M-5 ?\M-6 ?\M-7 ?\M-8 ?\M-9
        ?\M-a ?\M-b ?\M-c ?\M-d ?\M-e ?\M-f ?\M-g ?\M-h ?\M-i ?\M-j ?\M-k ?\M-l
        ?\M-m ?\M-n ?\M-o ?\M-p ?\M-q ?\M-r ?\M-s ?\M-t ?\M-u ?\M-v ?\M-w ?\M-x
        ?\M-y ?\M-z
        )
      completion-use-hotkeys 'auto-show
      completion-auto-show 'completion-show-tooltip
      completion-auto-show-delay 0
      completion-use-echo nil
      completion-use-popup-frame nil
      completion-browser-recurse-on-completions nil
      completion-accept-or-reject-by-default 'reject
      completion-how-to-resolve-old-completions 'reject
      completion-auto-update t
      )

;;;  Rebind the C-down to pop-up the browser-menu rather than a frame
(define-key completion-menu-map [C-down]
  (lambda ()
    (interactive)
    (completion-activate-menu nil t)))

;;;  Bind the completion cycling function to TAB in the completion overlay
(define-key completion-overlay-map [tab] 'completion-cycle)

;;;  Bind the tooltip menu cycling into the `completion-overlay-map'
;;  so that the up-down keys cause the tooltip to pop-up
(define-key completion-overlay-map [down] 'completion-tooltip-cycle)
(define-key completion-overlay-map [up] 'completion-tooltip-cycle-backwards)

;;;  Accept parts of the current completion candidate using the left, right keys
(define-key completion-overlay-map [left]
  (lambda ()
    (interactive)
    (completion-extend-prefix -1)))
(define-key completion-overlay-map [right]
  (lambda ()
    (interactive)
    (completion-extend-prefix 1)))

;;;  Accept current completion on return
(define-key completion-overlay-map [return] 'completion-accept)

;;;  Reject current completion on C-g
(define-key completion-overlay-map [(control ?g)]
  (lambda ()
    (interactive)
    (completion-reject)
    (keyboard-quit)))

;; -----------------------------------------------------------------------------
;;; completion-selection-mode
(require 'completion-selection)

;; Re-bind the flyspell C-. to switch the completion-ui to ispell-correct
;; and cycle through the alternative spellings
(define-key flyspell-mode-map [(control ?\.)]
  (lambda ()
    "Call `completion-selection-select-complete' for the ispell-correct
completion mechanism."
    (interactive)
    (completion-selection-set-complete 'complete-ispell)))

;; -----------------------------------------------------------------------------
;;; init-completion.el ends here
