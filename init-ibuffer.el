;;; init-ibuffer.el --- Initialize buffer selection menu
;; -----------------------------------------------------------------------------

(require 'ibuffer)

(setq ibuffer-default-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer t
      ibuffer-default-sorting-mode 'alphabetic
      ibuffer-use-header-line t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired"
                (mode . dired-mode))
               ("C++"
                (mode . c++-mode))
               ("C"
                (mode . c-mode))
               ("Elisp"
                (mode . emacs-lisp-mode))
               ("Org"
                (or
                 (name . "^\\*Calendar\\*$")
                 (name . "^diary$")
                 (mode . org-mode)))
               ("Emacs"
                (or
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")
                 (name . "^\\*Completions\\*$")
                 (name . "^\\*Help\\*$")))
               ("Wl"
                (or
                 (mode . wl-folder-mode)
                 (mode . wl-summary-mode)
                 (mode . wl-draft-mode)
                 (mode . mime-view-mode)
                 (mode . bbdb-mode)
                 (name . "^\\.bbdb$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; -----------------------------------------------------------------------------
;;; init-ibuffer.el ends here
