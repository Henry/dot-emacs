;;; init-icicles.el --- Minibuffer input completion
;; -----------------------------------------------------------------------------
(use-package icicles
  :ensure t
  :diminish icicle-mode
  :init
  (setq icicle-functions-to-redefine
        (quote
         (
          ;;comint-completion-at-point
          ;;comint-dynamic-complete-filename
          ;;comint-replace-by-expanded-filename
          complete
          completion-pcm--all-completions
          ess-complete-object-name
          gud-gdb-complete-command
          Info-goto-node
          Info-index
          Info-menu
          lisp-complete-symbol
          elisp-completion-at-point
          minibuffer-default-add-completions
          read-char-by-name
          read-color
          read-from-minibuffer
          read-string
          recentf-make-menu-items)))
  :config
  (progn
    (when window-system (require 'hexrgb))
    (require 'icicles-mac)

    ;; General settings
    (setq icicle-Completions-window-max-height 12
          icicle-require-match-flag 'no-match-required
          icicle-buffer-require-match-flag 'no-match-required
          icicle-file-require-match-flag 'no-match-required
          icicle-show-Completions-help-flag nil
          icicle-key-complete-keys '([C-M-tab])
          ;;icicle-default-value 'insert-end
          icicle-sort-functions-alist
          '(("by last use as input" . icicle-most-recent-first-p)
            ("by previous use alphabetically" . icicle-historical-alphabetic-p)
            ("by abbrev frequency" . icicle-command-abbrev-used-more-p)
            ("by directories last" . icicle-dirs-last-p)
            ("by last file modification time" . icicle-last-modified-first-p)
            ("by 2nd parts alphabetically" . icicle-2nd-part-string-less-p)
            ("case insensitive" . icicle-case-insensitive-string-less-p)
            ("proxy candidates first" . icicle-proxy-candidate-first-p)
            ("extra candidates first" . icicle-extra-candidates-first-p)
            ("special candidates first" . icicle-special-candidates-first-p)
            ("alphabetical" . icicle-case-string-less-p)
            ("turned OFF"))
          icicle-thing-at-point-functions
          '((itap-for-mode
             thing-at-point-filename-at-point
             ffap-guesser
             (lambda ()
               (symbol-name (symbol-at-point)))
             (lambda ()
               (thing-at-point 'word))
             thing-at-point-url-at-point)
            . forward-word))

    ;; Bind S-down to switch from the completion buffer back to the minibuffer
    (add-to-list 'icicle-completion-list-key-bindings
                 '([S-down] icicle-switch-to/from-minibuffer t))
    ))

;; -----------------------------------------------------------------------------

;; Bind S-up to switch from the minibuffer to the completion buffer
(defun bind-my-icicles-keys ()
  "Replace some default Icicles minibuffer bindings with others."
  (dolist (map (append (list minibuffer-local-completion-map
                             minibuffer-local-must-match-map)
                       (and (fboundp 'minibuffer-local-filename-completion-map)
                            (list minibuffer-local-filename-completion-map))))
    (when icicle-mode
      (define-key map (icicle-kbd "S-up")
        'icicle-switch-to-Completions-buf)))
  (add-to-list 'icicle-completion-list-key-bindings
               '([S-down] icicle-switch-to/from-minibuffer t)))

(add-hook 'icicle-mode-hook 'bind-my-icicles-keys)

(defvar itap-mode-alist
  '(
    (emacs-lisp-mode . ffap-guesser)
    (c++-mode . thing-at-point-filename-at-point)
    (cc-mode . thing-at-point-filename-at-point)
    )
  "Alist of \(MODE . FUNCTION\) pairs parsed by `itap-for-mode'.
A pair matches if MODE equals `major-mode'.
On a match, \(FUNCTION\) is called and should return a file name or nil.
If nil, search the alist for further matches.")

(defun itap-for-mode ()
  "Lookup the `icicle-thing-at-point' function for the current mode,
call it and return the resulting file name or nil if a valid file name is
not under the point."
  (let ((alist itap-mode-alist)
        mode-func
        try)
      (while (and alist (not try))
        (setq mode-func (car alist)
              alist (cdr alist))
        (if (eq major-mode (car mode-func))
            (setq try
                  (condition-case nil
                      (funcall (cdr mode-func))
                    (error nil)))
             ))
      try))

(define-key my-map [(control ?f)] 'icicle-find-file-in-tags-table)

;; -----------------------------------------------------------------------------
;;; Minibuffer incremental completion preview, sorted and coloured

(use-package icomplete+
  :ensure t
  :config
  (progn
    (setq icomplete-prospects-height 2)
    (icomplete-mode t)
    (setq partial-completion-mode t)))

;; -----------------------------------------------------------------------------
;;; Switch-on icicles

(icy-mode t)

;; -----------------------------------------------------------------------------
;;; init-icicles.el ends here
