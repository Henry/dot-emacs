;;; init-icicles.el --- Minibuffer input completion
;;;  and cycling of completion candidates

;; -----------------------------------------------------------------------------
;;; Set package path and load package

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/icicles"))

(when window-system (require 'hexrgb))
(require 'bbdb-com)
(require 'icicles)

;; -----------------------------------------------------------------------------
;;; General settings

(setq icicle-Completions-window-max-height 12
      icicle-require-match-flag 'no-match-required
      icicle-buffer-require-match-flag 'no-match-required
      icicle-file-require-match-flag 'no-match-required
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
        . forward-word)
      )

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
;; (define-key my-map "f" (lambda () (interactive)
;;                          (icicle-find-file-in-tags-table)
;;                          (icicle-insert-string-at-point)
;;                          (icicle-apropos-complete)
;;                          ))

;; -----------------------------------------------------------------------------
;;; Icicles `locate' interface
;;;  From http://www.emacswiki.org/emacs/RubikitchIciclesConfiguration

(require 'locate)
(icicle-define-command
 icicle-locate ; Command name
 "Run the program `locate', then visit files.
Unlike `icicle-locate-file' this command is a wrapper for the program `locate'."
 find-file           ; Function to perform the action
 "File: "
 (mapcar #'list
         (split-string
          (shell-command-to-string
           (format "%s '%s'" locate-command query))
          "\n" t))
 nil t nil 'locate-history-list nil nil
 ((query (read-string "Locate: "))))

;; -----------------------------------------------------------------------------
;;; Icicles menu selection mechanism (now using ee for this)
;;;  Menu-bar menu-command completion and execution via keyboard.
;;(require 'lacarte)

;; Globally set the key-sequence to get the icicles-menu to <ESC> M-x
;;(global-set-key "\e\M-x" 'lacarte-execute-menu-command)

;; -----------------------------------------------------------------------------
;;; Minibuffer incremental completion preview, sorted and coloured

(require 'icomplete+)
(setq icomplete-prospects-height 2)
(icomplete-mode t)
(partial-completion-mode)

;; -----------------------------------------------------------------------------
;;; Completing-help: press \M-? to display info on possible completions

(require 'completing-help)
(define-key minibuffer-local-completion-map "\M-?" 'minibuffer-completion-help)
(turn-on-completing-help-mode)

;; -----------------------------------------------------------------------------
;;; Switch-on icicles

(icy-mode t)

;; -----------------------------------------------------------------------------
;;; init-icicles.el ends here
