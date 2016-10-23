;;; init-flyspell.el --- Initialize on-the-fly spell checking
;; -----------------------------------------------------------------------------

;;;  Stop flyspell taking-over M-TAB, it's needed for other things
(setq flyspell-use-meta-tab nil)

(require 'flyspell)

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))

;;;  Choose aspell which is better at suggesting alternatives
(setq ispell-program-name "aspell")

;;;  Choose a faster but less accurate mode for aspell
(setq ispell-extra-args '("--sug-mode=fast"))

;;;  Don't print messages about the spell-checking
(setq flyspell-issue-message-flag nil)

;;;  Enable for text-mode, and disable it for log-edit-mode and change-log-mode.
(dolist (hook '(text-mode-hook))
  (add-hook hook
            (lambda ()
              (flyspell-mode 1)              ; switch on by default
              (local-unset-key [(meta tab)]) ; stop text-mode using M-TAB
              )))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(defun flyspell-add-word-to-dict ()
  "Add the word at the current location to the private dictionary
without question."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (setq opoint (point-marker))
  (let ((cursor-location (point))
        (word (flyspell-get-word nil)))
    (if (consp word)
        (let ((start (car (cdr word)))
              (end (car (cdr (cdr word))))
              (word (car word)))
          ;; The word is incorrect, we have to propose a replacement.
          (flyspell-do-correct 'save
                               nil word cursor-location start end opoint)))
    (ispell-pdict-save t)))

(define-key flyspell-mode-map [(control ?\')] 'flyspell-add-word-to-dict)

;; -----------------------------------------------------------------------------
;;; init-flyspell.el ends here
