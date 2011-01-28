;;; init-iswitchb.el --- Initialize incremental buffer selection
;; -----------------------------------------------------------------------------

(require 'iswitchb)
(iswitchb-default-keybindings)

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match) ; obvious behaviour
          ("<left>"  . iswitchb-prev-match) ; "
          ("<up>"    . iswitchb-prev-match) ; match icicles behaviour
          ("<down>"  . iswitchb-next-match) ; "
          )))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;;  Fontify the buffer name in the buffer selection list based on the mode-name
;;  of the buffer.
(require 'iswitchb-highlight)

;;;  Set the colours list for the mode buffers
(setq saint/iswitchb-highlight-modes-alist
      '(("Dired"       . 1)
        ("Org"     . 2)
        ("Fundamental" . 3)
        ("Completions" . 3)))

;; -----------------------------------------------------------------------------
;;; init-iswitchb.el ends here
