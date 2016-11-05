;;; init-movement.el --- Cursor movement configuration
;; -----------------------------------------------------------------------------

;;; Define acceleration of the cursor movement commands.
(require 'accelerate)
(accelerate previous-line 2)
(accelerate next-line 2)
(accelerate right-char 2)
(accelerate left-char 2)
(accelerate backward-char 2)
(accelerate forward-char 2)
(accelerate up-one 2)
(accelerate down-one 2)
(accelerate dired-previous-line 2)
(accelerate dired-next-line 2)
(accelerate speedbar-prev 2)
(accelerate speedbar-next 2)

;;; Define double key-bindings for home and end keys
;;  to move the cursor to the beginning/end of the line on first press and
;;  buffer on second press
(use-package sequential-command
  :config
  (require 'sequential-command-config)
  :bind (("<home>" . seq-home)
         ("<end>" . seq-end)))

;; -----------------------------------------------------------------------------
;;; init-movement.el ends here
