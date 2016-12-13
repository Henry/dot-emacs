;;; init-server.el --- client-server operation
;; -----------------------------------------------------------------------------

;;; Start client server
(defconst remote-display (getenv "DISPLAY")
  "Holds the `DISPLAY' environment variable which is set to the correct
value by the `-server' option for eemacs")

(defun command-line-server (switch)
  ;; Get the name of the server
  (setq server-name (pop command-line-args-left))

  (add-hook 'server-switch-hook
            (lambda nil
              (let ((server-buf (current-buffer)))
                (bury-buffer)
                (switch-to-buffer-other-frame server-buf))))

  ;; Ensure that all frames are closed when emacs exits
  ;; This causes magit commit to delete the frame
  ;;(add-hook 'server-done-hook (lambda () (delete-frame)))

  (setq remote-display (getenv "DISPLAY"))
  (server-start))

(add-to-list 'command-switch-alist '("-server" . command-line-server))

;; -----------------------------------------------------------------------------
;;; init-server.el ends here
