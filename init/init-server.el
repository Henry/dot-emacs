;;; init-server.el --- client-server operation
;; -----------------------------------------------------------------------------

;;; Start client server
(defconst remote-display (getenv "DISPLAY")
  "Holds the `DISPLAY' environment variable which is set to the correct
value by the `-server' option for eemacs")

;;;  emacsclient core-dumps whet TCP is used
;;(setq server-use-tcp t
;;      server-host "10.0.0.37")

(defun command-line-server (switch)
  ;; Get the name of the server
  (setq server-name (pop command-line-args-left))

  (add-hook 'server-switch-hook
            (lambda nil
              (let ((server-buf (current-buffer)))
                (bury-buffer)
                (switch-to-buffer-other-frame server-buf))))

  ;; Ensure that all frames are closed when emacs exits
  ;;(add-hook 'server-done-hook 'delete-frame)
  ;;(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))
  (custom-set-variables '(server-kill-new-buffers t))
  (add-hook 'server-done-hook (lambda () (delete-frame)))
  (setq remote-display (getenv "DISPLAY"))
  (server-start))

;;(defun command-line-reset-display (switch)
;;  (setenv "DISPLAY" (car command-line-args-left))
;;  (setq command-line-args-left (cdr command-line-args-left)))

(add-to-list 'command-switch-alist '("-server" . command-line-server))

;; -----------------------------------------------------------------------------
;;; init-server.el ends here
