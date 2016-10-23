;;; init-emms.el --- Initialize Emacs Multimedia System
;; -----------------------------------------------------------------------------
(use-package emms
  :init
  (progn
    (setq emms-directory "~/Emacs/Emms"
          emms-source-file-default-directory "~/Music/"
          emms-info-asynchronously t
          later-do-interval 0.0001
          emms-info-functions '(emms-info-libtag)
          emms-mode-line-format " %s "
          emms-show-format "Playing: %s"
          emms-stream-bookmarks-file (concat emms-directory "/emms-streams")
          emms-stream-default-action "play")
    ;;  Show the current track each time EMMS starts to play it
    (add-hook 'emms-player-started-hook 'emms-show)
    ;;  Highlight current line in browser
    (add-hook 'emms-browser-show-display-hook '(lambda () (hl-line-mode 1))))
  :config
  (progn
    (require 'emms-volume)
    (require 'emms-info-libtag)
    (require 'emms-streams)
    (require 'emms-stream-info)
    (emms-all)
    (emms-default-players)
    (emms-add-directory-tree emms-source-file-default-directory)
    ;;  Recenter the play-list on the current track
    (add-hook 'emms-playlist-selection-changed-hook 'my-emms-focus-on-track)
    ))

;; -----------------------------------------------------------------------------
;;; Emms buffer key-bindings
;;;  Emms play-list and track controls on the key-pad

;;; Start play
(define-key emms-browser-mode-map [return] 'emms-browser-add-tracks-and-play)

;;;  Pause play
(define-key emms-browser-mode-map [home] 'my-emms-toggle-playing)
(define-key emms-playlist-mode-map [home] 'my-emms-toggle-playing)

;;;  Stop play
(define-key emms-browser-mode-map [end] 'emms-stop)
(define-key emms-playlist-mode-map [end] 'emms-stop)

(define-key emms-browser-mode-map "p" 'emms-previous)
(define-key emms-playlist-mode-map "p" 'emms-previous)

(define-key emms-browser-mode-map "n" 'emms-next)
(define-key emms-playlist-mode-map "n" 'emms-next)

(define-key emms-browser-mode-map ">" 'emms-seek-forward)
(define-key emms-playlist-mode-map ">" 'emms-seek-forward)

(define-key emms-browser-mode-map "<" 'emms-seek-backward)
(define-key emms-playlist-mode-map "<" 'emms-seek-backward)

(define-key emms-browser-mode-map "-" 'emms-volume-lower)
(define-key emms-playlist-mode-map "-" 'emms-volume-lower)

(define-key emms-browser-mode-map "+" 'emms-volume-raise)
(define-key emms-playlist-mode-map "+" 'emms-volume-raise)

;;;  Add a new directory-tree to the browser
(define-key emms-browser-mode-map "*" 'my-emms-add-dir)
(define-key emms-playlist-mode-map "*" 'my-emms-add-dir)

;;;  Search in the browser or play-list
(define-key emms-browser-mode-map "/" 'my-emms-search)
(define-key emms-playlist-mode-map "/" 'my-emms-search)

;;; Streaming
(define-key emms-stream-mode-map (kbd "s") 'emms-stop)
(define-key emms-stream-mode-map (kbd "p") 'emms-start)

;; -----------------------------------------------------------------------------
;;; Extension functions bound to key-pad keys

(defun my-emms-toggle-playing ()
  "Emms: toggle playing the current track"
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

(defun my-emms-focus-on-track ()
  "Emms: recenter the play-list on the current track"
  (let ((w (get-buffer-window emms-playlist-buffer t)))
    (when w
      (with-selected-window w
        (emms-playlist-mode-center-current)
        (recenter '(4))))))

(defun my-emms-add-dir ()
  "Emms: add a new directory-tree to the browser"
  (interactive)
  (call-interactively 'emms-add-directory-tree)
  (emms-playlist-mode-go))

(defun my-emms-search ()
  "Emms: search in the browser or play-list"
  (interactive)
  (goto-char (point-min))
  (call-interactively 'isearch-forward))

;; -----------------------------------------------------------------------------
;;; init-emms.el ends here
