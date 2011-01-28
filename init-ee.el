;;; init-ee.el --- Initialize ee
;;;  Categorizing information manager for Emacs.
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/ee"))
(add-to-list 'Info-directory-list
             (expand-file-name "~/.emacs.d/packages/ee") t)

(setq ee-data-directory (expand-file-name "~/Emacs/EE")
      ee-view-data-directory (expand-file-name "~/Emacs/EE"))

(when (require 'ee-autoloads nil t)
  (defvar ee-map (make-sparse-keymap "EE"))
  (define-key my-map "E" ee-map)
  (define-key ee-map "i"  'ee-info)
  (define-key ee-map "b"  'ee-buffers)
  (define-key ee-map "hc" 'ee-history-command)
  (define-key ee-map "he" 'ee-history-extended-command)
  (define-key ee-map "hs" 'ee-history-shell-command)
  (define-key ee-map "u"  'ee-imenu)
  (define-key ee-map "m"  'ee-marks)
  (define-key ee-map "o"  'ee-outline)
  (define-key ee-map "pr" 'ee-programs)
  (define-key ee-map "ps" 'ee-ps)
  (define-key ee-map "t"  'ee-tags)
  (define-key ee-map "wa" 'ee-windows-add)
  (define-key ee-map "ww" 'ee-windows)
  ;(define-key global-map [(meta ?\x8a7)] 'ee-windows-and-add-current)
  (eval-after-load "ee-windows"
    '(progn
       (define-key ee-windows-keymap [(delete)]
         'ee-windows-select-and-delete-current)))
  (define-key ee-map "l"
    ;; jump to my links
    (lambda () (interactive)
      (ee-datafile nil (concat (my-home ?h) "/Emacs/bookmark.ee")))))

;; -----------------------------------------------------------------------------
;;; init-ee.el ends here
