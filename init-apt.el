;;; init-apt.el --- Initialize the apt package
;;;  Allows easy running of Debian apt-related software
;; -----------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/apt-el"))

(load "apt-mode")

;; -----------------------------------------------------------------------------
;;; init-apt.el ends here
