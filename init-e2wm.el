(add-to-list
 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-manager"))
(add-to-list
 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-layout"))

(require 'e2wm)
(require 'windata)
(autoload 'e2wm:dp-code2 "e2wm-code2" "e2wm-code2" t)

(global-set-key (kbd "M-+") 'e2wm:dp-code2)
