(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-manager"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-layout"))

(require 'e2wm)
(global-set-key (kbd "M-+") 'e2wm:start-management)
