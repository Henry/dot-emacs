(add-to-list
 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-manager"))
(add-to-list
 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-layout"))

(require 'e2wm)
(require 'windata)
;(require 'tree-mode)
;(require 'dirtree)
(autoload 'dirtree "tree-mode" "tree-mode" t)
(autoload 'dirtree "dirtree" "dirtree" t)

(global-set-key (kbd "M-+") 'e2wm:start-management)
