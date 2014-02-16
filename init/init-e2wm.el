(add-to-list
 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-manager"))
(add-to-list
 'load-path (expand-file-name "~/.emacs.d/packages/emacs-window-layout"))

(require 'e2wm)
(autoload 'e2wm:start-management "e2wm-code2" "e2wm-code2" t)

(global-set-key (kbd "M-+")
                '(lambda ()
                   (interactive)
                   (require 'e2wm-code2)
                   (e2wm:start-management '(code2))))
