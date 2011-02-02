;;; org-eshell.el - Support for links to working directories in eshell
;; Copyright (C) 2011 Free Software Foundation, Inc.
;;
;; Author: Konrad Hinsen <konrad.hinsen AT fastmail.net>
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.
;;
;; Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'eshell)
(require 'esh-mode)

(org-add-link-type "eshell" 'org-eshell-open)
(add-hook 'org-store-link-functions 'org-eshell-store-link)

(defun org-eshell-open (link)
  "Switch to am eshell buffer and execute a command line.
   The link can be just a command line (executed in the default
   eshell buffer) or a command line prefixed by a buffer name
   followed by a colon."
  (let* ((buffer-and-command
          (if (string-match "\\([A-Za-z0-9-+*]+\\):\\(.*\\)" link)
            (list (match-string 1 link)
                  (match-string 2 link))
            (list eshell-buffer-name link)))
         (eshell-buffer-name (car buffer-and-command))
         (command (cadr buffer-and-command)))
     (if (get-buffer eshell-buffer-name)
       (switch-to-buffer eshell-buffer-name)
       (eshell))
     (end-of-buffer)
     (eshell-kill-input)
     (insert command)
     (eshell-send-input)))

(defun org-eshell-store-link ()
  "Store a link that, when opened, switches back to the current eshell buffer
   and the current working directory."
  (when (eq major-mode 'eshell-mode)
    (let* ((command (concat "cd " dired-directory))
           (link  (concat (buffer-name) ":" command)))
      (org-store-link-props
       :link (org-make-link "eshell:" link)
       :description command))))
  

(provide 'org-eshell)

;;; org-eshell.el ends here
