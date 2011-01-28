;;; browse-kill-ring+.el --- Extensions to browse-kill-ring.el
;;
;; Filename: browse-kill-ring+.el
;; Description: Extensions to browse-kill-ring.el
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2008, Drew Adams, all rights reserved.
;; Created: Tue May 25 16:35:05 2004
;; Version: 21.0
;; Last-Updated: Tue Jan 01 13:44:45 2008 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 29
;; URL: http://www.emacswiki.org/cgi-bin/wiki/browse-kill-ring+.el
;; Keywords: convenience
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `browse-kill-ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Extensions to browse-kill-ring.el
;;
;;
;;  New command defined here:
;;
;;    `toggle-browse-kill-ring-display-style' - Toggle the display style
;;
;;
;;  New key binding defined here: `t' in `browse-kill-ring-mode-map' is
;;                                `toggle-browse-kill-ring-display-style'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case
(require 'browse-kill-ring)

;;;;;;;;;;;;;;;;;;;;;


(browse-kill-ring-default-keybindings)

(defun toggle-browse-kill-ring-display-style ()
  "Toggle browse-kill-ring-display-style between `separated' and `one-line'."
  (interactive)
  (setq browse-kill-ring-display-style
        (case browse-kill-ring-display-style
          (separated 'one-line)
          (otherwise 'separated)))
  (browse-kill-ring-update)
  (message "browse-kill-ring-display-style is now %s" browse-kill-ring-display-style))

(define-key browse-kill-ring-mode-map (kbd "t") 'toggle-browse-kill-ring-display-style)

;;;;;;;;;;;;;;;;;;;;;

(provide 'browse-kill-ring+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; browse-kill-ring+.el ends here
