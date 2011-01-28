;;; finder+.el --- Extensions to standard library finder.el
;;
;; Filename: finder+.el
;; Description: Extensions to standard library finder.el
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2008, Drew Adams, all rights reserved.
;; Created: Wed Mar 12 10:00:16 2008 (Pacific Standard Time)
;; Version: 21.0
;; Last-Updated: Sat Mar 22 18:58:36 2008 (Pacific Daylight Time)
;;           By: dradams
;;     Update #: 67
;; URL: http://www.emacswiki.org/cgi-bin/wiki/finder+.el
;; Keywords: help
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `emacsbug', `finder', `finder-inf', `lisp-mnt', `sendmail'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to standard library finder.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/03/22 dadams
;;     finder-mode: Don't also use lisp font-locking.
;; 2008/03/21 dadams
;;     Require finder.el.
;;     copy-syntax-table -> make-syntax-table.
;;     Added: finder-font-lock-keywords.
;;     finder-exit: Removed unbound var window.
;;     Set font-lock-defaults instead of using font-lock-add-keywords.
;; 2008/03/14 dadams
;;     Added redefinition of finder-mode.  Added: finder-mode-syntax-table.
;; 2008/03/12 dadams
;;     Created, with redefinition of finder-exit.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'finder)

;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar finder-mode-syntax-table
  (let ((st (make-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\; ".   " st)
    st)
  "Syntax table used while in `finder-mode'.")

(defvar finder-font-lock-keywords
  '(("`\\([^']+\\)'" 1 font-lock-constant-face prepend))
  "Font-lock keywords for Finder mode.")

;; REPLACES ORIGINAL in `finder.el'.
;; Wraps `delete-window' in `condition-case'.
;; Kills also buffer `*Finder-package*'.
;;
(defun finder-exit ()
  "Exit Finder mode.
Delete the window and kill the buffer."
  (interactive)
  (condition-case nil (delete-window) (error nil))
  (when (get-buffer "*Finder*") (kill-buffer "*Finder*"))
  (when (get-buffer "*Finder-package*") (kill-buffer "*Finder-package*"))
  (when (get-buffer "*Finder Category*") (kill-buffer "*Finder Category*")))


;; REPLACES ORIGINAL in `finder.el'.
;; Uses `finder-mode-syntax-table', not `emacs-lisp-mode-syntax-table'.
;; Adds font-lock keywords for `...' highlighting.
;;
(defun finder-mode ()
  "Major mode for browsing package documentation.
\\<finder-mode-map>
\\[finder-select]	more help for the item on the current line
\\[finder-exit]	exit Finder mode and kill the Finder buffer."
  (interactive)
  (kill-all-local-variables)
  (use-local-map finder-mode-map)
  (set-syntax-table finder-mode-syntax-table)
  (setq font-lock-defaults '(finder-font-lock-keywords nil nil
                             (("+-*/.<>=!?$%_&~^:@" . "w")) nil))
  (setq mode-name "Finder")
  (setq major-mode 'finder-mode)
  (set (make-local-variable 'finder-headmark) nil)
  (run-mode-hooks 'finder-mode-hook))

;;;;;;;;;;;;;;;;;;;

(provide 'finder+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finder+.el ends here
