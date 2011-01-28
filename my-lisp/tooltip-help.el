;;; tooltip-help.el --- show help as tooltip
;;
;; Author: Tamas Patrovics
;; Maintainer: Henry G. Weller <hweller0@gmail.com>
;;
;; Copyright (C) 2007  Tamas Patrovics
;;
;; Created: 2007
;; Version: 0.3
;; Last-Updated: Mon Apr 20 23:21:55 2009 (+0100)
;;           By: Henry G. Weller
;;     Update #: 4
;; URL: http://www.emacswiki.org/emacs/tooltip-help.el
;; Keywords: help
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;;; Commentary:
;;
;; I thought some kind of help which is less intrusive than the
;; default one (doesn't open a new window, frame, etc.) would be
;; useful, so I made one using tooltips.
;;
;; When using in a Lisp program the tooltip is displayed without
;; osbcuring the position of the cursor and it is dismissed
;; automatically when the user continues typing, so it doesn't disrupt
;; the current window configuration like the current help does.
;;
;; How it works:
;;
;; If the cursor is ON a symbol then help is shown for that symbol.
;;
;; If the cursor is after or before a symbol then the function symbol
;; belonging to the containing sexp is used. If no such symbol is
;; found then a nearby symbol is tried.
;;
;; If the symbol has both function and variable bindings then both of
;; them are shown together in the tooltip.
;;
;; Bind to suitable key e.g.:
;;
;;   (global-set-key [f1] 'tooltip-help-mode-show)
;;
;; Support can be added by creating a function with a name like this:
;;
;;   tooltip-help-<major-mode>-handler
;;
;; Currently only Emacs lisp mode is supported.
;; So a possible more restrictive binding:
;;
;;   (define-key emacs-lisp-mode-map [f1] 'tooltip-help-mode-show)
;;
;; Another usage is to display help from functions in the help-map in a tooltip
;; To do this populate the `tooltip-help-map' by calling
;;
;;  (tooltip-help-make-help-keymap)
;;
;; and then bind to a suitable key, e.g.
;;
;;   (global-set-key [(control shift h)] tooltip-help-map)
;;
;; then for example typing "C-S-h k" will prompt for a key and display the
;; details in a tooltip.
;;
;; -----------------------------------------------------------------------------
;;
;;; Change log:
;;
;; Version 0.1
;; * Initial release by Tamas Patrovics
;;
;; Version 0.2 by Henry G. Weller
;; * Simplified the code, removing the argument highlighting which
;;   caused problems and didn't apparently work.
;; * Simplified the logic to find the nearest symbol to the point.
;; * Improved the logic for looking-up function symbols.
;; * Changed the tooltip positioning code to work more reliably on
;;   GNU Emacs 23, this may not work for 21.
;;
;; Version 0.3 by Henry G. Weller
;; * Refined the logic for positioning and testing for interaction with
;;   frame boundaries.
;; * Added customisable variables to set the menu-bar and tool-bar heights
;;   because it appears to be impossible to access or calculate this information
;;   from within Emacs.
;; * If the message is large and does not fit either above or below the point
;;   locate the top of the tooltip at the top of the screen.
;; * Changed `length' to `string-width' when calculating the width of the
;;   tooltip to ensure tabs are handled correctly.
;; * Check if the mouse-point is inside the tooltip and if it is move it to the
;;   left of tooltip to avoid it disappearing immediately.
;; * Changed the `th-' naming convention to `tooltip-help' for clarity and
;;   consistency with other packages.
;; * Changed `subseq' to `nbutlast' to avoid the need for `cl-extra'.
;;
;; Version 0.4 by Henry G. Weller
;; * Added support for displaying help from functions in the help-map in a
;;   tooltip.
;; * Renamed `tooltip-help-show' to `tooltip-help-mode-show' to distinguish
;;   the original help-for-symbol-in-mode usage from above new functionality.
;;
;; -----------------------------------------------------------------------------
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
;; -----------------------------------------------------------------------------
;;
;;; Code:

(provide 'tooltip-help)

(require 'help)
(require 'eldoc)

;; -----------------------------------------------------------------------------
;;;  Customization variables

(defgroup tooltip-help nil
  "Customization group for tooltip-help"
  :group 'help)

(defcustom tooltip-help-max-lines 25
  "The maximum number of lines shown in a tooltip.
The tooltip is truncated if necessary."
  :type 'integer
  :group 'tooltip-help)

(defcustom tooltip-help-hide-delay 600
  "Hide tooltip-help automatically after this many seconds."
  :type 'integer
  :group 'tooltip-help)

(defcustom tooltip-help-menu-bar-height 30
  "Height of Emacs window menu bar.
Unfortunately it appears that it is not possible to obtain the menu bar height
from Emacs so it must be specified here.
Correct menu bar height will help tooltip-help to display popup windows in the
appropriate position."
  :type 'integer
  :group 'tooltip-help)

(defcustom tooltip-help-tool-bar-height 38
  "Height of Emacs window tool bar.
Unfortunately it appears that it is not possible to obtain the tool bar height
from Emacs so it must be specified here.
Correct tool bar height will help tooltip-help to display popup windows in the
appropriate position."
  :type 'integer
  :group 'tooltip-help)

;;  Get rid of compiler warnings
(eval-when-compile
  (defconst tooltip-help-title-bar-height nil))

(if (eq window-system 'w32)
    (defcustom tooltip-help-title-bar-height 30
      "Height of Emacs window title bar. It mostly depends on your window
manager settings. Correct title bar height will help tooltip-help to display
popup windows in a proper position."
      :type 'integer
      :group 'tooltip-help)

  (defconst tooltip-help-title-bar-height 0
    "On Linux the title bar is not the part of the window, so we
don't have to consider its height in calculations."))

;; -----------------------------------------------------------------------------
;;;  Variables

(defvar tooltip-help-map (make-sparse-keymap)
  "Keymap for help-functions.
Populated by calling `tooltip-help-make-help-keymap'")

;; -----------------------------------------------------------------------------
;;;  User Functions

(defun tooltip-help-mode-show ()
  "If the cursor is an a symbol then show the help for that symbol in a tooltip.

If the cursor is after or before a symbol then the function symbol
belonging to the containing sexp is used. If no such symbol is
found then a nearby symbol is tried.

If the symbol has both function and variable bindings then both of
them are shown together in the tooltip."
  (interactive)
  (let ((handler
         (intern (concat "tooltip-help-" (symbol-name major-mode) "-handler"))))
    (if (functionp handler)
        (let ((help (funcall handler)))
          (if (equal help "")
              (tooltip-help-show-for-point "No help available.")
            (tooltip-help-show-for-point help)
            (message "")))
      (message "The current major mode is not supported."))))

(defun tooltip-help-make-help-keymap ()
  "Create `tooltip-help' versions of all the help functions bound to
keys in the `help-map' and bind them into the `tooltip-help-map'."
  (dolist (key help-map)
    (when (and (consp key) (functionp (cdr key)))
      (define-key tooltip-help-map (vector (car key))
        `(lambda () (interactive) (tooltip-help-function-show ',(cdr key)))))))

;; -----------------------------------------------------------------------------
;;;  Elisp Help Functions

(defun tooltip-help-lisp-interaction-mode-handler ()
  (tooltip-help-emacs-lisp-mode-handler))

(defun tooltip-help-emacs-lisp-mode-handler ()
  (let* ((symbol (intern-soft (current-word)))
         (help ""))
    (unless (null symbol)
      (if (boundp symbol)
          (setq help (tooltip-help-get-help-text
                      'describe-variable nil symbol)))
      (when (fboundp symbol)
        (let ((funhelp (tooltip-help-get-help-text
                        'describe-function nil symbol)))
          (unless (equal funhelp "")
            (unless (equal help "")
              (setq help
                    (concat help "\n\n------------------------------\n\n")))
            (setq help (concat help funhelp))))))
    help))

;; -----------------------------------------------------------------------------
;;;  Support Functions

(defun tooltip-help-function-show (help-function)
  "Call the given HELP-FUNCTION and display the message in a tooltip."
  (tooltip-help-show-for-point (tooltip-help-get-help-text help-function t)))

(defun tooltip-help-get-help-text (func &optional interactive symbol)
  (let ((pop-up-frames nil)
        (wincfg (current-window-configuration)))
    (if (get-buffer "*Help*")
        (kill-buffer "*Help*"))
    (if interactive
        (call-interactively func symbol)
      (funcall func symbol))
    (if (get-buffer "*Help*")
        (progn
          (set-window-configuration wincfg)
          (with-current-buffer "*Help*" (buffer-string)))
      "")))

(defun tooltip-help-show-for-point (msg &optional position)
  "Show tooltip MSG at point or at POSITION if given."
  (let ((lines (split-string msg "\n")))

    ;; Remove the last empty string created by trailing \n
    (when (equal (car (last lines)) "")
      (setq lines (butlast lines)))

    ;; Truncate the tooltip if it is longer than tooltip-help-max-lines
    (when (> (length lines) tooltip-help-max-lines)
      (setq lines
            (append
             (nbutlast lines (- (length lines) tooltip-help-max-lines -1))
             (list
              (concat "(Further lines not shown "
                      "due to line number limit.)"))))
      (setq msg (mapconcat (lambda (x) x) lines "\n")))

    ;; Calculate and set the tooltip pop-up position
    ;; and set the other tooltip properties
    (let* ((fcw (frame-char-width))
           (fch (frame-char-height))
           (tooltip-width
            (* fcw (1+ (min (apply 'max (mapcar 'string-width lines))
                            (car x-max-tooltip-size)))))
           (tooltip-height
            (* fch (1+ (min (length lines)
                            (cdr x-max-tooltip-size)))))
           (tooltip-pos
            (tooltip-help-calculate-position
             tooltip-width
             tooltip-height
             'above
             position))
           (tooltip-hide-delay tooltip-help-hide-delay)
           (tooltip-frame-parameters
            (append `((left . ,(car tooltip-pos))
                      (top . ,(cdr tooltip-pos)))
                    tooltip-frame-parameters))
           (old-propertize (symbol-function 'propertize))
           (mouse-pos (mouse-pixel-position)))

      ;; When the mouse point is in the frame and in the way of the tooltip
      ;; move it out of the way.
      ;; If it is outside the frame move it to 0, 0 to ensure the tooltip
      ;; relative positioning mechanism can display the tooltip
      (when (and (numberp (cadr mouse-pos))
                 (numberp (cddr mouse-pos)))
        (let* ((offset (tooltip-help-frame-offsets))
               (mpx (+ (cadr mouse-pos) (car offset)))
               (mpy (+ (cddr mouse-pos) (cdr offset)))
               (minx (- (car tooltip-pos) (/ fcw 2)))
               (miny (- (cdr tooltip-pos) (/ fch 2)))
               (maxx (+ (car tooltip-pos) tooltip-width))
               (maxy (+ (cdr tooltip-pos) tooltip-height)))
          (if (and (>= mpx minx) (<= mpx maxx) (>= mpy miny) (<= mpy maxy))
              (let ((f (selected-frame)))
                (raise-frame f)
                (set-mouse-pixel-position
                 f (- (car tooltip-pos) (car offset) fcw) (cddr mouse-pos))
                (setq mouse-pos (mouse-pixel-position))))))

      ;; The definition of `propertize' is substituted with a dummy
      ;; function temporarily, so that tooltip-show doesn't override the
      ;; properties of msg
      (fset 'propertize (lambda (string &rest properties)
                          string))

      ;; Set the tooltip offsets to position the tooltip relative to the current
      ;; mouse point position
      (unwind-protect
          (tooltip-show msg)
        (fset 'propertize old-propertize)))))

(defun tooltip-help-frame-offsets ()
  "Return the offset from the top-left point of the drawable part of the frame
to the top-left of the display."
  (let* ((fx (frame-parameter nil 'left))
         (fy (frame-parameter nil 'top))
         (fw (frame-pixel-width))
         (fh (frame-pixel-height))

         ;; Handles the case where (frame-parameter nil 'top) or
         ;; (frame-parameter nil 'left) return something like (+ -4).
         ;; This was the case where e.g. Emacs window is maximized, at
         ;; least on Windows XP. The handling code is "shamelessly
         ;; stolen" from cedet/speedbar/dframe.el
         ;;
         ;; (contributed by Andrey Grigoriev)
         (frame-left (if (not (consp fx))
                         fx
                       ;; If fx is a list, that means we grow
                       ;; from a specific edge of the display.
                       ;; Convert that to the distance from the
                       ;; left side of the display.
                       (if (eq (car fx) '-)
                           ;; A - means distance from the right edge
                           ;; of the display, or DW - fx - framewidth
                           (- (x-display-pixel-width) (car (cdr fx)) fw)
                         (car (cdr fx)))))
         (frame-top (if (not (consp fy))
                        fy
                      ;; If fy is a list, that means we grow
                      ;; from a specific edge of the display.
                      ;; Convert that to the distance from the
                      ;; left side of the display.
                      (if (eq (car fy) '-)
                          ;; A - means distance from the right edge
                          ;; of the display, or DW - pfx - framewidth
                          (- (x-display-pixel-height) (car (cdr fy)) fh)
                        (car (cdr fy))))))
    ;; Return the offsets as an (X . Y) cons cell
    (cons frame-left
          (- (+ frame-top
                tooltip-help-title-bar-height
                (if menu-bar-mode tooltip-help-menu-bar-height 0)
                (if tool-bar-mode tooltip-help-tool-bar-height 0))
             (if header-line-format (frame-char-height) 0)))))

(defun tooltip-help-calculate-position
  (width height preferred-pos &optional position)
  "Calculate pixel position of a rectangle with size WIDTH*HEIGHT at
POSITION or `point' if not given and return a cons cell (X . Y) containing
the calculated position.
Ensure the rectangle does not cover the position.
PREFERRED-POS can either be the symbol `above' or `below' indicating the
preferred position of the popup relative to point."
  (let* ((point-pos (tooltip-help-get-pixel-position position))
         (offset (tooltip-help-frame-offsets))
         (fch (frame-char-height))
         (corner-x (let ((x (+ (car point-pos) (car offset))))
                     (if (< (+ x width)
                            (display-pixel-width))
                         x
                       (- (display-pixel-width) width))))
         (corner-y (+ (cdr point-pos) (cdr offset)))
         (y-above (+ (- corner-y height)
                     ;; Add 1/2 a character height to the y position
                     ;; so that the popup does not
                     ;; cover the current line
                     (/ fch 2)))
         (y-below (+ corner-y
                     ;; Add 3/2 character heights to the y position
                     ;; so that the popup does not
                     ;; cover the current line
                     fch (/ fch 2))))

    (setq corner-y (if (eq preferred-pos 'above) y-above y-below))
    (if (< corner-y 0)
        (setq corner-y y-below))
    (if (> (+ corner-y height) (display-pixel-height))
        (setq corner-y (max y-above 0)))

    (cons corner-x corner-y)))

(defun tooltip-help-get-pixel-position (&optional position)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing the selected window.
POSITION defaults to the position of point in the selected window."
  (unless position (setq position (window-point)))
  (let ((pixel-pos (posn-x-y (posn-at-point position)))
        (edges (window-inside-pixel-edges)))
    (cons (+ (car pixel-pos) (car  edges))
          (+ (cdr pixel-pos) (cadr edges)))))

;; -----------------------------------------------------------------------------
;;; tooltip-help.el ends here
