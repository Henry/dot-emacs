;;; tooltip-help.el --- show help as tooltip
;;
;; Author: Tamas Patrovics
;; Maintainer: Henry G. Weller <hweller0@gmail.com>
;;
;; Copyright (C) 2007  Tamas Patrovics
;;
;; Created: 2007
;; Version: 0.2
;; Last-Updated: Sat Apr 11 14:47:37 2009 (+0100)
;;           By: Henry G. Weller
;;     Update #: 2
;; URL: http://www.emacswiki.org/emacs/completion-selection.el
;; Keywords: completion, ui, user interface
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;;; Commentary:
;;
;; F1 is a useless key on my keyboard, because I don't use it in
;; Emacs, so I thought I bound some kind of help function on it.
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
;;   (global-set-key [f1] 'th-show-help)
;;
;; Support can be added by creating a function with a name like this:
;;
;;   th-<major-mode>-handler
;;
;; Currently only Emacs lisp mode is supported.
;; So a possible more restrictive binding:
;;
;;   (define-key emacs-lisp-mode-map [f1] 'th-show-help)
;;
;; Tested on Gnu 23.
;; -----------------------------------------------------------------------------
;;
;;; Change log:
;;
;; Version 0.1
;; * Initial release by Tamas Patrovics
;; Version 0.2 by Henry G. Weller
;; * Simplified the code, removing the argument highlighting which
;;   caused problems and didn't apparently work.
;; * Simplified the logic to find the nearest symbol to the point.
;; * Improved the logic for looking-up function symbols.
;; * Changed the tooltip positioning code to work more reliably on
;;   GNU Emacs 23, this may not work for 21.
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

(defcustom th-max-tooltip-lines 25
  "The maximum number of lines shown in a tooltip.
The tooltip is truncated if necessary."
  :type 'integer
  :group 'th)

;; -----------------------------------------------------------------------------
;;;  User Functions

(defun th-show-help ()
  (interactive)
  (let ((handler (intern (concat "th-" (symbol-name major-mode) "-handler"))))
    (if (functionp handler)
        (let ((help (funcall handler)))
          (if (equal help "")
              (th-show-tooltip-for-point "No help available.")
            (th-show-tooltip-for-point help)
            (message "")))
      (message "The current major mode is not supported."))))

;; -----------------------------------------------------------------------------
;;;  Elisp Help Functions

(defun th-lisp-interaction-mode-handler ()
  (th-emacs-lisp-mode-handler))

(defun th-emacs-lisp-mode-handler ()
  (let* ((symbol (intern-soft (current-word)))
         (help ""))
    (unless (null symbol)
      (if (boundp symbol)
          (setq help (th-elisp-get-help-text 'describe-variable symbol)))
      (when (fboundp symbol)
        (let ((funhelp (th-elisp-get-help-text 'describe-function symbol)))
          (unless (equal funhelp "")
            (unless (equal help "")
              (setq help
                    (concat help "\n\n------------------------------\n\n")))
            (setq help (concat help funhelp))))))
    help))

(defun th-elisp-get-help-text (func symbol)
  (let ((pop-up-frames nil)
        (wincfg (current-window-configuration)))
    (if (get-buffer "*Help*")
        (kill-buffer "*Help*"))
    (funcall func symbol)
    (if (get-buffer "*Help*")
        (progn
          (set-window-configuration wincfg)
          (with-current-buffer "*Help*" (buffer-string)))
      "")))

;; -----------------------------------------------------------------------------
;;;  Support Functions

(defun th-show-tooltip-for-point (msg &optional position)
  "Show tooltip MSG at point or at POSITION if given."
  (let ((lines (split-string msg "\n")))
    (when (> (length lines) th-max-tooltip-lines)
      (setq lines
            (append
             (subseq lines 0 (1- th-max-tooltip-lines))
             (list
              (concat "(Further lines not shown "
                      "due to line number limit.)"))))
      (setq msg (mapconcat (lambda (x) x) lines "\n")))

    (let* ((tooltip-width
            (* (frame-char-width)
               (1+ (apply 'max (mapcar 'length lines)))))
           (tooltip-height
            (* (frame-char-height)
               (1+ (min (length lines) (cdr x-max-tooltip-size)))))
           (tooltip-pos
            (th-calculate-popup-position
             tooltip-width
             tooltip-height
             'above
             position))
           (tooltip-hide-delay 600)
           (old-propertize (symbol-function 'propertize))
           (mouse-pos (mouse-pixel-position)))

      ;; When the mouse point is in the frame and in the way of the tooltip
      ;; move it out of the way.
      ;; If it is outside the frame move it to 0, 0 to ensure the tooltip
      ;; relative positioning mechanism can display the tooltip
      (if (and (numberp (cadr mouse-pos))
               (numberp (cddr mouse-pos)))
          (let* ((mpx (cadr mouse-pos))
                 (mpy (+ (cddr mouse-pos)))
                 (minx (car tooltip-pos))
                 (miny (cdr tooltip-pos))
                 (maxx (+ minx tooltip-width))
                 (maxy (+ miny tooltip-height)))
            (if (and (>= mpx minx) (<= mpx maxx) (>= mpy miny) (<= mpy maxy))
                (let ((f (selected-frame)))
                  (raise-frame f)
                  (set-mouse-pixel-position f (- (car tooltip-pos) 10) mpy)
                  (setq mouse-pos (mouse-pixel-position)))))
        (let ((f (selected-frame)))
          (raise-frame f)
          (set-mouse-pixel-position f 0 0)
          (setq mouse-pos (mouse-pixel-position))))

      ;; The definition of `propertize' is substituted with a dummy
      ;; function temporarily, so that tooltip-show doesn't override the
      ;; properties of msg
      (fset 'propertize (lambda (string &rest properties)
                          string))

      ;; Set the tooltip offsets to position the tooltip relative to the current
      ;; mouse point position
      (let ((tooltip-x-offset (- (car tooltip-pos) (cadr mouse-pos)))
            (tooltip-y-offset (- (cdr tooltip-pos) (cddr mouse-pos))))
        (unwind-protect
            (tooltip-show msg)
          (fset 'propertize old-propertize))))))

(defun th-calculate-popup-position
  (width height preferred-pos &optional position)
  "Calculate pixel position of a rectangle with size WIDTH*HEIGHT at
POSITION or point if they are not given and return a list (X . Y) containing
the calculated position.
Ensure the rectangle does not cover the position.
PREFERRED-POS can either be the symbol `above' or `below' indicating the
preferred position of the popup relative to point."

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
                        (car (cdr fy)))))

         (point-pos (completion-frame-posn-at-point position))

         (corner-x (let ((x (+ (car point-pos) frame-left)))
                     (if (< (+ x width)
                            (display-pixel-width))
                         x
                       (- (display-pixel-width) width))))

         (real-y-offset (+ (cdr point-pos) frame-top))

         (y-above (+ (- real-y-offset height)
                  ;; Add a character height to the height
                  ;; so that the popup does not
                  ;; cover the current line
                  (frame-char-height)))

         (y-below (+ real-y-offset
                     ;; Add a character height to the height
                     ;; so that the popup does not
                     ;; cover the current line
                     (frame-char-height)))

         (corner-y (if (eq preferred-pos 'above)
                       y-above
                     y-below)))

    (if (< corner-y 0)
        (setq corner-y y-below))

    (if (> (+ corner-y height)
           (display-pixel-height))
        (setq corner-y y-above))

    (cons (- corner-x frame-left) (- corner-y frame-top))))

;; -----------------------------------------------------------------------------
;;;  Completion-UI Functions (On loan)

(defun completion-frame-posn-at-point (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window.

DX and DY specify optional offsets from the top left of the glyph.

See also `completion-window-posn-at-point' and
`completion-window-inside-posn-at-point'."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))

  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges) (or dx 0))
          (+ (cdr x-y) (cadr edges) (or dy 0)))))

;; -----------------------------------------------------------------------------
;;; tooltip-help.el ends here
