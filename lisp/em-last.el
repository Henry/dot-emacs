;;; em-last.el --- insert arguments from previous commands

;; Copyright (C) 2002, 2003  Romain Francoise <romain@orebokech.com>

;; Author: Romain Francoise <romain@orebokech.com>
;; Keywords: eshell, convenience
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This Eshell module provides the `eshell-insert-previous-argument'
;; function, which is similar to the bash command `yank-last-arg'.  It
;; is a quick way to insert the last argument to the previous command,
;; or if called with a numeric argument, insert its nth last argument.
;; Successive calls insert arguments from previous commands from the
;; Eshell history, in turn.  It is bound to `C-c .', or `M-.' if the
;; em-rebind module is in use.
;;
;; This code has been greatly inspired by similar features/functions in
;; comint, so credit should go to all comint authors.  Some of this
;; code is directly copied from comint, I just changed variable names
;; or functions to refer to their comint equivalents.

;; This module has been proved to work with:
;; - GNU Emacs 21.4 (and its bundled version of Eshell)

;; Installation instructions:
;; 1. Put this file somewhere in your load-path.
;; 2. Byte-compile it.
;; 3. Add eshell-last to your eshell-modules-list variable.
;; 4. Launch Eshell, and enjoy.

;;; Code:

(defgroup eshell-last nil
  "This Eshell module provides the `eshell-insert-previous-argument'
function, which is similar to the bash command `yank-last-arg'.  It is a
quick way to insert the last argument to the previous command, or if
called with a numeric argument, insert its nth last argument.
Successive calls insert arguments from previous commands from the Eshell
history, in turn.  It is bound to `C-c .', or `M-.' if the em-rebind
module is in use."
  :tag "Inserting previous arguments."
  :group 'eshell-module)

;; User variables:

(defcustom eshell-last-load-hook '(eshell-last-initialize)
  "*A list of functions to call when loading `eshell-last'."
  :type 'hook
  :group 'eshell-last)

;; Internal variables:

(defvar eshell-insert-previous-argument-last-start-pos nil)
(make-variable-buffer-local 'eshell-insert-previous-argument-last-start-pos)
(defvar eshell-insert-previous-argument-last-index nil)
(make-variable-buffer-local 'eshell-insert-previous-argument-last-index)

;; Functions:

(defun eshell-last-initialize ()
  "Initialize the last arguments module."
  (unless (featurep 'em-hist)
    (load "em-hist"))
  (if (eshell-using-module 'eshell-rebind)
      (define-key eshell-mode-map [(meta ?.)]
        'eshell-insert-previous-argument)
    (define-key eshell-mode-map [(control c) (?.)]
      'eshell-insert-previous-argument)))

;; This function is a "port" to Eshell of its equivalent in
;; comint.el. The comint function was written by Miles Bader.
(defun eshell-insert-previous-argument (index)
  "Insert the INDEXth previous argument from the previous eshell command.
This command is similar to `M-.' in bash."
  (interactive "P")
  ;; If we have an argument, convert it to a number
  (unless (null index)
    (setq index (prefix-numeric-value index)))
  (cond ((eq last-command this-command)
         ;; We're called again, so delete what we inserted before
         ;; and import the arg index from the previous call
         (delete-region eshell-insert-previous-argument-last-start-pos
                        (point))
         (setq index eshell-insert-previous-argument-last-index))
        (t
         ;; First time we're called, initialize stuff
         (setq eshell-history-index nil)
         (setq eshell-insert-previous-argument-last-index index)
         (when (null eshell-insert-previous-argument-last-start-pos)
           (setq eshell-insert-previous-argument-last-start-pos 
                 (make-marker)))))
  ;; If we're not in the input zone, move
  (if (<= (point) eshell-last-output-end)
      (eshell-bol))
  ;; Remember the beginning of what we're going to insert
  (set-marker eshell-insert-previous-argument-last-start-pos (point))
  ;; Insert our argument
  (insert (eshell-arguments (eshell-previous-input-string 0)
                            index index))
  ;; Make next invocation return arg from previous input
  (setq eshell-history-index (1+ (or eshell-history-index 0)))
  ;; Add a space, unless we're at the end of the line
  (unless (eolp)
    (just-one-space)))


;; This is a port of `comint-arguments'.
(defun eshell-arguments (string nth mth)
  "Return from STRING the NTH to MTH arguments.
NTH and/or MTH can be nil, which means the last argument.
Returned arguments are separated by single spaces.
We assume whitespace separates arguments, except within quotes
and except for a space or tab that immediately follows a backslash.
Also, a run of one or more of a single character
in `eshell-delimiter-argument-list' is a separate argument.
Argument 0 is the command name."
  ;; The first line handles ordinary characters and backslash-sequences
  ;; (except with w32 msdos-like shells, where backslashes are valid).
  ;; The second matches "-quoted strings.
  ;; The third matches '-quoted strings.
  ;; The fourth matches `-quoted strings.
  ;; This seems to fit the syntax of BASH 2.0.
  (let* ((first "[^ \n\t\"'`\\]+\\|\\\\[\"'`\\ \t]+\\|")
	 (argpart (concat first
			  "\\(\"\\([^\"\\]\\|\\\\.\\)*\"\\|\
'[^']*'\\|\
`[^`]*`\\)"))
	 (args ()) (pos 0)
	 (count 0)
	 beg str value quotes)
    ;; Build a list of all the args until we have as many as we want.
    (while (and (or (null mth) (<= count mth))
		(string-match argpart string pos))
      (if (and beg (= pos (match-beginning 0)))
	  ;; It's contiguous, part of the same arg.
	  (setq pos (match-end 0)
		quotes (or quotes (match-beginning 1)))
	;; It's a new separate arg.
	(if beg
	    ;; Put the previous arg, if there was one, onto ARGS.
	    (setq str (substring string beg pos)
		  args (if quotes (cons str args)
			 (nconc (eshell-delim-arg str) args))
		  count (1+ count)))
	(setq quotes (match-beginning 1))
	(setq beg (match-beginning 0))
	(setq pos (match-end 0))))
    (if beg
	(setq str (substring string beg pos)
	      args (if quotes (cons str args)
		     (nconc (eshell-delim-arg str) args))
	      count (1+ count)))
    (let ((n (or nth (1- count)))
	  (m (if mth (1- (- count mth)) 0)))
      (mapconcat
       (function (lambda (a) a)) (nthcdr n (nreverse (nthcdr m args))) " "))))

;; eshell-delim-arg: ported from comint-delim-arg.

;; Return a list of arguments from ARG.  Break it up at the
;; delimiters in eshell-delimiter-argument-list.  Returned list is
;; backwards.
(defun eshell-delim-arg (arg)
  (if (null eshell-delimiter-argument-list)
      (list arg)
    (let ((args nil)
	  (pos 0)
	  (len (length arg)))
      (while (< pos len)
	(let ((char (aref arg pos))
	      (start pos))
	  (if (memq char eshell-delimiter-argument-list)
	      (while (and (< pos len) (eq (aref arg pos) char))
		(setq pos (1+ pos)))
	    (while (and (< pos len)
			(not (memq (aref arg pos)
				   eshell-delimiter-argument-list)))
	      (setq pos (1+ pos))))
	  (setq args (cons (substring arg start pos) args))))
      args)))

(provide 'em-last)
;;; em-last.el ends here
