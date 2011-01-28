;;; completing-help.el --- an enhancement to `display-completion-list'

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 3.13 $
;; Keywords: local, help

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This package enhances the completion mechanism of Emacs by
;; allowing you to display information on completions.
;;
;; While entering an argument to a command which supports completion
;; in the minibuffer, by pressing `?', you see possible completions
;; of what you've entered so far. This is one of the standard features
;; of Emacs.
;;
;; With this package, you can see not only possible completions, but
;; also information on them (e.g. docstrings of elisp variables.)
;;
;; This package directly supports the following standard commands:
;;
;;     bookmark-jump            (C-x r b)
;;     customize-group          (M-x customize-group, also on the menu-bar)
;;     customize-option         (M-x customize-option, also on the menu-bar)
;;     describe-function        (C-h f)
;;     describe-variable        (C-h v)
;;     execute-extended-command (M-x)
;;     find-function            (M-x find-function)
;;     find-variable            (M-x find-variable)
;;     setenv                   (M-x setenv)
;;     where-is                 (C-h w)
;;
;; As for the other commands, this package can guess, to some extent,
;; what's relevant as help information, based on knowledge of the type
;; of argument you are entering, so your custom commands and other
;; standard commands might work right off the bat without requiring
;; any configuration on your part. You can also add support for a command
;; by defining a "group" (a property list) for it,
;; see "How this package works" section.


;;; Usage:

;; #1: Use with `M-x' (execute-extended-command)      see "(emacs)M-x"
;;
;;     When you want to invoke a command which is not bound to any
;;     key, you enter `M-x <COMMAND-NAME-YOU-WANT-TO-INVOKE>'.
;;
;;     While entering <COMMAND-NAME-YOU-WANT-TO-INVOKE> part, you
;;     can use completion. In the following description, we use
;;     `apropos' command as an example.
;;     
;;     You enter `M-x aprop<TAB>'. => Emacs completes it to "apropos".
;;     You enter <TAB>             => Emacs says "Complete, but not unique".
;;
;;     You've learned that there're some commands whose name start with
;;     "apropos", and wonder what they are.
;;     
;;     You enter `?'               => Something like the following is
;;                                    displayed in a help window:
;;
;;     ----------------------------------------------------------------
;;     Click mouse-2 on a completion to select it.
;;     In this buffer, type RET to select the completion near point.
;;     
;;     Possible completions are:
;;     apropos
;;     <C-h C-a>
;;     Show all bound symbols whose names match REGEXP.
;;     With optional prefix ARG or if `apropos-do-all' is non-nil, als$
;;     symbols and key bindings, which is a little more time-consuming$
;;     Returns list of symbols and documentation found.
;;                                             
;;     apropos-command
;;     <C-h a, menu-bar help-menu describe apropos-commands>
;;     Show commands (interactively callable functions) that match REG$
;;     With optional prefix ARG, or if `apropos-do-all' is non-nil, al$
;;     noninteractive functions.
;;     -----------------------------------------------------------------
;;
;;     You can either,
;;       * click on an item (usually the middle button of your mouse) to
;;         invoke a command,
;;       * go into the help window (M-v), then select an item (<RET>)
;;         to invoke a command, or just move around then go back into
;;         the minibuffer (C-x o),
;;       * scroll the help window (M-C-v), or
;;       * continue to enter in the minibuffer.
;;     See "(emacs)Completion" for other key bindings.


;;; Requirements:

;; Tested with FSF Emacs 20.7.2 and XEmacs 21.1.9.


;;; Compatibility:

;; This package can work with `icomplete.el' and `complete.el'
;; (both of which also enhance the completion mechanism.)


;;; Install:

;; 1: Put this file in one of the directories listed in `load-path'.
;;    You can see the contents of `load-path' by entering
;;    `M-x customize-option <RET> load-path'.
;;
;; 2: Enter `M-x byte-compile-file <RET>
;;          <DIR-YOU-PUT-THIS-FILE-IN>/completing-help.el <RET>'
;;    to speed up the execution of this package.
;;
;; 3: Put the following lines in your .emacs file.
;;
;;    (autoload 'completing-help-mode "completing-help"
;;              "Toggle a facility to display information on completions."
;;              t nil)
;;    (autoload 'turn-on-completing-help-mode "completing-help"
;;              "Turn on a facility to display information on completions."
;;              t nil)
;;    (autoload 'turn-off-completing-help-mode "completing-help"
;;              "Turn off a facility to display information of completions."
;;              t nil)
;;
;;    If you want to activate this package as you start Emacs,
;;    add the following line, too:
;;    (turn-on-completing-help-mode)
;;
;; 4: Restart Emacs or enter `M-x load-library <RET> completing-help'.
;;
;; *WARNING* You might need to load packages which redefine (not advise)
;;           any of display-completion-list, minibuffer-completion-help
;;           and completing-read before loading completing-read.el
;;           See Version 3.13's change log.


;;; Activation:

;; * Enter `M-x turn-on-completing-help-mode' to activate this package.
;; * Enter `M-x turn-on-completing-help-mode' to deactivate this package.


;;; Customization:

;; * Enter `M-x customize-group <RET> completing-help' to customize
;;   this package.
;;   You might need to enter `M-x load-library <RET> completing-help'
;;   in advance.


;;; Distribution:

;; You can find the latest version of this package at:
;; http://homepage1.nifty.com/bmonkey/emacs/elisp/completing-help.el


;;; How this package works:

;; The key idea is to give `display-completion-list' a piece of "advice"
;; to display information on completions, with "defadvice" facility.
;; See "(elisp)Completion Commands" and "(elisp)Advising Functions".
;;
;; The argument to `display-completion-list' is normally a list of
;; completions just returned by `all-completions'.
;; But it can also be a list whose element is a list of two strings,
;; which is printed as if the strings were concatenated.
;;
;; Typical control flow:
;;   a command
;;       |
;;       V
;;   completing-read (reads the argument in the minibuffer)
;;       |
;;   `?' pressed in the minibuffer
;;       |
;;       V
;;   minibuffer-completion-help
;;       |
;;       V
;;   display-completion-list
;;   (This package's advice intercepts the call and add info to the
;;    argument, and then calls the function proper.)
;;       |
;;       V
;;   Possible completions are displayed in the "*Completions*" buffer.
;;
;;
;; How to add support for a command:
;;   1: Define a "group" symbol whose value is a property list.
;;      You must supply at least :predicate and :get properties.
;;
;;      Group Properties
;;      :predicate
;;          A function taking no argument, returning non-nil if information
;;          for the argument of the current `display-completion-list' call
;;          is available, or nil.
;;      
;;          Some useful variables are as follows:
;;          `completing-help-completing-read-command' is the value of
;;          `this-command' just before the current `completing-read' call.
;;      
;;          `completing-help-completing-read-args' is a list of the
;;           arguments of the current `completing-read' call.
;;      
;;      
;;      :get
;;          A function taking 1 argument returning information string on the
;;          argument.
;;          This function is applied to each element of the argument
;;          of the current `display-completion-list' call.
;;      
;;      :info-head    (default "\n")
;;      :info-tail    (default "\n")
;;          A completion and its information is displayed almost
;;          like the following code.
;;          (insert "Completion1"  (plist-get plist :info-head)
;;                  "Information1" (plist-get plist :info-tail))
;;          *note* Information strings (e.g. "Information1") are
;;                 formalized to end with "\n".
;;
;;      
;;          The default value of both :info-head and :info-tail is "\n",
;;          and the display is as follows:
;;          -----------------------------------------------------
;;          Completion1
;;          Information1
;;      
;;          Completion2
;;          Information2
;;      
;;          -----------------------------------------------------
;;      
;;      
;;          Examples:
;;      
;;          :info-head = "", :info-tail = ""
;;          -----------------------------------------------------
;;          Completion1Information1
;;          Completion2Information2
;;          -----------------------------------------------------
;;          *WARNING* XEmacs works as if information is part of
;;                    a completion unless the character after the
;;                    completion is a space or a newline.
;;      
;;      
;;          :info-head = " = ", :info-tail = "- - - - - - - - - -\n"
;;          -----------------------------------------------------
;;          Completion1 = Information1
;;          - - - - - - - - - -
;;          Completion2 = Information2
;;          - - - - - - - - - -
;;          -----------------------------------------------------
;;
;;
;;
;;   2: Add the group symbol to the `completing-help-groups'.
;;
;;   Here is an excerpt from iman.el which defines and registers
;;   `iman-completing-help-group'.
;;
;;    iman.el ---------------------------------------------------------------
;;     (defun iman-completing-help-p ()
;;       (eq minibuffer-completion-table iman-index-obarray))
;;     
;;     (defvar iman-completing-help-group
;;       '(:predicate iman-completing-help-p
;;         :get       completing-help-user-obarray-get-info
;;         :info-head ""
;;         :info-tail "")
;;       "")
;;
;;     ;; Add `iman-completing-help-group' to `completing-help-groups'
;;     ;; This code works irrespective of whether completing-help.el is
;;     ;; already loaded or not.
;;     (if (featurep 'completing-help)
;;         (add-to-list 'completing-help-groups 'iman-completing-help-group)
;;       (add-hook 'completing-help-load-hook
;;                 #'(lambda () (add-to-list 'completing-help-groups
;;                                           'iman-completing-help-group))))
;;     ----------------------------------------------------------------------


;;; Todo:

;;  * Write documentation about the advised functions redefinition problem.
;;    See Version 3.13's change log.


;;; Change Log:

;; Version 3.13 (9 Dec 2000)
;;  * Discovered the cause of `completing-read' problem of XEmacs described
;;    in yesterdays change log.
;;    The redefinition of `completing-read' done by evi.el distributed with
;;    `lookup' package seems to corrupt the defadvise facility.
;;    The problem was solved simply to load evi.el and completing-help.el
;;    in that order in .emacs. The same problem would reappear whenever
;;    another package tries to redefine advised functions (precisely, then
;;    call the original function).

;; Version 3.11 (8 Dec 2000)
;;  * Changed not to advise `completing-read' when running on XEmacs.
;;    On XEmacs, using `defadvise' for `completing-read' causes
;;    an unpredictable error.
;;    This means that completing-help won't work for some commands
;;    on XEmacs any more.

;; Version 3.5 - 3.10
;;  * Changed to use `minibuffer-prompt-end' if available for Emacs 21

;; Version 3.4 (29 Sep 2000):
;;  * Nearly a complete rewrite
;;  * Not compatible with the previous versions.
;;  * Changed to use group symbols and property lists.
;;  * Abolished weird format options.
;;  * Wrote doc on how to add support for a command.

;; Version 2.15 (26 May 2000):
;;  * Changed the name from "bm-hcmplt.el" to "completing-help.el",
;;    suggested by Francesco Potorti`.
;;  * Fixed `completing-help-delete-from-alist' to work.
;;  * Improved `completing-help-add-alist-str-dot-str-descriptions' and
;;      `completing-help-add-alist-str-pair-descriptions' to work with
;;      multiple lines.
;;  * Fixed an installation documentation bug, (incorrect `autoload' forms)
;;    reported by Hans van Dam.
;;  * Added some declarations to suppress byte-compiler warnings.
;;  * Added support for a simple user defined obarray.

;; Version 1.41 (18 May 2000):
;;  * Added `completing-help-get-keys' to display command key bindings, which
;;      was borrowed from Ken Manheimer's icomplete.el.

;; Version 1.39 (18 May 2000):
;;  * Fixed some comment error.
;;  * Added `completing-help-load-hook'

;; Version 1.36 (16 May 2000):
;;  * Added some documentation.
;;  * Added support for `bookmark-jump', `customize-group',
;;      `customize-option', `setenv', and 2 types of simply-structured alists.
;;  * Changed to display the whole documentation of ELisp objects.
;;  * Added a customization group for the default formatting mechanism.

;; Version 1.6 (06 May 2000):
;;  * Fixed `completing-help-function-wanted-p' and
;;      `completing-help-variable-wanted-p'
;;      to work properly.

;; Version 1.5 (06 May 2000):
;;  * First public release of this package.


;;; Code:
(eval-when-compile
  (defvar obarray)
  (defvar this-command)
  (defvar standard-output)
  (defvar minibuffer-completion-table)
  (defvar minibuffer-completion-predicate))


;;; customizations

(defgroup completing-help nil
  "This package enhances the completion mechanism of Emacs by
allowing you to display information on completions.

While entering an argument to a command which supports completion
in the minibuffer, by pressing `?', you see possible completions
of what you've entered so far. This is one of the standard features
of Emacs.

With this package, you can see not only possible completions, but
also information on them (e.g. docstrings of elisp variables.)

This package directly supports the following standard commands:

    bookmark-jump            (C-x r b)
    customize-group          (M-x customize-group, also on the menu-bar)
    customize-option         (M-x customize-option, also on the menu-bar)
    describe-function        (C-h f)
    describe-variable        (C-h v)
    execute-extended-command (M-x)
    find-function            (M-x find-function)
    find-variable            (M-x find-variable)
    setenv                   (M-x setenv)
    where-is                 (C-h w)

As for the other commands, this package can guess, to some extent,
what's relevant as help information, based on knowledge of the type
of argument you are entering, so your custom commands and other
standard commands might work right off the bat without requiring
any configuration on your part. You can also add support for a command
by defining a \"group\" (a property list) for it."
  :group 'minibuffer
  :group 'help)


(defcustom completing-help-mode nil
  "Toggle a facility to display information on possible completions.
Setting this variable directly does not take effect;
use either \\[customize] or the commands `completing-help-mode',
`turn-on-completing-help-mode', and `turn-off-completing-help-mode'."
  :set        #'(lambda (symbol value) (completing-help-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type       'boolean
  :group      'completing-help
  :require    'completing-help)


(defcustom completing-help-commands
  '(minibuffer-completion-help PC-completion-help)
  "Commands which triggers the display of information on completions.
The purpose of this variable is to filter out some commands whom you want
to display completions only, that is, without additional information added
by `completing-help-mode'.
e.g. minibuffer-complete which is activated via <TAB> in the minibuffer.

The default commands are usually activated via `?' or `M-?' in the minibuffer.
You should specify commands implemented in elisp which call
`display-completion-list' inside them.
If you are an XEmacs user and want to display information when you enter
<TAB>, add `minibuffer-complete'."
  :type '(repeat function)
  :group 'completing-help)


(defun completing-help-p ()
  "Return non-nil when `completing-help-mode' is active and applicable."
  (and completing-help-mode (memq this-command completing-help-commands)))


(defvar completing-help-completion-setup-hook nil
  "Normal hook for `completing-help-mode', run via `completion-setup-hook'.
This hook is run only when `completing-help-p' returns non-nil.")

(add-hook 'completion-setup-hook
          #'(lambda ()
              (when (completing-help-p)
                (run-hooks 'completing-help-completion-setup-hook)))
          "APPEND")                     ; must append because the default
                                        ; hook clears local vars.



;;; group
(defvar completing-help-groups nil
  "List of symbols denoting groups for `completing-help-mode'.")


(defvar completing-help-default-plist
  '(:predicate  ignore
    :get        (lambda (key) (error ":get not defined"))
    :format     completing-help-format-single-column
    :info-head  "\n"
    :info-tail  "\n")
  "Default property list for `completing-help-mode'.
This plist is used whenever a group omits some properties.")



;;; completions buffer customizations

(defcustom completing-help-buffer-truncate-lines t
  "`truncate-lines' for a completions buffer."
  :type 'boolean
  :group 'completing-help)

(add-hook 'completing-help-completion-setup-hook
          #'(lambda ()
              (with-current-buffer standard-output
                (setq truncate-lines completing-help-buffer-truncate-lines))))


(defcustom completing-help-buffer-completion-face 'bold
  "Face used to highlight possible completions by `completing-help-mode'."
  :type 'face
  :group 'completing-help)



;;; mode switches

;;;###autoload
(defun completing-help-mode (&optional arg)
  "Toggle a facility to display information on completions.
With ARG, turn the mode on if ARG is positive, off otherwise."
  (interactive "P")
  (setq completing-help-mode (if (null arg)
                                 (not completing-help-mode)
                               (> (prefix-numeric-value arg) 0)))
  (when (interactive-p)
    (message "Completing help mode %s"
             (if completing-help-mode "enabled" "disabled"))))


;;;###autoload
(defun turn-on-completing-help-mode ()
  "Turn on a facility to display information on completions."
  (interactive)
  (completing-help-mode 1))


;;;###autoload
(defun turn-off-completing-help-mode ()
  "Turn off a facility to display information on completions."
  (interactive)
  (completing-help-mode -1))



;;; format info
(defvar completing-help-single-column-min-width 40
  "Minimum width to prevent `display-completion-list' from 2-column display.
The default value is slightly larger than the one hardcoded in minibuf.c.")

(eval-when-compile
  (if (featurep 'xemacs)
      ;; FSF Emacs: (split-string "A\n" "\n") => ("A")
      ;; XEmacs   : (split-string "A\n" "\n") => ("A" "")

        (defsubst completing-help-split-string (str)
          (when (and (> (length str) 0)
                     (char-equal (aref str (1- (length str))) ?\n))
            (setq str (substring str 0 (1- (length str)))))
          (split-string str "\n"))

      (defsubst completing-help-split-string (str)
        (split-string str "\n"))))


(defun completing-help-add-info (group key-list)
  "Return a list whose element is a list of two strings.
First element is an element of KEY-LIST.
Second element is information on the first element."
  (let ((get-info    (plist-get group :get))
        (format-info (plist-get group :format))
        (info-head   (plist-get group :info-head))
        (info-tail   (plist-get group :info-tail))
        result key info)
    (while key-list
      (setq key (copy-sequence (if (symbolp (car key-list))
                                   (symbol-name (car key-list))
                                 (car key-list))))
      (setq info (or (funcall get-info key) ""))
      (when (and (> (length info) 0)
                 (not (char-equal (aref info (1- (length info))) ?\n)))
        (setq info (concat info "\n")))

      (setq result (cons (list key (funcall format-info
                                            key
                                            (concat info-head info info-tail)))
                         result))
      (setq key-list (cdr key-list)))
    (reverse result)))


(defun completing-help-format-single-column (key info)
  "Return a formatted string of INFO.

Peculiarity of `display-completion-list':
 (display-completion-list
    ((\"Item1\" \"Description1\") (\"Item2\" \"Description2\")))

 * An item and its description is displayed like \"Item1Description1\".
 * (length (concat \"Item1\" \"Description\")) should be greater than
   35 .. 40 to prevent 2 \"ItemDescriptions\"s from getting crammed in
   the same line.
 * FSF Emacs:
   Descriptions should not end with \n or display gets corrupted."
  ;; Assumptions: KEY never contains a newline.
  (let ((w completing-help-single-column-min-width)
        (lines (completing-help-split-string (concat key info))))
    (substring
     (mapconcat (lambda (str)
                  (format (concat "%-" (number-to-string w) "s") str))
                lines "\n")
     (length key))))


;;; fontify completions buffer
(defun completing-help-fontify ()
  "Fontify a completions buffer."
  (with-current-buffer standard-output
    (let (start
          (end (point-min))
          (max (point-max)))
      (while (setq start (next-single-property-change end 'mouse-face))
        (setq end (next-single-property-change start 'mouse-face
                                               (current-buffer) max))
        (put-text-property start end
                           'face completing-help-buffer-completion-face)))))
  
;; Fontify if information is available, even if `this-command' is not a
;; member of `completing-help-commands'.
(add-hook 'completion-setup-hook
          #'(lambda ()
              (when (and completing-help-mode
                         (completing-help-pick-group completing-help-groups))
                (completing-help-fontify))))
;(add-hook 'completing-help-completion-setup-hook 'completing-help-fontify)



;;; group dispatch mechanism
(defun completing-help-pick-group (groups)
  "Return a plist suitable for the current `display-completion-list' call."
  (let (plist pred)
    (catch 'found
      (while groups
        (setq plist (symbol-value (car groups)))
        (when (and (setq pred (plist-get plist :predicate))
                   (funcall pred))
          (throw 'found (append plist completing-help-default-plist)))
        (setq groups (cdr groups))))))


(defadvice display-completion-list
  (around completing-help first activate preactivate)
  "Advice to add relevant information to possible completions."
  (let ((group (when (completing-help-p)
                 (completing-help-pick-group completing-help-groups))))
    (if group
        (let ((completion-highlight-first-word-only t) ; For XEmacs
              (keys (ad-get-arg 0)))
          (ad-set-arg 0 (completing-help-add-info group keys))
          ad-do-it)
      ad-do-it)))



;;; minibuffer commands in elisp
(defmacro completing-help-minibuffer-prompt-end ()
  (if (fboundp 'minibuffer-prompt-end)
      '(minibuffer-prompt-end)
    '(point-min)))

(defsubst completing-help-minibuffer-string ()
  (buffer-substring (completing-help-minibuffer-prompt-end) (point-max)))


(defadvice minibuffer-completion-help
  (around completing-help activate preactivate)
  "Advice to emulate the original C function in Emacs Lisp.
`completing-help' package needs to call `display-completion-list'
within Emacs Lisp."
  (message "Making completion list...")
  (let ((completions (all-completions
                      (completing-help-minibuffer-string)
                      minibuffer-completion-table
                      minibuffer-completion-predicate)))
    (message nil)
    (if (null completions)
    ;; Sole purpose of this ad-do-it is to display " [No completions]"
        ad-do-it
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list (sort completions 'string<))))))


;;; gather a `completing-read' call info
(defvar completing-help-completing-read-command
  nil
  "Command which is directly/indirectly calling `completing-read'.")

(defvar completing-help-completing-read-args
  nil
  "List of arguments to the current invocation of `completing-read'.")


(defadvice completing-read
  (around completing-help first activate preactivate)
  "Record the caller command and the arguments."
  (let ((completing-help-completing-read-command this-command)
        (completing-help-completing-read-args (ad-get-args 0)))
    ad-do-it))



;;; groups - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; alist support ----------------------
(defun completing-help-alist-p ()
  ""
  (let ((table minibuffer-completion-table))
    (and (consp table)
         (consp (car table))
         (or (stringp (cdar table))     ; (("a" . "abc") ..)
             (and (consp (cdar table))  ; (("a"   "abc") ..)
                  (stringp (car (cdar table))))))))

(defun completing-help-alist-get-info (key)
  ""
  (let ((val (cdr (assoc key minibuffer-completion-table))))
    (cond
     ((stringp val) val)
     ((and (consp val) (stringp (car val))) (car val))
     (t ""))))

(defvar completing-help-alist-group
  '(:predicate  completing-help-alist-p
    :get        completing-help-alist-get-info)
  "Alist group for `completing-help-mode'.")
(add-to-list 'completing-help-groups 'completing-help-alist-group)

;; test
; (completing-read "test: " '(("Item A" "Info on Item A")
;                             ("Item B" "Info on Item B")
;                             ("Item C" "Info on Item C")))
; (completing-read "test: " '(("Item A" . "Info on Item A")
;                             ("Item B" . "Info on Item B")
;                             ("Item C" . "Info on Item C")))


;;; user obarray support ----------------
(defun completing-help-user-obarray-p ()
  (let ((table minibuffer-completion-table))
    (and (vectorp table) (not (eq table obarray)))))

(defun completing-help-user-obarray-get-info (name)
  (let* ((symbol (intern name minibuffer-completion-table))
         (val    (when (boundp symbol)
                   (symbol-value symbol))))
    (cond
     ((stringp val) val)
     ((and (consp val) (stringp (car val))) (car val))
     (t ""))))

(defvar completing-help-user-obarray-group
  '(:predicate  completing-help-user-obarray-p
    :get        completing-help-user-obarray-get-info)
  "User defined obarray group for `completing-help-mode'.")

(add-to-list 'completing-help-groups 'completing-help-user-obarray-group)



;;; Emacs Lisp functions support -------------------------------------
(defvar completing-help-function-predicates
  '(byte-code-function-p commandp functionp subrp fboundp)
  "Set of predicates identifying various kinds of Emacs Lisp functions.")


(defun completing-help-function-p ()
  "Return t if the current minibuffer session seems to want a ELisp function."
  (and (eq minibuffer-completion-table obarray)
       (memq minibuffer-completion-predicate
             completing-help-function-predicates)))


(defun completing-help-get-keys (func-name)
  "Return strings naming keys bound to `func-name', or nil if none.
I borrowed icomplete-get-keys from Ken Manheimer's icomplete.el.
If we're in the minibuffer, check the keymap of (other-buffer),
otherwise check that of (current-buffer)."
  (if (commandp func-name)
    (save-excursion
      (let* ((sym (intern func-name))
             (buf (if (active-minibuffer-window)
                      (if (featurep 'xemacs)
                          (other-buffer nil nil "VISIBLE-OK")
                        (other-buffer nil "VISIBLE-OK"))
                    (current-buffer)))
             (map (save-excursion (set-buffer buf) (current-local-map)))
             (keys (where-is-internal sym map)))
        (if keys
            (concat "<"
                    (mapconcat 'key-description
                               (sort keys
                                     #'(lambda (x y)
                                         (< (length x) (length y))))
                               ", ")
                    ">\n"))))))


(defun completing-help-function-get-info (name)
  (concat (completing-help-get-keys name) (documentation (intern name))))

  
(defvar completing-help-function-group
  '(:predicate  completing-help-function-p
    :get        completing-help-function-get-info)
  "Elisp function group for `completing-help-mode'.")

(add-to-list 'completing-help-groups 'completing-help-function-group)

;; test
; (call-interactively 'describe-function)
; (call-interactively 'execute-extended-command)
; (call-interactively 'find-function)
; (call-interactively 'where-is)


;;; Emacs Lisp variables support -------------------------------------
(defvar completing-help-variable-predicates
  '(user-variable-p boundp)
  "Set of predicates identifying various kinds of Emacs Lisp variables.")


(defvar completing-help-variable-commands
  '(customize-option     customize-option-other-window
    customize-variable   customize-variable-other-window))


(defun completing-help-variable-p ()
  "Return t if the current minibuffer session seems to want a ELisp variable."
  (and (eq minibuffer-completion-table obarray)
       (or (memq minibuffer-completion-predicate
                 completing-help-variable-predicates)
           (memq completing-help-completing-read-command
                 completing-help-variable-commands))))

(defun completing-help-variable-get-info (name)
  (or (documentation-property (intern name) 'variable-documentation)
      ""))
  
(defvar completing-help-variable-group
  '(:predicate  completing-help-variable-p
    :get        completing-help-variable-get-info)
  "Elisp variable group for `completing-help-mode'.")

(add-to-list 'completing-help-groups 'completing-help-variable-group)

;; test
; M-x customize-option
; (call-interactively 'describe-variable)
; (call-interactively 'find-variable)


;;; Emacs Lisp custom groups support ---------------------------------
(defvar completing-help-custom-group-commands
  '(customize-group customize-group-other-window))


(defun completing-help-custom-group-p ()
  ""
  (memq completing-help-completing-read-command
        completing-help-custom-group-commands))

(defun completing-help-custom-group-get-info (name)
  ""
  (or (get (intern name) 'group-documentation) ""))
  
(defvar completing-help-custom-group
  '(:predicate  completing-help-custom-group-p
    :get        completing-help-custom-group-get-info)
  "Elisp customization group for `completing-help-mode'.")

(add-to-list 'completing-help-groups 'completing-help-custom-group)

;; test
; M-x 'customize-group

  
;;; Emacs Lisp custom faces support -----------------------------------
(defvar completing-help-custom-face-predicates
  '(custom-facep find-face)
  "")

(defun completing-help-custom-face-p ()
  ""
  (or (memq minibuffer-completion-predicate
            completing-help-custom-face-predicates)
      (eq (nth 5 completing-help-completing-read-args) 'face-history)))

(defmacro completing-help-custom-face-get-info-1 (name)
  (if (featurep 'xemacs)
      `(face-property (find-face (intern ,name)) 'doc-string)
    `(get (intern ,name) 'face-documentation)))

(defun completing-help-custom-face-get-info (name)
  (or (completing-help-custom-face-get-info-1 name) ""))

(defvar completing-help-custom-face-group
  '(:predicate  completing-help-custom-face-p
    :get        completing-help-custom-face-get-info)
  "Face group for `completing-help-mode'.")

(add-to-list 'completing-help-groups 'completing-help-custom-face-group)

;; test
; (call-interactively 'customize-face)


;;; bookmark.el support ----------------------------------------------
(eval-when-compile
  (defvar bookmark-alist)
  (autoload 'bookmark-get-filename   "bookmark")
  (autoload 'bookmark-get-annotation "bookmark"))

(defun completing-help-bookmark-p ()
  ""
  (and (boundp 'bookmark-alist)
       (eq minibuffer-completion-table bookmark-alist)))

(defun completing-help-bookmark-get-info (name)
  (concat (bookmark-get-filename name)
          "\n"
          (bookmark-get-annotation name)))

(defvar completing-help-bookmark-group
  '(:predicate  completing-help-bookmark-p
    :get        completing-help-bookmark-get-info)
  "Bookmark group for `completing-help-mode'.")

(add-to-list 'completing-help-groups 'completing-help-bookmark-group)

;; test
; (call-interactively 'bookmark-jump)


;;; env.el support ---------------------------------------------------
(defun completing-help-environment-variable-p ()
  (eq completing-help-completing-read-command 'setenv))

(defun completing-help-environment-variable-get-info (name)
  (getenv name))

(defvar completing-help-environment-variable-group
  `(:predicate completing-help-environment-variable-p
    :get       completing-help-environment-variable-get-info
    :info-head ,(concat (when (featurep 'xemacs) " ") ; maybe a bug in XEmacs
                        "=")
    :info-tail "")
  "Environment variable group for `completing-help-mode'.")

(add-to-list 'completing-help-groups
             'completing-help-environment-variable-group)

;; test
; M-x setenv


;;; filename support ------------------------------------------------
; NOT WORK
; (defun completing-help-filename-p ()
;   (eq minibuffer-completion-table 'read-file-name-internal))

; (defun completing-help-filename-get-info (name)
;   (shell-command-to-string
;    (concat "ls -ld "
;            (expand-file-name name (file-name-directory (buffer-string))))))

; (defvar completing-help-filename-group
;   `(:predicate completing-help-filename-p
;     :get       completing-help-filename-get-info)
;   "")

; (add-to-list 'completing-help-groups 'completing-help-filename-group)



;;; Hook
(defvar completing-help-load-hook nil
  "Hook to run at the end of loading completing-help.")

(provide 'completing-help)
(run-hooks 'completing-help-load-hook)


;;; completing-help.el ends here
