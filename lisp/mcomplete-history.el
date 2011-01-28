;;; mcomplete-history.el ---  minibuffer completion from history
;; -*- Mode: Emacs-Lisp -*-

;; $Id: mcomplete-history.el,v 2.3 2007/12/15 13:10:12 akihisa Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:
;; 1. Donwload mcomplete.el and mcomplete-history.el
;;       from http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el
;;       and http://www.bookshelf.jp/elc/mcomplete-history.el
;; 2. Put mcomplete.el and mcomplete-history.el into load-path'ed
;; directory, and byte compile it if desired.
;; 3. Put the following expression into your ~/.emacs.
;;   (require 'mcomplete)
;;   (load "mcomplete-history")
;;   (turn-on-mcomplete-mode)

;; session.el (http://emacs-session.sourceforge.net/) is useful to
;; save the minibuffer history.

(eval-when-compile (require 'cl))

;;; Key maps
(setq mcomplete-permissive-completion-map-alist
      '(
        ;;("\t"   . completer-complete)
        ("\t"   . MComplete-complete)
        ;;(" "    . completer-word)
        (" "    . MComplete-complete-word)
        ("?"    . MComplete-completion-help)
        ("\r"   . MComplete-exit-minibuffer)
        ("\e\r" . exit-minibuffer)
        ("\n"   . MComplete-exit-minibuffer)
        ("\e\n" . exit-minibuffer)
        ("\C-c" . MComplete-toggle-ignore-case)
        ("\C-n" . MComplete-next-method)
        ("\C-p" . MComplete-previous-method)
        ("\C-s" . MComplete-next-candidate)
        ([right] . MComplete-next-candidate)
        ("\C-r" . MComplete-previous-candidate)
        ([left] . MComplete-previous-candidate)
        ;;(";" . MComplete-previous-method)
        ))

(setq mcomplete-must-match-completion-map-alist
      '(
        ;;("\t"   . completer-complete)
        ("\t"   . MComplete-complete)
        ;;(" "    . completer-word)
        (" "    . MComplete-complete-word)
        ("?"    . MComplete-completion-help)
        ("\r"   . MComplete-complete-and-exit)
        ("\n"   . MComplete-complete-and-exit)
        ("\C-c" . MComplete-toggle-ignore-case)
        ("\C-n" . MComplete-next-method)
        ("\C-p" . MComplete-previous-method)
        ("\C-s" . MComplete-next-candidate)
        ([right] . MComplete-next-candidate)
        ("\C-r" . MComplete-previous-candidate)
        ([left] . MComplete-previous-candidate)
        ;;(";" . MComplete-previous-method)
        ))

(setq mcomplete-default-method-set
      '(mcomplete-history-method mcomplete-prefix-method
                                 mcomplete-substr-method
                                 ))

(setq mcomplete-history-favorite-function-list '("indent-region" "face2html" "grep" "ediff-buffers"
                                                 "lisp-interaction-mode" "shell"))

(defvar mcomplete-history-method
  '(:name                 "History match"

                          ;; Core functions
    :try-completion       mcomplete-history-method-try-completion
    :all-completions      mcomplete-history-method-all-completions

    ;; Candidates exhibition
    ;;:exhibit              mcomplete-history-method-exhibit)
    :exhibit              mcomplete-prefix-method-exhibit)
  "Property list for history matching completion method.")

(defvar mcomplete-history-minibuf-history nil)
(defun mcomplete-history-init-minibuf-history (str abort-on-input)
  (let (list all-list)
    (setq all-list (all-completions str
                                    minibuffer-completion-table
                                    (if (and minibuffer-completion-predicate abort-on-input)
                                        'mcomplete-predicate-with-input-check
                                      minibuffer-completion-predicate)))
    ;;    (dolist (elt (symbol-value minibuffer-history-variable))
    (dolist (elt (append
                  mcomplete-history-favorite-function-list
                  (symbol-value minibuffer-history-variable)))
      (if (and
           (not (member elt list))
           (member elt all-list))
          (push elt list)))
    (setq mcomplete-history-minibuf-history (nreverse list))))

(defun mcomplete-history-method-try-completion (str abort-on-input)
  "`try-completion' for history matching method."
  (let ((completion-ignore-case mcomplete-ignore-case))
    (if (string= str "")
        (setq mcomplete-history-minibuf-history nil)
      (mcomplete-history-init-minibuf-history str abort-on-input))
    (if (null
         (try-completion str
                         (mapcar 'list mcomplete-history-minibuf-history)))
        (try-completion str
                        minibuffer-completion-table
                        (if (and minibuffer-completion-predicate abort-on-input)
                            'mcomplete-predicate-with-input-check
                          minibuffer-completion-predicate))
      (try-completion str
                      (mapcar 'list mcomplete-history-minibuf-history)))))

(defun mcomplete-history-method-all-completions (str abort-on-input)
  "`all-completions' for history matching method."
  (let ((completion-ignore-case mcomplete-ignore-case))
    (if (string= str "")
        (setq mcomplete-history-minibuf-history nil)
      (mcomplete-history-init-minibuf-history str abort-on-input))
    (sort
     (if (null
          (all-completions str
                           (mapcar 'list mcomplete-history-minibuf-history)))
         (all-completions str
                          minibuffer-completion-table
                          (if (and minibuffer-completion-predicate abort-on-input)
                              'mcomplete-predicate-with-input-check
                            minibuffer-completion-predicate))
       (all-completions str
                        (mapcar 'list mcomplete-history-minibuf-history)))
     'string<)))

(defface mcomplete-history-method-fixed-part-face
  '((((class color)
      (background dark))
     (:bold t :foreground "Aquamarine"))
    (((class color)
      (background light))
     (:bold t :foreground "medium blue"))
    (t
     ()))
  "Face to highlight the fixed part of input for history matching method."
  :group      'mcomplete)

(defface mcomplete-history-method-alternative-part-face
  '((((class color)
      (background dark))
     (:foreground "Aquamarine"))
    (((class color)
      (background light))
     (:foreground "medium blue"))
    (t
     ()))
  "Face to highlight the alternative part of input for history matching method."
  :group      'mcomplete)

(defun mcomplete-history-method-exhibit (str all try)
  "Exhibit history matching completion information in the minibuffer."
  (let* ((f-face 'mcomplete-history-method-fixed-part-face)
         (a-face 'mcomplete-history-method-alternative-part-face))
     
    ;;     (if all
    ;;         (let (list)
    ;;           (dolist (elt all)
    ;;             (unless (member elt list)
    ;;               (push elt list)))
    ;;           (setq all (nreverse list))))
    (unless (null try)
      (put-text-property (mcomplete-prompt-end) (point-max) 'face f-face))
    (cond
     ((null try) (insert " [No match]"))
     ((eq try t) (insert " [Sole completion]"))
     (t
      (let* ((fixed (substring try (length str)))
             (tail1 (substring (car all) (length try)))
             (rest  (cdr all))
             tail)
        (unless (string= fixed "")
          (put-text-property 0 (length fixed) 'face f-face fixed)
          (insert (concat "[" fixed "]")))
        (when (or rest (not (string= tail1 "")))
          (insert "{")
          (put-text-property 0 (length tail1) 'face a-face tail1)
          (insert tail1)
          (while (and rest
                      (< (+ (point-max)
                            (length (setq tail (substring (car rest)
                                                          (length try))))
                            4)
                         (window-width)))
            (insert "," tail)
            (setq rest (cdr rest)))
          (insert (if rest ",..}" "}"))))))))
