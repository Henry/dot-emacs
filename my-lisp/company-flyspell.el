;;; company-flyspell.el --- company-mode flyspell backend -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Henry G. Weller

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'flyspell)

(defgroup company-flyspell nil
  "Completion backend using flyspell."
  :group 'company)

(defvar company-flyspell-available 'unknown)

(defun company-flyspell--lookup-words (word)
  "Return a list of possible corrections for misspelled WORD.
Based on `flyspell-correct-word-before-point'."
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let (words ispell-filter)
    ;; now check spelling of word.
    (ispell-send-string "%\n")  ; put in verbose mode
    (ispell-send-string (concat "^" word "\n"))
    ;; wait until Aspell has processed word
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (if (consp ispell-filter)
        (setq words (ispell-parse-output (car ispell-filter))))
    (cond
     ((or (eq words t) (stringp words))
      ;; don't correct word
      (list word))
     ((null words)
      ;; ispell error
      (error "Ispell: error in Ispell process")
      nil)
     (t
      ;; The word is incorrect, return list of alternatives
      (nth 2 words)))))

(defun company-flyspell-available ()
  (when (eq company-flyspell-available 'unknown)
    (condition-case _err
        (progn
          (company-flyspell--lookup-words "WHATEVER")
          (setq company-flyspell-available t))
      (error
       (message "Company: flyspell-look-command not found")
       (setq company-flyspell-available nil))))
  company-flyspell-available)

;;;###autoload
(defun company-flyspell (command &optional arg &rest ignored)
  "`company-mode' completion backend using flyspell."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-flyspell))
    (prefix (when (company-flyspell-available)
              (company-grab-word)))
    (candidates (company-flyspell--lookup-words arg))
    (sorted t)
    (ignore-case nil)))

(provide 'company-flyspell)
;;; company-flyspell.el ends here
