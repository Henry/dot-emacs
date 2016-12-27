;;; company-pcomplete.el --- company-mode pcomplete backend -*- lexical-binding: t -*-

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
;; Based on `ac-pcomplete', see https://www.emacswiki.org/emacs/EshellCompletion
;; and `pcomplete`, see pcomplete.el.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'pcomplete)

(defgroup company-pcomplete nil
  "Completion backend using pcomplete."
  :group 'company)

(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--prefix ()
  (let* ((pcomplete-stub)
         pcomplete-seen
         pcomplete-norm-func
         pcomplete-args
         pcomplete-last pcomplete-index
         (pcomplete-autolist pcomplete-autolist)
         (pcomplete-suffix-list pcomplete-suffix-list))
    (pcomplete-completions)
    (buffer-substring (pcomplete-begin) (point))))

(defun company-pcomplete--candidates ()
  (let* ((pcomplete-stub)
         (pcomplete-show-list t)
         pcomplete-seen pcomplete-norm-func
         pcomplete-args pcomplete-last pcomplete-index
         (pcomplete-autolist pcomplete-autolist)
         (pcomplete-suffix-list pcomplete-suffix-list)
         (candidates (pcomplete-completions))
         (prefix (buffer-substring (pcomplete-begin) (point)))
         ;; Collect all possible completions for the current stub
         (cnds (all-completions pcomplete-stub candidates))
         (bnds (completion-boundaries pcomplete-stub candidates nil ""))
         (skip (- (length pcomplete-stub) (car bnds))))
    ;; Replace the stub at the beginning of each candidate by the prefix
    (mapcar #'(lambda (cand) (concat prefix (substring cand skip))) cnds)))

(defun company-pcomplete-available ()
  (when (eq company-pcomplete-available 'unknown)
    (condition-case _err
        (progn
          (company-pcomplete--candidates)
          (setq company-pcomplete-available t))
      (error
       (message "Company: pcomplete not found")
       (setq company-pcomplete-available nil))))
  company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional _arg &rest ignored)
  "`company-mode' completion backend using `pcomplete'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pcomplete))
    (prefix (when (company-pcomplete-available)
              (company-pcomplete--prefix)))
    (candidates (company-pcomplete--candidates))
    (sorted t)))

(provide 'company-pcomplete)
;;; company-pcomplete.el ends here
