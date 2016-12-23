;;; company-pcomplete.el --- company-mode completion backend using pcomplete

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

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'pcomplete)

(defgroup company-pcomplete nil
  "Completion backend using pcomplete."
  :group 'company)

(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--candidates ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen for a company backend
  (cl-letf (((symbol-function 'insert-and-inherit) (lambda) (&rest args)))
    ;; From `pcomplete' in pcomplete.el
    (let* ((pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; Note: buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. by the may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; Complete only if the stub  matches the end of the buffer text
      (when (or (string= pcomplete-stub
                           (substring buftext
                                      (max 0
                                           (- (length buftext)
                                              (length pcomplete-stub)))))
                  (string= pcomplete-stub
                           (substring arg
                                      (max 0
                                           (- (length arg)
                                              (length pcomplete-stub))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                    cnds))))))

(defun company-pcomplete-available ()
  (when (eq company-pcomplete-available 'unknown)
    (condition-case err
        (progn
          (company-pcomplete--candidates)
          (setq company-pcomplete-available t))
      (error
       (message "Company: pcomplete not found")
       (setq company-pcomplete-available nil))))
  company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional arg &rest ignored)
  "`company-mode' completion backend using `pcomplete'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pcomplete))
    (prefix (when (company-pcomplete-available)
              (company-grab-symbol)))
    (candidates (company-pcomplete--candidates))
    (sorted t)))

(provide 'company-pcomplete)
;;; company-pcomplete.el ends here
