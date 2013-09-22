;;; dirtree.el --- Directory tree using tree-widget
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Henry G. Weller
;; Copyright (C) 2010 Free Software Foundation, Inc.
;; Created: 09 Jan 2010
;; Version: 0.1
;; Last-Updated: Fri Sep 20 19:04:32 2013 (+0100)
;;           By: Henry G. Weller
;;     Update #: 1
;; URL:
;; Keywords: files, convenience
;; Compatibility: GNU Emacs 24.x (may work with earlier versions)
;; This file is NOT part of Emacs.
;;
;;------------------------------------------------------------------------------
;;; Commentary:
;;
;; Basic functionality
;;  1. Displays many directory in one buffer to reduce buffer numbers
;;  2. Reuse directory tree
;;  3. Use simple key bindings
;;
;;------------------------------------------------------------------------------
;;; Installation
;;
;; 1. Download dependencies
;;    wget http://www.emacswiki.org/cgi-bin/wiki/tree-mode.el
;;
;; 2. Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'dirtree)
;;   or
;;   (autoload 'dirtree "dirtree" "Add directory to tree view" t)
;;
;; -----------------------------------------------------------------------------
;;; Change log:
;;
;; Version 0.1
;; * Initial release
;; * Added dirtree-up and dirtree-down to reset tree to parent or child
;; * Changed dirtree-display to open a directory in same window
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
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tree-mode)
(require 'dired-x)

(defgroup dirtree nil
  "Directory tree views"
  :group 'tools)

(defcustom dirtree-buffer "*dirtree*"
  "*Buffer name for `dirtree'"
  :type 'string
  :group 'dirtree)

(define-widget 'dirtree-dir-widget 'tree-widget
  "Directory Tree widget."
  :dynargs        'dirtree-expand
  :has-children   t)

(define-widget 'dirtree-file-widget 'push-button
  "File widget."
  :format         "%[%t%]\n"
  :button-face    'default
  :notify         'dirtree-select)

(defun dirtree-show ()
  "Show `dirtree-buffer'. Create tree when no parent directory find."
  (interactive)
  (let ((buffer (get-buffer-create dirtree-buffer))
        (dir default-directory)
        trees tree button path)
    (with-current-buffer buffer
      (setq trees tree-mode-list)
      (while (and trees
                  (not tree))
        (if (string-match
             (concat "^" (regexp-quote (widget-get (car trees) :file))) dir)
            ;; if parent directory in buffer
            (setq tree (car trees))
          (setq trees (cdr trees)))))
    (if tree
        (progn
          (setq path
                (split-string
                 (file-relative-name buffer-file-name (widget-get tree :file))
                 "/"))
          (dirtree (widget-get tree :file) t)
          (setq button (tree-mode-find-node tree path))
          (if button
              (goto-char (widget-get (car button) :from))))
      (call-interactively 'dirtree))))

(defun dirtree-build (buffer root &optional select win)
  "Create tree of `root' directory in `buffer' and select if `select' is true."
  (let (tree)
    (with-current-buffer buffer
      (unless (eq major-mode 'dirtree-mode)
        (dirtree-mode))
      (dolist (atree tree-mode-list)
        (if (string= (widget-get atree :file) root)
            (setq tree atree)))
      (or tree
          (setq tree (tree-mode-insert (dirtree-root-widget root)))))
    (unless win
      (setq win (or (get-buffer-window dirtree-buffer)
                    (display-buffer dirtree-buffer)))
      (select-window win))
    (with-selected-window win
      (unless (widget-get tree :open)
        (widget-apply-action tree))
      (goto-char (widget-get tree :from))
      (recenter 1))
    (if select
        (select-window win))))

(defun dirtree (root select)
  "create tree of `root' directory
With prefix arguement select `dirtree-buffer'"
  (interactive "DDirectory: \nP")
  (let ((buffer (get-buffer-create dirtree-buffer)))
    (dirtree-build buffer root select)))

(define-derived-mode dirtree-mode tree-mode "Dir-Tree"
  "A mode to display tree of directory"
  (tree-widget-set-theme "folder"))

(defun dirtree-root-widget (directory)
  "create the root directory"
  `(dirtree-dir-widget
    :node (dirtree-file-widget
           :tag ,directory
           :file ,directory)
    :file ,directory
    :open t))

(defun dirtree-expand (tree)
  "expand directory"
  (or (widget-get tree :args)
      (let ((directory (widget-get tree :file))
            (re (dired-omit-regexp))
            dirs files basename)
        (dolist (file (directory-files directory t))
          (setq basename (file-name-nondirectory file))
          (unless (string-match re basename)
            (if (file-directory-p file)
                (push (cons file basename) dirs)
              (push (cons file basename) files))))
        (setq dirs (sort dirs (lambda (a b) (string< (cdr a) (cdr b)))))
        (setq files (sort files (lambda (a b) (string< (cdr a) (cdr b)))))
        (append
         (mapcar (lambda (file)
                   `(dirtree-dir-widget
                     :file ,(car file)
                     :node (dirtree-file-widget
                            :tag ,(cdr file)
                            :file ,(car file))))
                 dirs)
         (mapcar (lambda (file)
                   `(dirtree-file-widget
                     :file ,(car file)
                     :tag ,(cdr file)))
                 files)))))

(defun dirtree-select (node &rest ignore)
  "Open file in other window"
  (let ((file (widget-get node :file)))
    (and file
         (if (file-directory-p file)
             (find-file file)
           (find-file-other-window file)))))

;; -----------------------------------------------------------------------------
;;; Commands / Keybindings

(defun dirtree-display ()
  "Open file under point"
  (interactive)
  (let ((widget (widget-at (1- (line-end-position))))
        file)
    (if (setq file (widget-get widget :file))
        (if (file-directory-p file)
            (find-file file)
          (find-file-other-window file)))))

(defun dirtree-up ()
  "Recreate the dirtree for the parent directory."
  (interactive)
  (cd "..")
  (tree-mode-goto-root)
  (tree-mode-delete (tree-mode-tree-ap))
  (dirtree-build (current-buffer) default-directory t))

(defun dirtree-down ()
  "Recreate the dirtree for the child directory."
  (interactive)
  (let ((widget (widget-at (1- (line-end-position))))
        file)
    (when (and (setq file (widget-get widget :file))
               (file-directory-p file))
      (cd file)
      (tree-mode-goto-root)
      (tree-mode-delete (tree-mode-tree-ap))
      (dirtree-build (current-buffer) default-directory t))))

(define-key dirtree-mode-map "^" 'dirtree-up)
(define-key dirtree-mode-map [(shift return)] 'dirtree-down)
(define-key dirtree-mode-map "\C-o" 'dirtree-display)
(define-key dirtree-mode-map [(control return)] 'dirtree-display)

(provide 'dirtree)

;; -----------------------------------------------------------------------------
;;; dirtree.el ends here
