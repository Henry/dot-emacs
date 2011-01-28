;;; org-fstree.el --- include a filesystem subtree into an org file


;; Copyright 2009 Andreas Burtzlaff
;;
;; Author: Andreas Burtzlaff < andreas at burtz[REMOVE]laff dot de >
;; Version: 0.3
;; Keywords: org-mode filesystem tree
;; X-URL: <http://www.burtzlaff.de/org-fstree/org-fstree.el>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; org-fstree inserts the filesystem subtree for a given directory.
;; Each file/directory is formatted as a headline, provides links back 
;; to all headlines that are associated with it (by containing links to the file) 
;; and is assigned their tags.
;;
;; Installation:
;;   - put this file into your load-path 
;;   - insert "(require 'org-fstree)" into ~/.emacs
;;
;; Usage:
;;   - enter a line containing "#+BEGIN_FSTREE: <dir>" into an org buffer, 
;;     where <dir> is the directory, that is to be inserted.
;;   - while the cursor is in the line mentioned, press "C-c C-c"
;;
;; Options:
;;   Specify options in the form:
;;   "#+BEGIN_FSTREE: <dir> :<optionname1> <optionvalue1> :<optionname2> <optionvalue2>  ...
;;   Options are:
;;     - :non-recursive t , to suppress recursion into directories
;;     - :exclude-regexp-name <list of regexp strings> , exclude file/directory names matching either 
;;                                                  of the given regexp expressions
;;       Examples: 
;;         :exclude-regexp-name (".*\\.pdf$" ".*\\.zip$"), excludes files/directories ending with either ".pdf" or ".zip"
;;         :exclude-regexp-name ("^\\.git$") , excludes files/directories named ".git"
;;
;;     - :exclude-regexp-fullpath <list of regexp strings>, same as :exclude-regexp-name but matches absolute path to file/directory
;;     - :relative-links t , generates relative instead of absolute links
;;
;; Limitations and warnings:
;;
;;   - when triggering an update (by pressing "C-c C-c" while in the line mentioned above)
;;     the COMPLETE REGION BETWEEN "#+BEGIN_FSTREE" AND "#+END_FSTREE" IS REPLACED.
;;   - problems matching links to files with exotic characters in their names
;;   - speed  
;;     
;; Code:

(provide 'org-fstree)

(require 'org)

(defun org-fstree-generate (dir level options)
  (if (file-directory-p dir)
     (let (
	   (non-recursive (plist-get options :non-recursive))
	   (exclude-regexp-name-list (plist-get options :exclude-regexp-name))
	   (exclude-regexp-fullpath-list (plist-get options :exclude-regexp-fullpath))
	   (links-as-properties (plist-get options :links-as-properties))
	   (fullFileNames (directory-files dir 1 nil t) )
	   (fileNames (directory-files dir nil nil t) )
	   fileName
	   fullFileName
	   currentHeadline
	   orgHeadlineInfo
	   curTags
	   curPos
	   (linksList nil)
	   retString
	   )
       (while fileNames
	 (setq fullFileName (car fullFileNames))
	 (setq fullFileNames (cdr fullFileNames))
	 (setq fileName (car fileNames))
	 (setq fileNames (cdr fileNames))
	 (setq linksList nil)
	 (setq curTags nil)
	 (cond ((member fileName '("." "..")))
	       ;; the following two lines is a really ugly. I'll be glad if someone with more lisp experience tidies this up.
	       ((reduce (function (lambda (a b) (or a b)))  (mapcar (function (lambda (regexp) (not (string= fullFileName (replace-regexp-in-string regexp "" fullFileName) )) )) exclude-regexp-fullpath-list ) :initial-value nil))
	       ((reduce (function (lambda (a b) (or a b)))  (mapcar (function (lambda (regexp) (not (string= fileName (replace-regexp-in-string regexp "" fileName) )) )) exclude-regexp-name-list ) :initial-value nil))
	       (t
		(save-excursion 
                ;; Search for links in current buffer
		(goto-char (point-min))
		(setq curPos (point))
		(while (re-search-forward org-bracket-link-regexp nil t)
		  (cond ( (string= fullFileName (expand-file-name (car (split-string (replace-regexp-in-string "^file:" "" (match-string 1) ) ":" ) ) ) )
			  (let ((p (point)))
			    (cond ((org-before-first-heading-p))
				  (t
				   ;; go to associated heading
				   (org-back-to-heading t)
				   (setq orgHeadlineInfo (org-heading-components))
				   (setq curTags (concat curTags (nth 5 orgHeadlineInfo) ))
				   (setq currentHeadline (nth 4 orgHeadlineInfo))
				   ;; filter all links from headline, generate link to it and append to linksList
				   (let ((cleanedHeadline (replace-regexp-in-string "\\[\\[.*\\]\\]" "" currentHeadline)))
				     
				     (setq linksList (cons (concat "[[*"  cleanedHeadline "][" 
								   (replace-regexp-in-string fullFileName "" cleanedHeadline) "]]") linksList))
				     )
				   (goto-char p)
				   )
				  )
			    )
			  )
			)
		  )
		(cond ((or (not (plist-get options :show-only-matches)) (not (null linksList)))
		       ;; construct headline for current file/directory
		       (let* ((tagString (cond ((not (null curTags)) (concat "  " (replace-regexp-in-string "::" ":" curTags)) ) ))
			      (headingString 
			       (concat retString "\n" (make-string level ?*) (if (file-directory-p fullFileName) " [D]" " [ ]") 
				       (format " [[file:%s][%s]]" (if (plist-get options :relative-links) (file-relative-name fullFileName) fullFileName) fileName)
				       
				       )
			       )
			      (linkCount 0)
			      )
			 (setq retString 
			       (cond ( links-as-properties
				       
				       (concat headingString tagString (if (not (null linksList)) (concat "\n :PROPERTIES:\n " (mapconcat (function (lambda (string) (setq linkCount (1+ linkCount)) (concat ":Link" (number-to-string linkCount) ":" string ))) linksList "\n") "\n :END:" ) )
					       )
				       )
				     (t 
				      (concat headingString 
					      (cond ((not (null linksList)) 
						     (concat "      { " (mapconcat 'identity linksList " | ") " }" ) 
						     )
						    )
					      tagString 
					      )
				      )
				     )
			       )
			 )
		       (if (and (null non-recursive) (file-directory-p fullFileName) )
			   (setq retString (concat retString (org-fstree-generate fullFileName (1+ level) options) ) )
			 )
		       )
		      )
		
		)
		)	
	       )
	 )
       retString)
    (message "%s is not a directory" dir)
    )
  )

(defun org-fstree-apply-maybe ()
  (save-excursion
     (if (save-excursion (beginning-of-line 1) (looking-at "#\\+END_FSTREE"))
	 (re-search-backward "#\\+BEGIN_FSTREE" nil t))
     (cond
      ((save-excursion (beginning-of-line 1) (looking-at "#\\+BEGIN_FSTREE"))
       (let* ((params (org-fstree-gather-parameters))
	      (dir (plist-get params :dir))
	      (options (plist-get params :params))
	      level)
	 ;; get current level; there is a BUG if "#+BEGIN_FSTREE" is inserted after the last headlines dots, that indicate its folded state.
	 (let ((p (point)))
	   (cond ((org-before-first-heading-p)
		  (setq level 1))
		 (t (org-back-to-heading)
		    (setq level (+ (funcall outline-level) 1))
		    (goto-char p)
		    )
		 )
	   )
	   (forward-line)
	   (let ((beg (point)))
	     (re-search-forward "#\\+END_FSTREE" nil t)
	     (cond ( (looking-back "#\\+END_FSTREE") 
		     (forward-line -1)
		     (end-of-line 1)
		     (delete-region beg (point) )
		     (insert (concat (org-fstree-generate dir level options) "\n") )
		     )
		   (t (goto-char beg)
		      (insert (concat (concat (org-fstree-generate dir level options) "\n") "\n#+END_FSTREE"))
		      )
		   )
	     ) 
	 )
       1
       )   
      )
     )
  )


(defun org-fstree-gather-parameters ()
  (save-excursion 
    (let (rtn)
      (beginning-of-line 1)
      (if (looking-at "#\\+BEGIN_FSTREE[: \t][ \t]*\\([^ \t\r\n]+\\)\\( +.*\\)?")
	(let ((dir (org-no-properties (match-string 1)))
	      (params (if (match-end 2)
			  (read (concat "(" (match-string 2) ")")))))
	  ;; no additional parameters yet
	  (setq rtn (list :dir dir :params params) )
	  ))
      rtn)
    )
)

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-fstree-apply-maybe)