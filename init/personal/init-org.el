;; -----------------------------------------------------------------------------
;;; Basic configuration

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-agenda-files
      (list (concat org-directory "/Agenda")
            (concat org-directory "/OpenFOAM")
            ;(concat org-directory "/OpenFOAM/Contracts")
            )
      )

;; -----------------------------------------------------------------------------
;;; Remember

(setq org-remember-templates
      '(("TODO" ?t "* TODO %?\n  %i\n  %a" "Agenda/TODO.org" "Tasks")
        ("Discuss" ?d "* DIS %U %?\n\n  %i\n  %a" "Agenda/discuss.org" "Discuss")
        ("OF DEV" ?o "* DEV %?\n\n  %i\n  %a" "OpenFOAM/OpenFOAM.org"
         "Developments")
        ("OF TODO" ?T "* TODO %?\n  %i\n  %a" "OpenFOAM/OpenFOAM.org" "Tasks")
        ("Emacs" ?e "* TODO %?\n  %i\n  %a" "emacs.org" "Emacs")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "Agenda/journal.org" "Journal")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "ideas.org" "New Ideas")
        ("Bookmark" ?b "* %A\n  %i" "bookmarks.org" "Bookmarks")
        ("Web" ?w "* %c
  :PROPERTIES:
  :Created:    %U
  :Title:      %:description
  :URL:        %:link
  :Link:       %c
  :END:
*** Description
*** Selected Region
    %i
    " "webLinks.org" "Web Links")
        ))

;; -----------------------------------------------------------------------------
;;; LaTeX export

(add-to-list
 'org-latex-classes
 '("OpenFOAMProposal"
   "\\input{.latex}
\\input{\\macroPath/preambleReport}
\\renewcommand{\\ReportType}{Project Proposal}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list
 'org-latex-classes
 '("OpenFOAMProgressReport"
   "\\input{.latex}
\\input{\\macroPath/preambleReport}
\\renewcommand{\\ReportType}{Progress Report}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; -----------------------------------------------------------------------------
;;; Support contract configuration

(defvar support-hours-dir (expand-file-name "~/SupportHours"))

(defun support ()
  "Open the support hours directory in dired."
  (interactive)
  (select-frame (make-frame))
  (setq org-startup-folded 'nofold)
  (dired support-hours-dir))

(setq org-hide-leading-stars t
      org-odd-levels-only t
      org-tag-alist
      '(
        ("Contact". ?c)
        ("ID" . ?i)
        ("Email" . ?e)
        ("Phone" . ?p)
        ("Mobile" . ?m)
        ("Address" . ?a))
      org-global-properties
      '(
        ("Currency_ALL" . "GBP SKR")
        ("Payment_Status_ALL" . "\"Not Invoiced\" Invoiced Paid")
        ("Approved_ALL" . "\"[ ]\" \"[X]\"")))

(defun org-tbl-push-support-data
  (final values value-prop percent-prop value-curr fxrate value-gbp-prop value-liab-prop)
  "Calculate the sum of the VALUES and the percentage w.r.t. FINAL,
push these results into the given properties VALUE-PROP and
PERCENT-PROP.

Read the project value in given currency VALUE_CURR and exchange
rate FXRATE in GBP.  Evaluate the project value in GBP and return
the result and push it into VALUE-GBP-PROP.

From the percent completed, evaluate the liability and push into
in VALUE-LIAB-PROP.

Return the sum of the VALUES."
  (let* ((total-20mins (apply '+ values))
         (total-hrs (/ (apply '+ values) 3.0))
         (percent-hrs (/ (* 100 total-hrs) final))
         (value-gbp (* value-curr fxrate))
         (value-liab (/ (* value-gbp (- 100 percent-hrs)) 100)))
    (org-entry-put (point) value-prop
                   (format "%.1f" total-hrs))
    (org-entry-put (point) percent-prop
                   (format "%.0f" percent-hrs))
    (org-entry-put (point) value-gbp-prop
                   (format "%.0f" value-gbp))
    (org-entry-put (point) value-liab-prop
                   (format "%.0f" value-liab))
    (format "%d" total-20mins)))

(add-to-list
 'org-latex-classes
 '("OpenFOAMSupport"
   "\\input{.latex}
\\input{\\macroPath/preambleReport}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\renewcommand{\\ReportType}{Support Contract}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list
 'org-latex-classes
 '("CFDDirectQuote"
   "\\input{.latex}
\\input{\\macroPath/preambleQuote}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{amssymb}
\\usepackage{hyperref}
%%\\addtocounter{page}{1}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
