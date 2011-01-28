;;; peg.el --- Parsing Expression Grammars in Emacs Lisp
;;
;; Copyright 2008  Helmut Eller <eller.helmut@gmail.com>.
;;
;; Version: 0.5 (2008-Dec-21)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Parsing Expression Grammars (PEG) are a formalism in the spirit of
;; Context Free Grammars (CFG) with some simplifications which makes
;; the implementation of PEGs as top-down parser particularly simple
;; and easy to understand [Ford, Baker].
;;
;; This file implements a macro `peg-parse' which parses the current
;; buffer according to a PEG.  E.g. we can match integers with a PEG
;; like this:
;;
;;  (peg-parse (number   sign digit (* digit))
;;             (sign     (or "+" "-" ""))
;;             (digit    [0-9]))
;;
;; In contrast to regexps, PEGs allow us to define recursive rules.  A
;; PEG is a list of rules.  A rule is written as (NAME PE ...).
;; E.g. (sign (or "+" "-" "")) is a rule with the name "sign".  The
;; syntax for Parsing Expression (PE) is a follows:
;;
;; Description   	Lisp		Haskell, as in Ford's paper
;; Sequence 		(and e1 e2)    	e1 e2
;; Prioritized Choice   (or e1 e2)	e1 / e2
;; Not-predicate 	(not e)		!e
;; And-predicate	(if e)		&e
;; Any character	(any)		.
;; Literal string	"abc"		"abc"
;; Character C		(char c)	'c'
;; Zero-or-more 	(* e)		e*
;; One-or-more 		(+ e)		e+
;; Optional		(opt e)		e?
;; Character range	(range a b)	[a-b]
;; Character set	[a-b "+*" ?x]	[a-b+*x]  ; note: [] is a elisp vector
;; Character classes    [ascii cntrl]
;; Beginning-of-Buffer  (bob)
;; End-of-Buffer        (eob)
;; Beginning-of-Line    (bol)
;; End-of-Line        	(eol)
;; Beginning-of-Word    (bow)
;; End-of-Word        	(eow)
;; Beginning-of-Symbol  (bos)
;; End-of-Symbol       	(eos)
;; Syntax-Class       	(syntax-class NAME)
;;
;; `peg-parse' also supports parsing actions, i.e. Lisp snippets which
;; are executed when a PE matches.  This can be used to construct
;; syntax trees or for similar tasks.  Actions are written as
;;
;;  (action FORM)          ; evaluate FORM
;;  `(VAR... -- FORM...)   ; stack action
;; 
;; Actions don't consume input, but are executed at the point of
;; match.  A "stack action" takes VARs from the "value stack" and
;; pushes the result of evaluating FORMs to that stack.  See
;; `peg-ex-parse-int' for an example.
;;
;; Derived Operators:
;;
;; The following operators are implemented as combinations of
;; primitive expressions:
;;
;; (substring E)  ; match E and push the substring for the matched region
;; (region E)     ; match E and push the corresponding start and end positions
;; (replace E RPL); match E and replace the matched region with RPL.
;; (list E)       ; match E and push a list of all items that E produces.
;;
;; Regexp equivalents:
;;
;; Here a some examples for regexps and how those could be written as PE.
;; [Most are taken from rx.el]
;;
;; "^[a-z]*"
;; (and (bol) (* [a-z]))
;;
;; "\n[^ \t]"
;; (and "\n" (not [" \t"]) (any))
;;
;; "\\*\\*\\* EOOH \\*\\*\\*\n"
;; "*** EOOH ***\n"
;;
;; "\\<\\(catch\\|finally\\)\\>[^_]"
;; (and (bow) (or "catch" "finally") (eow) (not "_") (any))
;;
;; "[ \t\n]*:\\([^:]+\\|$\\)"
;; (and (* [" \t\n"]) ":" (or (+ (not ":") (any)) (eol)))
;;
;; "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
;; (and (bol) 
;;      "content-transfer-encoding:" 
;;      (* (opt "\n") ["\t "])
;;      "quoted-printable"
;;      (* (opt "\n") ["\t "]))
;;
;; "\\$[I]d: [^ ]+ \\([^ ]+\\) "
;; (and "$Id: " (+ (not " ") (any)) " " (+ (not " ") (any)) " ")
;;
;; "^;;\\s-*\n\\|^\n"
;; (or (and (bol) ";;" (* (syntax-class whitespace)) "\n")
;;     (and (bol) "\n"))
;;
;; "\\\\\\\\\\[\\w+"
;; (and "\\\\[" (+ (syntax-class word)))
;;
;; Search forward for ";;; Examples" for other examples.
;;
;; References:
;;
;; [Ford] Bryan Ford. Parsing Expression Grammars: a Recognition-Based
;; Syntactic Foundation. In POPL'04: Proceedings of the 31st ACM
;; SIGPLAN-SIGACT symposium on Principles of Programming Languages,
;; pages 111-122, New York, NY, USA, 2004. ACM Press.
;;
;; [Baker] Baker, Henry G. "Pragmatic Parsing in Common Lisp".  ACM Lisp
;; Pointers 4(2), April--June 1991, pp. 3--15.
;;

(unless (>= emacs-major-version 22)
  (error "peg.el requires Emacs version 22 or newer"))

;;; Code:

(defmacro peg-parse (&rest rules)
  "Match RULES at point.
Return (T STACK) if the match succeed and nil on failure."
  (peg-translate-rules rules))

(defmacro peg-parse-exp (exp)
  "Match the parsing expression EXP at point.
Note: a PE can't \"call\" rules by name."
  `(let ((peg-thunks nil))
     (when ,(peg-translate-exp (peg-normalize exp))
       (peg-postprocess peg-thunks))))

;; A table of the PEG rules.  Used during compilation to resolve
;; references to named rules.
(defvar peg-rules)

;; used at runtime for backtracking.  It's a list ((POS . THUNK)...).
;; Each THUNK is executed at the corresponding POS.  Thunks are
;; executed in a postprocessing step, not during parsing.
(defvar peg-thunks)

;; The basic idea is to translate each rule to a lisp function.
;; The result looks like
;;   (let ((rule1 (lambda () code-for-rule1))
;;         ...
;;         (ruleN (lambda () code-for-ruleN)))
;;     (funcall rule1))
;; 
;; code-for-ruleX returns t if the rule matches and nil otherwise.
;;
(defun peg-translate-rules (rules)
  "Translate the PEG RULES, to a top-down parser."
  (let ((peg-rules (make-hash-table :size 20)))
    (dolist (rule rules)
      (puthash (car rule) 'defer peg-rules))
    (dolist (rule rules)
      (puthash (car rule) (peg-normalize `(and . ,(cdr rule))) peg-rules))
    (peg-check-cycles peg-rules)
    `(let ((peg-thunks '()) . ,(mapcar #'car rules))
       ,@(mapcar (lambda (rule)
		   (let ((name (car rule)))
		     `(setq ,name
			    (lambda ()
			      ,(peg-translate-exp (gethash name peg-rules))))))
		 rules)
       (when (funcall ,(car (car rules)))
	 (peg-postprocess peg-thunks)))))

(eval-and-compile 
  (defun peg-method-table-name (method-name)
    (intern (format "peg-%s-methods" method-name))))

(defmacro peg-define-method-table (name)
  (let ((tab (peg-method-table-name name)))
    `(progn
       (defvar ,tab)
       (setq ,tab (make-hash-table :size 20)))))

(defmacro peg-add-method (method type args &rest body)
  (declare (indent 3))
  `(puthash ',type (lambda ,args . ,body) ,(peg-method-table-name method)))

(peg-define-method-table normalize)

;; Internally we use a regularized syntax, e.g. we only have binary OR
;; nodes.  Regularized nodes are lists of the form (OP ARGS...).
(defun peg-normalize (exp)
  "Return a \"normalized\" form of EXP."
  (cond ((and (consp exp)
	      (let ((fun (gethash (car exp) peg-normalize-methods)))
		(and fun 
		     (apply fun (cdr exp))))))
	((stringp exp)
	 (let ((len (length exp)))
	   (cond ((zerop len) '(null))
		 ((= len 1) `(char ,(aref exp 0)))
		 (t `(str ,exp)))))
	((and (symbolp exp) exp)
	 (when (not (gethash exp peg-rules))
	   (error "Reference to undefined PEG rule: %S" exp))
	 `(call ,exp))
	((vectorp exp)
	 (peg-normalize `(set . ,(append exp '()))))
	(t
	 (error "Invalid parsing expression: %S" exp))))

(defvar peg-leaf-types '(null fail any call action char range str set
			      bob eob bol eol bow eow bos eos syntax-class))

(dolist (type peg-leaf-types)
  (puthash type `(lambda (&rest args) (cons ',type args)) 
	   peg-normalize-methods))

(peg-add-method normalize or (&rest args)
  (cond ((null args) '(fail))
	((null (cdr args)) (peg-normalize (car args)))
	(t `(or ,(peg-normalize (car args)) 
		,(peg-normalize `(or . ,(cdr args)))))))

(peg-add-method normalize and (&rest args)
  (cond ((null args) '(null))
	((null (cdr args)) (peg-normalize (car args)))
	(t `(and ,(peg-normalize (car args)) 
		 ,(peg-normalize `(and . ,(cdr args)))))))

(peg-add-method normalize * (&rest args)
  `(* ,(peg-normalize `(and . ,args))))

;; FIXME: this duplicates code; could use some loop to avoid that
(peg-add-method normalize + (&rest args)
  (let ((e (peg-normalize `(and . ,args))))
    `(and ,e (* ,e))))

(peg-add-method normalize opt (&rest args)
  (let ((e (peg-normalize `(and . ,args))))
    `(or ,e (null))))

(peg-add-method normalize if (&rest args)
  `(if ,(peg-normalize `(and . ,args))))

(peg-add-method normalize not (&rest args)
  `(not ,(peg-normalize `(and . ,args))))

(peg-add-method normalize \` (form)
  (peg-normalize `(stack-action ,form)))

(peg-add-method normalize stack-action (form)
  (unless (member '-- form)
    (error "Malformed stack action: %S" form))
  (let ((args (cdr (member '-- (reverse form))))
	(values (cdr (member '-- form))))
    (let ((form `(let ,(mapcar (lambda (var) `(,var (pop peg-stack))) args)
		   ,@(mapcar (lambda (val) `(push ,val peg-stack)) values))))
      `(action ,form))))

(defvar peg-char-classes 
  '(ascii alnum alpha blank cntrl digit graph lower multibyte nonascii print
	  punct space unibyte upper word xdigit))

(peg-add-method normalize set (&rest specs)
  (cond ((null specs) '(fail))
	((and (null (cdr specs))
	      (let ((range (peg-range-designator (car specs))))
		(and range `(range ,(car range) ,(cdr range))))))
	(t 
	 (let ((chars '()) (ranges '()) (classes '()))
	   (while specs
	     (let* ((spec (pop specs))
		    (range (peg-range-designator spec)))
	       (cond (range
		      (push range ranges))
		     ((peg-characterp spec)
		      (push spec chars))
		     ((stringp spec)
		      (setq chars (append (reverse (append spec ())) chars)))
		     ((memq spec peg-char-classes)
		      (push spec classes))
		     (t (error "Invalid set specifier: %S" spec)))))
	   (setq ranges (reverse ranges))
	   (setq chars (delete-dups (reverse chars)))
	   (setq classes (reverse classes))
	   (cond ((and (null ranges) 
		       (null classes)
		       (cond ((null chars) '(fail))
			     ((null (cdr chars)) `(char ,(car chars))))))
		 (t `(set ,ranges ,chars ,classes)))))))

(defun peg-range-designator (x)
  (and (symbolp x)
       (let ((str (symbol-name x)))
	 (and (= (length str) 3)
	      (eq (aref str 1) ?-)
	      (< (aref str 0) (aref str 2))
	      (cons (aref str 0) (aref str 2))))))

;; characterp is new in Emacs 23.
(defun peg-characterp (x)
  (if (fboundp 'characterp) 
      (characterp x)
    (integerp x)))

(peg-add-method normalize list (&rest args)
  (peg-normalize 
   (let ((marker (make-symbol "magic-marker")))
     `(and (stack-action (-- ',marker))
	   ,@args
	   (stack-action (-- 
			  (let ((l '()))
			    (while 
				(let ((e (pop peg-stack)))
				  (cond ((eq e ',marker) nil)
					((null peg-stack)
					 (error "Marker not longer stack"))
					(t (push e l) t))))
			    l)))))))

(peg-add-method normalize substring (&rest args)
  (peg-normalize 
   `(and `(-- (point))
	 ,@args
	 `(start -- (buffer-substring-no-properties start (point))))))

(peg-add-method normalize region (&rest args)
  (peg-normalize 
   `(and `(-- (point))
	 ,@args
	 `(-- (point)))))

(peg-add-method normalize replace (pe replacement)
  (peg-normalize
   `(and (stack-action (-- (point)))
	 ,pe
	 (stack-action (start -- (progn
				   (delete-region start (point))
				   (insert-before-markers ,replacement))))
	 (stack-action (x --)))))

(peg-add-method normalize quote (form)
  (error "quote is reverved for future use"))

(peg-define-method-table translate)

;; This is the main translation function.
(defun peg-translate-exp (exp)
  "Return the ELisp code to match the PE EXP."
  (let ((translator (or (gethash (car exp) peg-translate-methods)
			(error "No translator for: %S" (car exp)))))
    (apply translator (cdr exp))))

(peg-add-method translate and (e1 e2)
  `(and ,(peg-translate-exp e1)
	,(peg-translate-exp e2)))

(peg-add-method translate or (e1 e2)
  (let ((cp (peg-make-choicepoint)))
    `(,@(peg-save-choicepoint cp)
      (or ,(peg-translate-exp e1)
	  (,@(peg-restore-choicepoint cp)
	   ,(peg-translate-exp e2))))))

;; Choicepoints are used for backtracking.  At a choicepoint we save
;; enough state, so that we can continue from there if needed.
(defun peg-make-choicepoint ()
  (cons (make-symbol "point") (make-symbol "thunks")))

(defun peg-save-choicepoint (choicepoint)
  `(let ((,(car choicepoint) (point))
	 (,(cdr choicepoint) peg-thunks))))

(defun peg-restore-choicepoint (choicepoint)
  `(progn 
     (goto-char ,(car choicepoint))
     (setq peg-thunks ,(cdr choicepoint))))

;; match empty strings
(peg-add-method translate null ()
  `t)

;; match nothing
(peg-add-method translate fail ()
  `nil)

(peg-add-method translate bob () '(bobp))
(peg-add-method translate eob () '(eobp))
(peg-add-method translate eol () '(eolp))
(peg-add-method translate bol () '(bolp))
(peg-add-method translate bow () '(looking-at "\\<"))
(peg-add-method translate eow () '(looking-at "\\>"))
(peg-add-method translate bos () '(looking-at "\\_<"))
(peg-add-method translate eos () '(looking-at "\\_>"))

(defvar peg-syntax-classes
  '((whitespace ?-) (word ?w) (symbol ?s) (punctuation ?.)
    (open ?\() (close ?\)) (string ?\") (escape ?\\) (charquote ?/)
    (math ?$) (prefix ?') (comment ?<) (endcomment ?>)
    (comment-fence ?!) (string-fence ?|)))

(peg-add-method translate syntax-class (class)
  (let ((probe (assoc class peg-syntax-classes)))
    (cond (probe `(looking-at ,(format "\\s%c" (cadr probe))))
	  (t (error "Invalid syntax class: %S\nMust be one of: %s" class
		    (mapcar #'car peg-syntax-classes))))))

(peg-add-method translate * (e)
  (let ((cp (peg-make-choicepoint)))
    `(progn (while (,@(peg-save-choicepoint cp)
		    (cond (,(peg-translate-exp e))
			  (t ,(peg-restore-choicepoint cp)
			     nil))))
	    t)))

(peg-add-method translate if (e)
  (let ((cp (peg-make-choicepoint)))
    `(,@(peg-save-choicepoint cp)
      (when ,(peg-translate-exp e)
	,(peg-restore-choicepoint cp)
	t))))

(peg-add-method translate not (e)
  (let ((cp (peg-make-choicepoint)))
    `(,@(peg-save-choicepoint cp)
      (when (not ,(peg-translate-exp e))
	,(peg-restore-choicepoint cp)
	t))))

(peg-add-method translate any ()
  '(when (not (eobp))
     (forward-char)
     t))

(peg-add-method translate char (c)
  `(when (eq (char-after) ',c)
     (forward-char)
     t))

(peg-add-method translate set (ranges chars classes)
  `(when (looking-at ',(peg-make-charset-regexp ranges chars classes))
     (forward-char)
     t))

(defun peg-make-charset-regexp (ranges chars classes)
  (when (and (not ranges) (not classes) (<= (length chars) 1))
    (error "Bug"))
  (let ((rbracket (member ?\] chars))
	(minus (member ?- chars))
	(hat (member ?^ chars)))
    (dolist (c '(?\] ?- ?^))
      (setq chars (delete c chars)))
    (format "[%s%s%s%s%s%s]"
	    (if rbracket "]" "")
	    (if minus "-" "")
	    (mapconcat (lambda (x) (format "%c-%c" (car x) (cdr x)))
		       ranges "")
	    (mapconcat (lambda (c) (format "[:%s:]" c)) classes "")
	    (mapconcat (lambda (c) (format "%c" c)) chars "")
	    (if hat "^" ""))))

(peg-add-method translate range (from to)
  `(when (and (char-after)
	      (<= ',from (char-after))
	      (<= (char-after) ',to))
     (forward-char)
     t))

(peg-add-method translate str (str)
  `(when (looking-at ',(regexp-quote str))
     (goto-char (match-end 0))
     t))

(peg-add-method translate call (name)
  (or (gethash name peg-rules) 
      (error "Reference to unknown rule: %S" name))
  `(funcall ,name))

(peg-add-method translate action (form)
  `(progn
     (push (cons (point) (lambda () ,form)) peg-thunks)
     t))

(defvar peg-stack)
(defun peg-postprocess (thunks)
  "Execute \"actions\"."
  (let  ((peg-stack '()))
    (dolist (thunk (mapcar (lambda (x)
			     (goto-char (car x))
			     (cons (point-marker) (cdr x)))
			   (reverse thunks)))
      (goto-char (car thunk))
      (funcall (cdr thunk)))
    (list t peg-stack)))

;; Left recursion is presumably a common mistate when using PEGs.
;; Here we try to detect such mistakes.  Essentailly we traverse the
;; graph as long as we can without consuming input.  When we find a
;; recursive call we signal an error.

(defun peg-check-cycles (peg-rules)
  (maphash (lambda (name exp)
	     (peg-detect-cycles exp (list name))
	     (dolist (node (peg-find-star-nodes exp))
	       (peg-detect-cycles node '())))
	   peg-rules))

(defun peg-find-star-nodes (exp)
  (let ((type (car exp)))
    (cond ((memq type peg-leaf-types) '())
	  (t (let ((kids (apply #'append 
				(mapcar #'peg-find-star-nodes (cdr exp)))))
	       (if (eq type '*)
		   (cons exp kids)
		 kids))))))

(peg-define-method-table detect-cycles)

(defun peg-detect-cycles (exp path)
  "Signal an error on a cycle.
Otherwise traverse EXP recursively and return T if EXP can match
without consuming input.  Return nil if EXP definetly consumes
input.  PATH is the list of rules that we have visited so far."
  (apply (or (gethash (car exp) peg-detect-cycles-methods) 
	     (error "No detect-cycle method for: %S" exp))
	 path (cdr exp)))

(peg-add-method detect-cycles call (path name)
  (cond ((member name path)
	 (error "Possible left recursion: %s"
		(mapconcat (lambda (x) (format "%s" x))
			   (reverse (cons name path)) " -> ")))
	(t
	 (peg-detect-cycles (gethash name peg-rules) (cons name path)))))

(peg-add-method detect-cycles and (path e1 e2)
  (and (peg-detect-cycles e1 path)
       (peg-detect-cycles e2 path)))

(peg-add-method detect-cycles or (path e1 e2)
  (or (peg-detect-cycles e1 path)
      (peg-detect-cycles e2 path)))

(peg-add-method detect-cycles * (path e) 
  (when (peg-detect-cycles e path)
    (error "Infinite *-loop: %S matches empty string" e))
  t)

(peg-add-method detect-cycles if  (path e) (peg-unary-nullable e path))
(peg-add-method detect-cycles not (path e) (peg-unary-nullable e path))

(defun peg-unary-nullable (exp path)
  (peg-detect-cycles exp path)
  t)

(peg-add-method detect-cycles any   (path)       nil)
(peg-add-method detect-cycles char  (path c)     nil)
(peg-add-method detect-cycles set   (path r c k) nil)
(peg-add-method detect-cycles range (path c1 c2) nil)
(peg-add-method detect-cycles str   (path s)     (equal s ""))
(peg-add-method detect-cycles null  (path)       t)
(peg-add-method detect-cycles fail  (path)       nil)
(peg-add-method detect-cycles bob   (path)       t)
(peg-add-method detect-cycles eob   (path)       t)
(peg-add-method detect-cycles bol   (path)       t)
(peg-add-method detect-cycles eol   (path)       t)
(peg-add-method detect-cycles bow   (path)       t)
(peg-add-method detect-cycles eow   (path)       t)
(peg-add-method detect-cycles bos   (path)       t)
(peg-add-method detect-cycles eos   (path)       t)
(peg-add-method detect-cycles syntax-class (p n) nil)
(peg-add-method detect-cycles action (path form) t)

;;; Tests:

(defmacro peg-parse-string (rules string)
  `(with-temp-buffer 
     (insert ,string)
     (goto-char (point-min))
     (peg-parse . ,rules)))

;; We can't expand the macro at compile time, because it needs helper
;; functions which aren't available yet.  Delay the expansion to
;; load-time (or later).
(eval '(progn " 
(" ;<-- this stops Emacs from indenting the next form

(defun peg-test ()
  (interactive)
  (assert (peg-parse-string ((s "a")) "a"))
  (assert (not (peg-parse-string ((s "a")) "b")))
  (assert (peg-parse-string ((s (not "a"))) "b"))
  (assert (not (peg-parse-string ((s (not "a"))) "a")))
  (assert (peg-parse-string ((s (if "a"))) "a"))
  (assert (not (peg-parse-string ((s (if "a"))) "b")))
  (assert (peg-parse-string ((s "ab")) "ab"))
  (assert (not (peg-parse-string ((s "ab")) "ba")))
  (assert (not (peg-parse-string ((s "ab")) "a")))
  (assert (peg-parse-string ((s (range ?0 ?9))) "0"))
  (assert (not (peg-parse-string ((s (range ?0 ?9))) "a")))
  (assert (peg-parse-string ((s [0-9])) "0"))
  (assert (not (peg-parse-string ((s [0-9])) "a")))
  (assert (not (peg-parse-string ((s [0-9])) "")))
  (assert (peg-parse-string ((s (any))) "0"))
  (assert (not (peg-parse-string ((s (any))) "")))
  (assert (peg-parse-string ((s (eob))) ""))
  (assert (peg-parse-string ((s (not (eob)))) "a"))
  (assert (peg-parse-string ((s (or "a" "b"))) "a"))
  (assert (peg-parse-string ((s (or "a" "b"))) "b"))
  (assert (not (peg-parse-string ((s (or "a" "b"))) "c")))
  (assert (peg-parse-string ((s (and "a" "b"))) "ab"))
  (assert (peg-parse-string ((s (and "a" "b"))) "abc"))
  (assert (not (peg-parse-string ((s (and "a" "b"))) "ba")))
  (assert (peg-parse-string ((s (and "a" "b" "c"))) "abc"))
  (assert (peg-parse-string ((s (* "a") "b" (eob))) "b"))
  (assert (peg-parse-string ((s (* "a") "b" (eob))) "ab"))
  (assert (peg-parse-string ((s (* "a") "b" (eob))) "aaab"))
  (assert (not (peg-parse-string ((s (* "a") "b" (eob))) "abc")))
  (assert (peg-parse-string ((s "")) "abc"))
  (assert (peg-parse-string ((s "" (eob))) ""))
  (assert (peg-parse-string ((s (opt "a") "b")) "abc"))
  (assert (peg-parse-string ((s (opt "a") "b")) "bc"))
  (assert (not (peg-parse-string ((s (or))) "ab")))
  (assert (peg-parse-string ((s (and))) "ab"))
  (assert (peg-parse-string ((s (and))) ""))
  (assert (peg-parse-string ((s ["^"])) "^"))
  (assert (peg-parse-string ((s ["^a"])) "a"))
  (assert (peg-parse-string ((s ["-"])) "-"))
  (assert (peg-parse-string ((s ["]-"])) "]"))
  (assert (peg-parse-string ((s ["^]"])) "^"))
  (assert (peg-parse-string ((s [alpha])) "z"))
  (assert (not (peg-parse-string ((s [alpha])) "0")))
  (assert (not (peg-parse-string ((s [alpha])) "")))
  (assert (not (peg-parse-string ((s ["][:alpha:]"])) "z")))
  (assert (peg-parse-string ((s (bob))) ""))
  (assert (peg-parse-string ((s (bos))) "x"))
  (assert (not (peg-parse-string ((s (bos))) " x")))
  (assert (peg-parse-string ((s "x" (eos))) "x"))
  (assert (peg-parse-string ((s (syntax-class whitespace))) " "))
  (assert (equal (peg-parse-string ((s `(-- 1 2))) "") '(t (2 1))))
  (assert (equal (peg-parse-string ((s `(-- 1 2) `(a b -- a b))) "")
		 '(t (2 1))))
  (assert (equal (peg-parse-string ((s (list x y))
				    (x `(-- 1))
				    (y `(-- 2)))
				   "")
		 '(t ((1 2)))))
  (assert (equal (peg-parse-string ((s (list (* x)))
				    (x "x" `(-- 'x)))
				   "xxx")
		 '(t ((x x x)))))
  (assert (equal (peg-parse-string ((s (region (* x)))
				    (x "x" `(-- 'x)))
				   "xxx")
		 '(t (4 x x x 1))))
  (assert (equal (peg-parse-string ((s (region (list (* x))))
				    (x "x" `(-- 'x 'y)))
				   "xxx")
		 '(t (4 (x y x y x y) 1))))
  (assert (equal (with-temp-buffer 
		   (save-excursion (insert "abcdef"))
		   (list
		    (peg-parse (x "a" 
				  (replace "bc" "x")
				  (replace "de" "y")
				  "f"))
		    (buffer-string)))
		 '((t nil) "axyf")))
  )

(when (featurep 'cl) 
  (peg-test))

;;; Examples:

;; peg-ex-recognize-int recognizes integers.  An integer begins with a
;; optional sign, then follows one or more digits.  Digits are all
;; characters from 0 to 9.
;;
;; Notes: 
;; 1) "" matches the empty sequence, i.e. matches without
;; consuming input.  
;; 2) [0-9] is the character range from 0 to 9.  This can also be
;; written as (range ?0 ?9). [Note that 0-9 is a symbol.]
(defun peg-ex-recognize-int ()
  (peg-parse (number   sign digit (* digit))
	     (sign     (or "+" "-" ""))
	     (digit    [0-9])))

;; peg-ex-parse-int recognizes integers and computes the corresponding
;; value.  The grammer is the same as for `peg-ex-recognize-int' added
;; with parsing actions.  Unfortunaletly, the actions add quite a bit
;; of clutter.
;;
;; The action for the sign rule pushes t on the stack for a minus sign
;; and nil for plus or no sign.
;;
;; The action for the digit rule pushes the value for a single digit.
;;
;; The action `(a b -- (+ (* a 10) b)), takes two items from the stack
;; and pushes the first digit times 10 added to second digit.
;;
;; The action `(minus val -- (if minus (- val) val)), negates the
;; value if the minus flag is true.
(defun peg-ex-parse-int ()
  (peg-parse (number sign
		     digit
		     (* digit `(a b -- (+ (* a 10) b)))
		     `(minus val -- (if minus (- val) val)))
	     (sign (or (and "+" `(-- nil))
		       (and "-" `(-- t))
		       (and "" `(-- nil))))
	     (digit [0-9] `(-- (- (char-before) ?0)))))

;; Put point after the ) and press C-x C-e
;; (peg-ex-parse-int)-234234

;; Parse arithmetic expressions and compute the result as side effect.
(defun peg-ex-arith ()
  (peg-parse 
   (expr (or (and ws sum eol)
	     (and (* (not eol) (any)) eol error)))
   (sum product (* (or (and plus product `(a b -- (+ a b)))
		       (and minus product `(a b -- (- a b))))))
   (product value (* (or (and times value `(a b -- (* a b)))
			 (and divide value `(a b -- (/ a b))))))
   (value (or (and (substring number) `(string -- (string-to-number string)))
	      (and open sum close)))
   (number (+ [0-9]) ws)
   (plus "+" ws)
   (minus "-" ws)
   (times "*" ws)
   (divide "/" ws)
   (open "(" ws)
   (close ")" ws)
   (ws (* (or " " "\t")))
   (eol (or "\n" "\r\n" "\r"))
   (error (action (error "Parse error at: %s" (point))))))

;; (peg-ex-arith)   1 + 2 * 3 * (4 + 5)

;; Parse URI according to RFC 2396.
(defun peg-ex-uri ()
  (peg-parse
   (URI-reference (or absoluteURI relativeURI) 
		  (or (and "#" (substring fragment))
		      `(-- nil))
		  `(scheme user host port path query fragment -- 
			   (list :scheme scheme :user user
				 :host host :port port
				 :path path :query query 
				 :fragment fragment)))
   (absoluteURI (substring scheme) ":" (or hier-part opaque-part))
   (hier-part ;(-- user host port path query)
    (or net-path 
	(and `(-- nil nil nil)
	     abs-path))
    (or (and "?" (substring query))
	`(-- nil)))
   (net-path "//" authority (or abs-path `(-- nil)))
   (abs-path "/" path-segments)
   (path-segments segment (list (* "/" segment)) `(s l -- (cons s l)))
   (segment (substring (* pchar) (* ";" param)))
   (param (* pchar))
   (pchar (or unreserved escaped [":@&=+$,"]))
   (query (* uric))
   (fragment (* uric))
   (relativeURI (or net-path abs-path rel-path) (opt "?" query))
   (rel-path rel-segment (opt abs-path))
   (rel-segment (+ unreserved escaped [";@&=+$,"]))
   (authority (or server reg-name))
   (server (or (and (or (and (substring userinfo) "@")
			`(-- nil))
		    hostport)
	       `(-- nil nil nil)))
   (userinfo (* (or unreserved escaped [";:&=+$,"])))
   (hostport (substring host) (or (and ":" (substring port))
				  `(-- nil)))
   (host (or hostname ipv4address))
   (hostname (* domainlabel ".") toplabel (opt "."))
   (domainlabel alphanum
		(opt (* (or alphanum "-") (if alphanum))
		     alphanum))
   (toplabel alpha
	     (* (or alphanum "-") (if alphanum))
	     alphanum)
   (ipv4address (+ digit) "." (+ digit) "." (+ digit) "." (+ digit))
   (port (* digit))
   (scheme alpha (* (or alpha digit ["+-."])))
   (reg-name (or unreserved escaped ["$,;:@&=+"]))
   (opaque-part uric-no-slash (* uric))
   (uric (or reserved unreserved escaped))
   (uric-no-slash (or unreserved escaped [";?:@&=+$,"]))
   (reserved (set ";/?:@&=+$,"))
   (unreserved (or alphanum mark))
   (escaped "%" hex hex)
   (hex (or digit [A-F] [a-f]))
   (mark (set "-_.!~*'()"))
   (alphanum (or alpha digit))
   (alpha (or lowalpha upalpha))
   (lowalpha [a-z])
   (upalpha [A-Z])
   (digit [0-9])))

;; (peg-ex-uri)file:/bar/baz.html?foo=df#x
;; (peg-ex-uri)http://luser@www.foo.com:8080/bar/baz.html?x=1#foo

;; Parse a lisp style Sexp.
;; [To keep the example short, ' and . are handled as ordinary symbol.]
(defun peg-ex-lisp ()
  (peg-parse 
   (sexp _ (or string list number symbol))
   (_ (* (or [" \n\t"] comment)))
   (comment ";" (* (not (or "\n" (eob))) (any)))
   (string "\"" (substring  (* (not "\"") (any))) "\"")
   (number (substring (opt (set "+-")) (+ digit))
	   (if terminating)
	   `(string -- (string-to-number string)))
   (symbol (substring (and symchar (* (not terminating) symchar)))
	   `(s -- (intern s)))
   (symchar [a-z A-Z 0-9 "-;!#%&'*+,./:;<=>?@[]^_`{|}~"])
   (list "("          	`(-- (cons nil nil)) `(hd -- hd hd)
	 (* sexp      	`(tl e -- (setcdr tl (list e)))
	    ) _ ")" 	`(hd tl -- (cdr hd)))
   (digit [0-9])
   (terminating (or (set " \n\t();\"'") (eob)))))

;; (peg-ex-lisp)

;; We try to detect left recursion and report it as error.
(defun peg-ex-left-recursion ()
  (eval '(peg-parse (exp (or term
			     (and exp "+" exp)))
		    (term (or digit
			      (and term "*" term)))
		    (digit [0-9]))))

(defun peg-ex-infinite-loop ()
  (eval '(peg-parse (exp (* (or "x"
				"y"
				(action (foo))))))))

)) ; end of eval-when-load

(provide 'peg)

;;; peg.el ends here
