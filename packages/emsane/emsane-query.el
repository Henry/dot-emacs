;;(require 'emsane)

;; stuff that asks a user for values
;; "queries" are classes that encapsulate something to ask a user
;; this is mostly for asking a user to fill a slot in an object, which is where most prompting happens

;; there are other functions to ask thing, but i haven figured out what to do with them yet.
;; maybe everything can be queries, maybe not.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "query" classes

(defclass emsane-query (eieio-named
                        eieio-instance-tracker)
  ((tracking-symbol :initform 'emsane-query-list)
   (prompt :initarg :prompt :documentation "string to use as prompt for queries")
   (default :initarg :default  :documentation "this value will be the default if user doesnt choose another value")
   )
  
  "base class for queries.they live on an eieieo named object list")

(defclass emsane-query-object (emsane-query)
  ((object-type :initarg :object-type  :documentation "type of objects to ask for.")
)
  "ask for an object name")

(defclass emsane-query-integer (emsane-query)
  ()
  "ask for an integer"
  ;;TODO implement max and min limits. good for scanner resolution, and other parameters limited by scanner hw
  )

(defclass emsane-query-named-values (emsane-query)
  ((values :initarg :values  :documentation "candidate values to ask for.")
   (require-match :initarg :require-match :initform t))
  "ask for named values. the names will be prompted the values returned")


;;TODO refactor query-named-values and query-string
(defclass emsane-query-string (emsane-query)
  ((values :initarg :values :initform nil  :documentation "candidate strings to ask for.")
   (require-match :initarg :require-match :initform t))
  "ask for a string")

(defclass emsane-query-atom (emsane-query)
    ((values :initarg :values :initform nil  :documentation "candidate atoms to ask for."))
  "ask for a atom")


(defclass emsane-query-paper-size (emsane-query-named-values)
  ((sizes :initarg :sizes  :documentation "candidate sizes to ask for.")
      (require-match :initarg :require-match :initform nil))
  "ask for paper size, its a named value")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
;;methods

(defmethod emsane-query-default ((this emsane-query))
  (if (slot-boundp this :default)
      (oref this :default)));;TODO also check if its an expression to be evaluated(for page for instance)

(defmethod emsane-query-promptstring ((this emsane-query))
  (format "%s:" (oref this :prompt))
  )
(defmethod emsane-do-query ((this emsane-query-object))
  "ask for a object"
  (let*
      ((object-list-symbol (intern (format "emsane-%s-list" (oref this :object-type))))
       (object-history-symbol (intern (format "emsane-%s-history" (oref this :object-type))))
       (object-names (mapcar (lambda (x)  (oref x :object-name))  (eval object-list-symbol)))
       (final-object-name (ido-completing-read (emsane-query-promptstring this)
                                         object-names
                                         nil
                                         t
                                         nil
                                         object-history-symbol
                                         (emsane-query-default this)
                                         )))
    final-object-name
       ))

(defmethod emsane-do-query ((this emsane-query-integer))
  "ask for an integer"
  (read-number  (emsane-query-promptstring this) (emsane-query-default this)))

(defmethod emsane-do-query ((this emsane-query-named-values))
  ;;(defun emsane-ask (prompt values );;&optional validator
  "Ask with PROMPT to select from VALUES.
VALUES is an assoc list."
  (let*
      ((user-input (ido-completing-read (emsane-query-promptstring this)
                   (mapcar (lambda (x) (car x)) (oref this :values))
                   nil
                   (oref this :require-match)
                   nil
                   nil
                   (emsane-query-default this)))
       (user-assoc (cadr (assoc user-input
                                (oref this :values)))))
    (if (not (or (oref this :require-match)  user-assoc))
        user-input
      user-assoc)))



(defmethod emsane-do-query ((this emsane-query-atom))
  ;;(defun emsane-ask (prompt values );;&optional validator
  "Ask with PROMPT to select from VALUES.
VALUES is an assoc list."
  (let*
      ((user-input (ido-completing-read (emsane-query-promptstring this)
                   (mapcar (lambda (x) (symbol-name x)) (oref this :values))
                   nil
                   t
                   nil
                   nil
                   (emsane-query-default this))))
       (intern user-input)))


(defmethod emsane-do-query ((this emsane-query-string))
  "Ask with PROMPT to select from VALUES.
VALUES is a list of strings."
  (let*
      ((user-input (ido-completing-read (emsane-query-promptstring this)
                                    (oref this :values)
                   nil
                   (oref this :require-match)
                   nil
                   nil
                   (emsane-query-default this))))
    user-input))


(defmethod emsane-do-query ((this emsane-query-paper-size))
  (let* ((user-size (call-next-method))) ;;TODO re-do this until a proper size is returned!
    (if (consp user-size)
        user-size
      (emsane-parse-paper-size user-size (oref this :values)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defun emsane-ask-subsection ()
;;   "prompt for a subsection number"
;;   (read-number "subsection of next scan: "  (+ 1 emsane-subsection)))


;; (defun emsane-ask (prompt values );;&optional validator
;;   "Ask with PROMPT to select from VALUES.
;; VALUES is an assoc list."
;;   (completing-read (format "%s:" prompt)
;;                    (mapcar (lambda (x) (car x)) values)
;;                    nil)
;;   ;;TODO validate user input somehow.
;;   ;;its not perfectly obvious how, since "size" for instance can be a size string like "XxY" or "A4"
;;   ;; call "validator" here, if fail, re-read user input
;;   )


(defun emsane-read-values (template)
  "templage is a list.
cdr template is values which is a list of things to prompt for, return answers as a list.
car template is a format string"
  (let
      ((returnlist)
       (values (cdr template)))
    (while values
      (setq returnlist (cons (emsane-read-value (car values))
                             returnlist))
      (setq values (cdr values)))
    (reverse returnlist)))

(defun emsane-read-value (value-spec)
  "value-spec is (prompt-string type)"
  (let*
      ((prompt (car value-spec))
       (type (cadr value-spec)))
    (cond
     ;;TODO add some more convenient types. date in particular
     ((eq 'string type) (read-string prompt))
     ((eq 'char type) (read-char prompt))
     ((eq 'directory-name type) (read-directory-name prompt))
     ((eq 'number type) (read-number prompt)))))




(defun emsane-ask-buffer ()
  "ask for an emsane buffer"
  (read-buffer
   "Choose other Emsane buffer: "))


(provide 'emsane-query)

