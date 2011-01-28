;;; init-smart-tab.el --- Initialize Smart TAB completion
;; -----------------------------------------------------------------------------

(eval-when-compile (require 'cl))

(defun smart-tab-1 (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (if (smart-tab-must-expand prefix)
        (completion-selection-complete)
      (smart-indent)))
  )

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command))
  )

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>"))
  )


(defun smart-tab-2 ()
  "This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (completion-selection-complete))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (completion-selection-complete)
        (indent-for-tab-command))))
  )


(defun smart-tab-3 ()
  "This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region.  Else do
the normal indent command.  If that does not move the point and
the point is at the end of a symbol, expands it."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (completion-selection-complete))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (let ((p (point)))
        (indent-for-tab-command)
        (when (and (= p (point))
                   (not (bolp))
                   (looking-at "\\_>"))
          (completion-selection-complete)))))
  )


(defvar smart-tab-completion-functions
  '((emacs-lisp-mode lisp-complete-symbol)
    (lisp-mode slime-complete-symbol)
    (python-mode py-complete)
    (text-mode dabbrev-completion))
  "List of major modes in which to use a mode specific completion
  function.")

(defun get-completion-function()
  "Get a completion function according to current major mode."
  (let ((completion-function
         (second (assq major-mode smart-tab-completion-functions))))
    (if (null completion-function)
        'dabbrev-completion
      completion-function)))

(defun smart-tab-4 (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (if (smart-tab-must-expand prefix)
        (let ((dabbrev-case-fold-search t)
              (dabbrev-case-replace nil))
          (funcall (get-completion-function))))
    (smart-indent)))


;;(global-set-key [(tab)] 'indent-for-tab-command)
(global-set-key [(tab)] 'smart-tab-3)


;;;  Advise a keybinding
;; This code does so by listening to the lower-level input hooks and advising
;; the function about to be run.
(defmacro ad-add-advice-to-key (key expr)
  "Around advice the key KEY with expression EXPR. KEY should be
a key in the format accepted by key-binding and such, and EXPR an
expression of the same type as those required by around advices"
  `(add-hook
    'pre-command-hook
    (lambda ()
      (when (equal (this-command-keys-vector) ,key)
        (ad-add-advice
         this-command
         '(azerrswdf ;arbitrary advice name
           nil       ;not protected
           t         ;activated
           (lambda ()
             ,expr
             (ad-unadvise this-command)))
         'around
         'last)
        (ad-activate this-command)))))

;;;  Make TAB auto-complete if the point was not moved by the normal TAB command
;; and the point is at the end of a word
;; (ad-add-advice-to-key
;;  [(tab)]
;;  (let ((p (point)))
;;    ad-do-it
;;    (when (and (= p (point))
;;               (not (bolp))
;;               (looking-at "\\_>"))
;;      (completion-selection-complete))))


;; -----------------------------------------------------------------------------
;;; init-smart-tab.el ends here
