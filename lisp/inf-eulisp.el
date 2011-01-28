;;; inf-eulisp.el --- an inferior-eulisp mode
;;
;; Maintainer: Henry G. Weller <hweller0@gmail.com>
;;
;; Created: Wed Aug 19 23:24:17 2009 (+0100)
;; Version: 0.1
;; Last-Updated: Wed Aug 19 23:24:17 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 1
;; URL:
;; Keywords: Eulisp inf-mode
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; Based on inferior-lisp.
;;
;; This file defines a eulisp-in-a-buffer package (inferior-eulisp mode)
;; built on top of comint mode.
;;
;; Since this mode is built on top of the general command-interpreter-in-
;; a-buffer mode (comint mode), it shares a common base functionality,
;; and a common set of bindings, with all modes derived from comint mode.
;; This makes these modes easier to use.
;;
;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customising it, see the file comint.el.
;; For further information on inferior-eulisp mode, see the comments below.
;;
;; -----------------------------------------------------------------------------
;;; Change log:
;;
;; Version 0.1
;; * Initial release
;;
;; -----------------------------------------------------------------------------
;;; Code:

(require 'comint)

(eval-when-compile
  (require 'eulisp-mode))

;; -----------------------------------------------------------------------------
;;;  Custom variables

(defgroup inferior-eulisp nil
  "Run an outside Eulisp in an Emacs buffer."
  :group 'eulisp
  :version "0.1")

;;;###autoload
(defcustom inferior-eulisp-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "*What not to save on inferior Eulisp's input history.
Input matching this regexp is not saved on the input history in inferior Eulisp
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inferior-eulisp)

;;;###autoload
(defcustom inferior-eulisp-program "eulisp"
  "*Program name for running Eulisp."
  :type 'string
  :group 'inferior-eulisp)

;;;###autoload
(defcustom inferior-eulisp-load-command "(load \"%s\")\n"
  "*Format-string for building a Eulisp expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Eulisp expression that will command Eulisp
to load that file."
  :type 'string
  :group 'inferior-eulisp)

;;;###autoload
(defcustom inferior-eulisp-prompt "^[^> \n]*>+:? *"
  "*Regexp to recognise Eulisp prompts.
Defaults to \"^[^> \\n]*>+:? *\".
This variable is used to initialize `comint-prompt-regexp' in the Eulisp buffer.

This variable is only used if the variable
`comint-use-prompt-regexp-instead-of-fields' is non-nil."
  :type 'regexp
  :group 'inferior-eulisp)

;;;###autoload
(defcustom eulisp-source-modes '(eulisp-mode)
  "*Used to determine if a buffer contains Eulisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Eulisp source file by `eulisp-load-file' and `eulisp-compile-file'.
Used by these commands to determine defaults."
  :type '(repeat symbol)
  :group 'inferior-eulisp)

(defvar inferior-eulisp-buffer nil "*The current inferior-eulisp process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Eulisp processes, you start the first up
with \\[inferior-eulisp].  It will be in a buffer named `*inferior-eulisp*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inferior-eulisp].  It will be in a new buffer,
named `*inferior-eulisp*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Eulisp processes --
like `eulisp-eval-defun' -- have to choose a process
to send to, when you have more than one Eulisp process around.  This
is determined by the global variable `inferior-eulisp-buffer'.  Suppose you
have three inferior Eulisps running:
    Buffer              Process
    foo                 inferior-eulisp
    bar                 inferior-eulisp<2>
    *inferior-eulisp*     inferior-eulisp<3>
If you do a \\[eulisp-eval-defun] command on some Eulisp source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inferior-eulisp*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inferior-eulisp-buffer'.
This process selection is performed by function `inferior-eulisp-proc'.

Whenever \\[inferior-eulisp] fires up a new process, it resets
`inferior-eulisp-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you can change `inferior-eulisp-buffer' to another process
buffer with \\[set-variable].")

;;;###autoload
(defvar inferior-eulisp-mode-hook '()
  "*Hook for customising Inferior Eulisp mode.")

(put 'inferior-eulisp-mode 'mode-class 'special)

(defun eulisp-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun eulisp-input-filter (str)
  "t if STR does not match `inferior-eulisp-filter-regexp'."
  (not (string-match inferior-eulisp-filter-regexp str)))

;;;###autoload
(defun inferior-eulisp (cmd)
  "Run an inferior Eulisp process, input and output via buffer `*inferior-eulisp*'.
If there is a process already running in `*inferior-eulisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-eulisp-program').  Runs the hooks from
`inferior-eulisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
                         (read-string "Run eulisp: " inferior-eulisp-program)
                       inferior-eulisp-program)))
  (if (not (comint-check-proc "*inferior-eulisp*"))
      (let ((cmdlist (split-string cmd)))
        (set-buffer (apply (function make-comint)
                           "inferior-eulisp" (car cmdlist) nil (cdr cmdlist)))
        (inferior-eulisp-mode)))
  (setq inferior-eulisp-buffer "*inferior-eulisp*")
  (pop-to-buffer "*inferior-eulisp*"))
;;;###autoload (add-hook 'same-window-buffer-names "*inferior-eulisp*")

;;;###autoload
(defalias 'eulisp-shell 'inferior-eulisp)

(defun eulisp-eval-region (start end &optional and-go)
  "Send the current region to the inferior Eulisp process.
Prefix argument means switch to the Eulisp buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inferior-eulisp-proc) start end)
  (comint-send-string (inferior-eulisp-proc) "\n")
  (if and-go (eulisp-switch-to-eulisp-shell t)))

(defun eulisp-eval-defun (&optional and-go)
  "Send the current defun to the inferior Eulisp process.
Prefix argument means switch to the Eulisp buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f")
    (let ((end (point)))
      (beginning-of-defun)
      (eulisp-eval-region (point) end)))
  (if and-go (eulisp-switch-to-eulisp-shell t)))

(defun eulisp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Eulisp process.
Prefix argument means switch to the Eulisp buffer afterwards."
  (interactive "P")
  (eulisp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

;;;  Eulisp Compilation
(defun eulisp-compile-region (start end &optional and-go)
  "Compile the current region in the inferior Eulisp process.
Prefix argument means switch to the Eulisp buffer afterwards."
  (interactive "r\nP")
  (comint-send-string
   (inferior-eulisp-proc)
   (format "(funcall (compile nil `(lambda () (progn 'compile %s))))\n"
           (buffer-substring start end)))
  (if and-go (eulisp-switch-to-eulisp-shell t)))

(defun eulisp-compile-defun (&optional and-go)
  "Compile the current defun in the inferior Eulisp process.
Prefix argument means switch to the Eulisp buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((e (point)))
      (beginning-of-defun)
      (eulisp-compile-region (point) e)))
  (if and-go (eulisp-switch-to-eulisp-shell t)))

(defun eulisp-switch-to-eulisp-shell (eob-p)
  "Switch to the inferior Eulisp process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-eulisp-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inferior-eulisp-buffer t))))
        (pop-to-buffer inferior-eulisp-buffer))
    (eulisp-shell inferior-eulisp-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defvar eulisp-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `eulisp-load-file' or `eulisp-compile-file' command.")

(defun eulisp-load-file (file-name)
  "Load a Eulisp file into the inferior Eulisp process."
  (interactive (comint-get-source "Load Eulisp file: " eulisp-prev-l/c-dir/file
                                  eulisp-source-modes nil)) ; NIL because LOAD
                                        ; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq eulisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-eulisp-proc)
                      (format inferior-eulisp-load-command file-name))
  (eulisp-switch-to-eulisp-shell t))


(defun eulisp-compile-file (file-name)
  "Compile a Eulisp file in the inferior Eulisp process."
  (interactive (comint-get-source "Compile Eulisp file: " eulisp-prev-l/c-dir/file
                                  eulisp-source-modes nil)) ; NIL = don't need
                                        ; suffix .eulisp
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq eulisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-eulisp-proc) (concat "(compile-file \""
                                                   file-name
                                                   "\"\)\n"))
  (eulisp-switch-to-eulisp-shell t))


;; -----------------------------------------------------------------------------
;;;  Documentation functions for functions, variables and symbols

(defvar eulisp-doc-command
  "(documentation %s)\n"
  "Command to query inferior Eulisp for a function's or variable's documentation.")

(defvar eulisp-describe-command
  "(describe %s)\n"
  "Command to query inferior Eulisp for a symbol's details.")


;; -----------------------------------------------------------------------------
;;;  Ancillary functions

;;;   Reads a string from the user.
(defun eulisp-symprompt (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))


;;;   Adapted from function-called-at-point in help.el.
(defun eulisp-fn-called-at-pt ()
  "Returns the name of the function called in the current call.
The value is nil if it can't find one."
  (condition-case nil
      (save-excursion
        (save-restriction
          (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
          (backward-up-list 1)
          (forward-char 1)
          (let ((obj (read (current-buffer))))
            (and (symbolp obj) obj))))
    (error nil)))


;;;   Adapted from variable-at-point in help.el.
(defun eulisp-var-at-pt ()
  (condition-case ()
      (save-excursion
        (forward-sexp -1)
        (skip-chars-forward "'")
        (let ((obj (read (current-buffer))))
          (and (symbolp obj) obj)))
    (error nil)))

;; Returns the current inferior Eulisp process.
;; See variable `inferior-eulisp-buffer'."
(defun inferior-eulisp-proc ()
  (let ((proc (get-buffer-process
               (if (eq major-mode 'inferior-eulisp-mode)
                   (current-buffer)
                 inferior-eulisp-buffer))))
    (or proc
        (error "No Eulisp subprocess; see variable `inferior-eulisp-buffer'"))))

;; -----------------------------------------------------------------------------
;;;  Documentation functions

(defun eulisp-documentation (fv)
  "Send a command to the inferior Eulisp to give documentation for
function or variable FV.
See variable `eulisp-doc-command'."
  (interactive (eulisp-symprompt "Documentation" (eulisp-fn-called-at-pt)))
  (comint-proc-query (inferior-eulisp-proc)
                     (format eulisp-doc-command fv)))

(defun eulisp-describe (sym)
  "Send a command to the inferior Eulisp to describe symbol SYM.
See variable `eulisp-describe-command'."
  (interactive (eulisp-symprompt "Describe" (eulisp-var-at-pt)))
  (comint-proc-query (inferior-eulisp-proc)
                     (format eulisp-describe-command sym)))

;; -----------------------------------------------------------------------------
;;;  Key-bindings

(defun inferior-eulisp-set-common-key-bindings (map)
  "Generic key-bindings common to both `inferior-eulisp-mode' and `eulisp-mode'
when the Eulisp process is available to interrogate."
  (define-key map "\C-x\C-e" 'eulisp-eval-last-sexp)
  (define-key map "\C-c\C-e" 'eulisp-eval-defun)
  (define-key map "\C-c\C-r" 'eulisp-eval-region)
  (define-key map "\C-c\C-d" 'eulisp-compile-defun)
  (define-key map "\C-c\C-l" 'eulisp-load-file)
  (define-key map "\C-c\C-k" 'eulisp-compile-file)
  (define-key map "\C-c\C-s" 'eulisp-describe)
  (define-key map "\C-c\C-f" 'eulisp-documentation)
  (define-key map "\C-c\C-v" 'eulisp-documentation))

(defvar inferior-eulisp-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (set-keymap-parent map lisp-mode-shared-map)
    (inferior-eulisp-set-common-key-bindings map)
    map)
  "Eulisp inferior mode key map which inherits the standard comint key map.")

;; -----------------------------------------------------------------------------
;;;  Menu

(defvar inferior-eulisp-mode-basic-menu-items
  '(["Evaluate Last S-expression" eulisp-eval-last-sexp
     :help "Evaluate sexp before point"]
    ["Evaluate Current Function" eulisp-eval-defun
     :help "Evaluate the function containing point."]
    ["Evaluate Region" eulisp-eval-region
     :help "Execute the region as Eulisp code"
     :active mark-active]
    ["Compile Current Function" eulisp-compile-defun
     :help "Compile the function containing point."]
    ["Load File" eulisp-load-file
     :help "Load, compile and intern file"]
    ["Compile File" eulisp-compile-file
     :help "Compile file"]
    ["Describe Symbol" eulisp-describe
     :help "Describe symbol containing point"]
    ["Show Documentation" eulisp-documentation
     :help "Show documentation for symbol containing point"]
    ))

(easy-menu-define inferior-eulisp-menu inferior-eulisp-mode-map "Eulisp-shell menu"
  (append
   '("Eulisp" :help "Eulisp inferior process specific Features")
   inferior-eulisp-mode-basic-menu-items))

;; -----------------------------------------------------------------------------
;;;  Inferior Eulisp Mode

(define-derived-mode inferior-eulisp-mode comint-mode "Inferior Eulisp"
  "Major mode for interacting with an inferior Eulisp process
started with \\[eulisp-shell].

Runs a Eulisp interpreter as a subprocess of Emacs, with Eulisp I/O through an
Emacs buffer.  Variable `inferior-eulisp-program' controls which Eulisp interpreter
is run.  Variables `inferior-eulisp-prompt', `inferior-eulisp-filter-regexp' and
`inferior-eulisp-load-command' can customize this mode for different Eulisp
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-eulisp-buffer'.

Customisation: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-eulisp-mode-hook' (in that order).

You can send text to the inferior Eulisp process from other buffers containing
Eulisp source.
    eulisp-switch-to-eulisp-shell switches the current buffer to the Eulisp process
        buffer.
    eulisp-eval-defun sends the current defun to the Eulisp process.
    eulisp-compile-defun compiles the current defun.
    eulisp-eval-region sends the current region to the Eulisp process.
    eulisp-compile-region compiles the current region.

    Prefixing the eulisp-eval/compile-defun/region commands with
    a \\[universal-argument] causes a switch to the Eulisp process buffer after
    sending the text.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Eulisp; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{inferior-eulisp-mode-map}"
  :group 'eulisp

  ;; Setup the Eulisp process parsing
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'eulisp-input-filter)
  (set (make-local-variable 'comint-prompt-regexp) 'inferior-eulisp-prompt)
  (set (make-local-variable 'comint-get-old-input) 'eulisp-get-old-input)

  ;; Font-lock the content of the Eulisp process buffer
  (eulisp-font-lock)
  (font-lock-set-defaults)

  ;; Add Eulisp inferior process related functionality to Eulisp mode
  ;; Eulisp source code can be evaluated.
  (inferior-eulisp-set-common-key-bindings eulisp-mode-map)
  (define-key eulisp-mode-map "\C-c\C-z" 'eulisp-switch-to-eulisp-shell)

  (easy-menu-define eulisp-menu eulisp-mode-map "Eulisp Mode menu"
    (append
     '("Eulisp" :help "Eulisp-specific Features")
     eulisp-mode-basic-menu-items
     inferior-eulisp-mode-basic-menu-items))

  (run-hooks 'inferior-eulisp-mode-hook))


;; -----------------------------------------------------------------------------
;;;  Run load customisations

(defvar inferior-eulisp-load-hook nil
  "This hook is run when the library `inf-eulisp' is loaded.
This is a good place to put keybindings.")

(run-hooks 'inferior-eulisp-load-hook)

(provide 'inf-eulisp)

;; -----------------------------------------------------------------------------
;;; inf-eulisp.el  ends here
