;;; inf-yaqi.el --- an inferior-yaqi mode
;;
;; Maintainer: Henry G. Weller <hweller0@gmail.com>
;;
;; Created: Wed Aug 19 23:24:17 2009 (+0100)
;; Version: 0.1
;; Last-Updated: Wed Aug 19 23:24:17 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 1
;; URL:
;; Keywords: YAQi inf-mode
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; Based on inferior-lisp.
;;
;; This file defines a yaqi-in-a-buffer package (inferior-yaqi mode)
;; built on top of comint mode.
;;
;; Since this mode is built on top of the general command-interpreter-in-
;; a-buffer mode (comint mode), it shares a common base functionality,
;; and a common set of bindings, with all modes derived from comint mode.
;; This makes these modes easier to use.
;;
;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customising it, see the file comint.el.
;; For further information on inferior-yaqi mode, see the comments below.
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
  (require 'yaqi-mode))

;; -----------------------------------------------------------------------------
;;;  Custom variables

(defgroup inferior-yaqi nil
  "Run an outside YAQi in an Emacs buffer."
  :group 'yaqi
  :version "0.1")

;;;###autoload
(defcustom inferior-yaqi-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "*What not to save on inferior YAQi's input history.
Input matching this regexp is not saved on the input history in inferior YAQi
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inferior-yaqi)

;;;###autoload
(defcustom inferior-yaqi-program "yaqi"
  "*Program name for running YAQi."
  :type 'string
  :group 'inferior-yaqi)

;;;###autoload
(defcustom inferior-yaqi-load-command "(load \"%s\")\n"
  "*Format-string for building a YAQi expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a YAQi expression that will command YAQi
to load that file."
  :type 'string
  :group 'inferior-yaqi)

;;;###autoload
(defcustom inferior-yaqi-prompt "^[^> \n]*>+:? *"
  "*Regexp to recognise YAQi prompts.
Defaults to \"^[^> \\n]*>+:? *\".
This variable is used to initialize `comint-prompt-regexp' in the YAQi buffer.

This variable is only used if the variable
`comint-use-prompt-regexp-instead-of-fields' is non-nil."
  :type 'regexp
  :group 'inferior-yaqi)

;;;###autoload
(defcustom yaqi-source-modes '(yaqi-mode)
  "*Used to determine if a buffer contains YAQi source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a YAQi source file by `yaqi-load-file' and `yaqi-compile-file'.
Used by these commands to determine defaults."
  :type '(repeat symbol)
  :group 'inferior-yaqi)

(defvar inferior-yaqi-buffer nil "*The current inferior-yaqi process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple YAQi processes, you start the first up
with \\[inferior-yaqi].  It will be in a buffer named `*inferior-yaqi*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inferior-yaqi].  It will be in a new buffer,
named `*inferior-yaqi*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to YAQi processes --
like `yaqi-eval-defun' -- have to choose a process
to send to, when you have more than one YAQi process around.  This
is determined by the global variable `inferior-yaqi-buffer'.  Suppose you
have three inferior YAQis running:
    Buffer              Process
    foo                 inferior-yaqi
    bar                 inferior-yaqi<2>
    *inferior-yaqi*     inferior-yaqi<3>
If you do a \\[yaqi-eval-defun] command on some YAQi source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inferior-yaqi*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inferior-yaqi-buffer'.
This process selection is performed by function `inferior-yaqi-proc'.

Whenever \\[inferior-yaqi] fires up a new process, it resets
`inferior-yaqi-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you can change `inferior-yaqi-buffer' to another process
buffer with \\[set-variable].")

;;;###autoload
(defvar inferior-yaqi-mode-hook '()
  "*Hook for customising Inferior YAQi mode.")

(put 'inferior-yaqi-mode 'mode-class 'special)

(defun yaqi-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun yaqi-input-filter (str)
  "t if STR does not match `inferior-yaqi-filter-regexp'."
  (not (string-match inferior-yaqi-filter-regexp str)))

;;;###autoload
(defun inferior-yaqi (cmd)
  "Run an inferior YAQi process, input and output via buffer `*inferior-yaqi*'.
If there is a process already running in `*inferior-yaqi*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-yaqi-program').  Runs the hooks from
`inferior-yaqi-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
                         (read-string "Run yaqi: " inferior-yaqi-program)
                       inferior-yaqi-program)))
  (if (not (comint-check-proc "*inferior-yaqi*"))
      (let ((cmdlist (split-string cmd)))
        (set-buffer (apply (function make-comint)
                           "inferior-yaqi" (car cmdlist) nil (cdr cmdlist)))
        (inferior-yaqi-mode)))
  (setq inferior-yaqi-buffer "*inferior-yaqi*")
  (pop-to-buffer "*inferior-yaqi*"))
;;;###autoload (add-hook 'same-window-buffer-names "*inferior-yaqi*")

;;;###autoload
(defalias 'yaqi-shell 'inferior-yaqi)

(defun yaqi-eval-region (start end &optional and-go)
  "Send the current region to the inferior YAQi process.
Prefix argument means switch to the YAQi buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inferior-yaqi-proc) start end)
  (comint-send-string (inferior-yaqi-proc) "\n")
  (if and-go (yaqi-switch-to-yaqi-shell t)))

(defun yaqi-eval-defun (&optional and-go)
  "Send the current defun to the inferior YAQi process.
Prefix argument means switch to the YAQi buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f")
    (let ((end (point)))
      (beginning-of-defun)
      (yaqi-eval-region (point) end)))
  (if and-go (yaqi-switch-to-yaqi-shell t)))

(defun yaqi-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior YAQi process.
Prefix argument means switch to the YAQi buffer afterwards."
  (interactive "P")
  (yaqi-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

;;;  YAQi Compilation
(defun yaqi-compile-region (start end &optional and-go)
  "Compile the current region in the inferior YAQi process.
Prefix argument means switch to the YAQi buffer afterwards."
  (interactive "r\nP")
  (comint-send-string
   (inferior-yaqi-proc)
   (format "(funcall (compile nil `(lambda () (progn 'compile %s))))\n"
           (buffer-substring start end)))
  (if and-go (yaqi-switch-to-yaqi-shell t)))

(defun yaqi-compile-defun (&optional and-go)
  "Compile the current defun in the inferior YAQi process.
Prefix argument means switch to the YAQi buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((e (point)))
      (beginning-of-defun)
      (yaqi-compile-region (point) e)))
  (if and-go (yaqi-switch-to-yaqi-shell t)))

(defun yaqi-switch-to-yaqi-shell (eob-p)
  "Switch to the inferior YAQi process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-yaqi-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inferior-yaqi-buffer t))))
        (pop-to-buffer inferior-yaqi-buffer))
    (yaqi-shell inferior-yaqi-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defvar yaqi-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `yaqi-load-file' or `yaqi-compile-file' command.")

(defun yaqi-load-file (file-name)
  "Load a YAQi file into the inferior YAQi process."
  (interactive (comint-get-source "Load YAQi file: " yaqi-prev-l/c-dir/file
                                  yaqi-source-modes nil)) ; NIL because LOAD
                                        ; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq yaqi-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-yaqi-proc)
                      (format inferior-yaqi-load-command file-name))
  (yaqi-switch-to-yaqi-shell t))


(defun yaqi-compile-file (file-name)
  "Compile a YAQi file in the inferior YAQi process."
  (interactive (comint-get-source "Compile YAQi file: " yaqi-prev-l/c-dir/file
                                  yaqi-source-modes nil)) ; NIL = don't need
                                        ; suffix .yaqi
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq yaqi-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-yaqi-proc) (concat "(compile-file \""
                                                   file-name
                                                   "\"\)\n"))
  (yaqi-switch-to-yaqi-shell t))


;; -----------------------------------------------------------------------------
;;;  Documentation functions for functions, variables and symbols

(defvar yaqi-doc-command
  "(documentation %s)\n"
  "Command to query inferior YAQi for a function's or variable's documentation.")

(defvar yaqi-describe-command
  "(describe %s)\n"
  "Command to query inferior YAQi for a symbol's details.")


;; -----------------------------------------------------------------------------
;;;  Ancillary functions

;;;   Reads a string from the user.
(defun yaqi-symprompt (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))


;;;   Adapted from function-called-at-point in help.el.
(defun yaqi-fn-called-at-pt ()
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
(defun yaqi-var-at-pt ()
  (condition-case ()
      (save-excursion
        (forward-sexp -1)
        (skip-chars-forward "'")
        (let ((obj (read (current-buffer))))
          (and (symbolp obj) obj)))
    (error nil)))

;; Returns the current inferior YAQi process.
;; See variable `inferior-yaqi-buffer'."
(defun inferior-yaqi-proc ()
  (let ((proc (get-buffer-process
               (if (eq major-mode 'inferior-yaqi-mode)
                   (current-buffer)
                 inferior-yaqi-buffer))))
    (or proc
        (error "No YAQi subprocess; see variable `inferior-yaqi-buffer'"))))

;; -----------------------------------------------------------------------------
;;;  Documentation functions

(defun yaqi-documentation (fv)
  "Send a command to the inferior YAQi to give documentation for
function or variable FV.
See variable `yaqi-doc-command'."
  (interactive (yaqi-symprompt "Documentation" (yaqi-fn-called-at-pt)))
  (comint-proc-query (inferior-yaqi-proc)
                     (format yaqi-doc-command fv)))

(defun yaqi-describe (sym)
  "Send a command to the inferior YAQi to describe symbol SYM.
See variable `yaqi-describe-command'."
  (interactive (yaqi-symprompt "Describe" (yaqi-var-at-pt)))
  (comint-proc-query (inferior-yaqi-proc)
                     (format yaqi-describe-command sym)))

;; -----------------------------------------------------------------------------
;;;  Key-bindings

(defun inferior-yaqi-set-common-key-bindings (map)
  "Generic key-bindings common to both `inferior-yaqi-mode' and `yaqi-mode'
when the YAQi process is available to interrogate."
  (define-key map "\C-x\C-e" 'yaqi-eval-last-sexp)
  (define-key map "\C-c\C-e" 'yaqi-eval-defun)
  (define-key map "\C-c\C-r" 'yaqi-eval-region)
  (define-key map "\C-c\C-d" 'yaqi-compile-defun)
  (define-key map "\C-c\C-l" 'yaqi-load-file)
  (define-key map "\C-c\C-k" 'yaqi-compile-file)
  (define-key map "\C-c\C-s" 'yaqi-describe)
  (define-key map "\C-c\C-f" 'yaqi-documentation)
  (define-key map "\C-c\C-v" 'yaqi-documentation))

(defvar inferior-yaqi-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (set-keymap-parent map lisp-mode-shared-map)
    (inferior-yaqi-set-common-key-bindings map)
    map)
  "YAQi inferior mode key map which inherits the standard comint key map.")

;; -----------------------------------------------------------------------------
;;;  Menu

(defvar inferior-yaqi-mode-basic-menu-items
  '(["Evaluate Last S-expression" yaqi-eval-last-sexp
     :help "Evaluate sexp before point"]
    ["Evaluate Current Function" yaqi-eval-defun
     :help "Evaluate the function containing point."]
    ["Evaluate Region" yaqi-eval-region
     :help "Execute the region as YAQi code"
     :active mark-active]
    ["Compile Current Function" yaqi-compile-defun
     :help "Compile the function containing point."]
    ["Load File" yaqi-load-file
     :help "Load, compile and intern file"]
    ["Compile File" yaqi-compile-file
     :help "Compile file"]
    ["Describe Symbol" yaqi-describe
     :help "Describe symbol containing point"]
    ["Show Documentation" yaqi-documentation
     :help "Show documentation for symbol containing point"]
    ))

(easy-menu-define inferior-yaqi-menu inferior-yaqi-mode-map "YAQi-shell menu"
  (append
   '("YAQi" :help "YAQi inferior process specific Features")
   inferior-yaqi-mode-basic-menu-items))

;; -----------------------------------------------------------------------------
;;;  Inferior YAQi Mode

(define-derived-mode inferior-yaqi-mode comint-mode "Inferior YAQi"
  "Major mode for interacting with an inferior YAQi process
started with \\[yaqi-shell].

Runs a YAQi interpreter as a subprocess of Emacs, with YAQi I/O through an
Emacs buffer.  Variable `inferior-yaqi-program' controls which YAQi interpreter
is run.  Variables `inferior-yaqi-prompt', `inferior-yaqi-filter-regexp' and
`inferior-yaqi-load-command' can customize this mode for different YAQi
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-yaqi-buffer'.

Customisation: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-yaqi-mode-hook' (in that order).

You can send text to the inferior YAQi process from other buffers containing
YAQi source.
    yaqi-switch-to-yaqi-shell switches the current buffer to the YAQi process
        buffer.
    yaqi-eval-defun sends the current defun to the YAQi process.
    yaqi-compile-defun compiles the current defun.
    yaqi-eval-region sends the current region to the YAQi process.
    yaqi-compile-region compiles the current region.

    Prefixing the yaqi-eval/compile-defun/region commands with
    a \\[universal-argument] causes a switch to the YAQi process buffer after
    sending the text.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for YAQi; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{inferior-yaqi-mode-map}"
  :group 'yaqi

  ;; Setup the YAQi process parsing
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'yaqi-input-filter)
  (set (make-local-variable 'comint-prompt-regexp) 'inferior-yaqi-prompt)
  (set (make-local-variable 'comint-get-old-input) 'yaqi-get-old-input)

  ;; Font-lock the content of the YAQi process buffer
  (yaqi-font-lock)
  (font-lock-set-defaults)

  ;; Add YAQi inferior process related functionality to YAQi mode
  ;; YAQi source code can be evaluated.
  (inferior-yaqi-set-common-key-bindings yaqi-mode-map)
  (define-key yaqi-mode-map "\C-c\C-z" 'yaqi-switch-to-yaqi-shell)

  (easy-menu-define yaqi-menu yaqi-mode-map "YAQi Mode menu"
    (append
     '("YAQi" :help "YAQi-specific Features")
     yaqi-mode-basic-menu-items
     inferior-yaqi-mode-basic-menu-items))

  (run-hooks 'inferior-yaqi-mode-hook))


;; -----------------------------------------------------------------------------
;;;  Run load customisations

(defvar inferior-yaqi-load-hook nil
  "This hook is run when the library `inf-yaqi' is loaded.
This is a good place to put keybindings.")

(run-hooks 'inferior-yaqi-load-hook)

(provide 'inf-yaqi)

;; -----------------------------------------------------------------------------
;;; inf-yaqi.el  ends here
