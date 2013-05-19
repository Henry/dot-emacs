;;; init-OpenFOAM.el --- Initialize OpenFOAM specific settings

(require 'cc-mode)
(require 'ppindent)

;; -----------------------------------------------------------------------------
;;;  Set the auto-mode-alist for all C++ related files

(setq auto-mode-alist
      (
       append
       '(
         ("\\.C$"    . c++-mode)
         ("\\.H$"    . c++-mode)
         ("\\.l$"    . c++-mode)
         ("\\.L$"    . c++-mode)
         ("\\.y$"    . c++-mode)
         ("\\.Y$"    . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.hh$"   . c++-mode)
         ("\\.cxx$"   . c++-mode)
         ("\\.hxx$"   . c++-mode)
         ("\\.java$" . c++-mode)
         ("\\.cfg$" . c++-mode)
         ("\\.c$"    . c-mode)
         ("\\.h$"    . c-mode)
         ("ebrowse\\'" . ebrowse-tree-mode)
         )
       auto-mode-alist))

;; Better commenting/un-commenting
(define-key c-mode-map "\C-c\C-c" 'comment-dwim-line)
(define-key c++-mode-map "\C-c\C-c" 'comment-dwim-line)

(require 'git-grep)

(defun wmake (&rest args)
  "`wmake' script wrapper callable from `eshell' and directly."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal
   (concat "wdwim " (eshell-flatten-and-stringify args))
   "No more errors"))

(defun Allwmake ()
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal "./Allwmake" "No more errors"))

(defun wclean (&rest args)
  (interactive)
  (shell-command (concat "wclean" args)))

(defun insert-spaces ()
  (interactive)
  (insert "    "))


(defvar OPENFOAM_DIR (getenv "WM_PROJECT_DIR"))
(defvar OPENFOAM_TAGS_DIR (concat OPENFOAM_DIR "/.tags"))
(defvar OpenFOAM-semantic-active-flag nil)
(defvar OpenFOAM-ecb-active-flag nil)

(defun OpenFOAM-make-tags ()
  "Create/update the tags files for etags, gtags and ebrowse."
  (interactive)
  (call-process "foamTags"))

(defun OpenFOAM-initialise-gtags ()
  "Set the location of the gtags tables and initialise xgtags."
  (setenv "GTAGSDBPATH" OPENFOAM_TAGS_DIR)
  (setenv "GTAGSROOT" OPENFOAM_DIR)
  (setq xgtags-rootdir OPENFOAM_DIR)
  (xgtags-mode 1))

(defun OpenFOAM-ebrowse ()
  "Run ebrowse on the table created by `OpenFOAM-make-tags'."
  (interactive)
  (find-file (concat OPENFOAM_TAGS_DIR "/ebrowse")))

(defun OpenFOAM-dec ()
  "Use `etags' to find the declaration of the tag at point."
  (interactive)
  (visit-tags-table (concat OPENFOAM_TAGS_DIR "/etagsDec"))
  (etags-select-find-tag))

(defun OpenFOAM-def ()
  "Use `etags' to find a definition of the tag at point."
  (interactive)
  (visit-tags-table (concat OPENFOAM_TAGS_DIR "/etagsDef"))
  (etags-select-find-tag))

(defun OpenFOAM-tag ()
  "Use `etags' to find the definition or declaration of the tag at point."
  (interactive)
  (visit-tags-table (concat OPENFOAM_TAGS_DIR "/etags"))
  (etags-select-find-tag))

(defun OpenFOAM-use ()
  "Use `gtags' to find all the usages of the tag at point."
  (interactive)
  (OpenFOAM-initialise-gtags)
  (xgtags-find-rtag))

(defun OpenFOAM-sym ()
  "Use `gtags' to find all the usages of the symbol at point."
  (interactive)
  (OpenFOAM-initialise-gtags)
  (xgtags-find-symbol))

(defun OpenFOAM-tags ()
  "Load the etags and gtags tables and setup the `xgtags' minor mode."
  (interactive)
  ;; Set the etags table to use
  (visit-tags-table (concat OPENFOAM_TAGS_DIR "/etags"))
  (OpenFOAM-initialise-gtags)
  (xgtags-make-complete-list))

(defun OpenFOAM-dec-sig ()
  "Use `ectags' to find the declaration of the tag at point."
  (interactive)
  (unless ectags-obarray
    (ectags-visit-tags-table (concat OPENFOAM_TAGS_DIR "/ectagsDec")))
  (ectags-select-find-tag))

(defun OpenFOAM-activate-semantic ()
  "Activate semantic for the current session"
  (interactive)
  (cond
   ((null OpenFOAM-semantic-active-flag)

    ;; Set paths for global gtags
    (setenv "GTAGSDBPATH" OPENFOAM_TAGS_DIR)
    (setenv "GTAGSROOT" OPENFOAM_DIR)

    ;; Add support for global gtags in the c++ mode ...
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    ;; ... and for this buffer
    (semanticdb-enable-gnu-global-in-buffer t)

    ;; Add support for exuberant ctags in the c++ mode ...
    (semantic-load-enable-all-exuberent-ctags-support)

    ;;(global-srecode-minor-mode 1)
    ;;(global-semantic-mru-bookmark-mode 1)
    ;;(which-func-mode 1)
    ;; This enables the auto-completion part of guady-code-helpers
    ;;(global-semantic-idle-completions-mode 1)
    ;;(semantic-load-enable-code-helpers)
    (semantic-load-enable-excessive-code-helpers)
    ;;(semantic-load-enable-guady-code-helpers)

    ;;(OpenFOAM-semantic-create-ebrowse)
    ;;(semanticdb-load-ebrowse-caches)

    ;;(c++-mode) ;; Re-run the mode hooks to setup semantic
    (if (equal OpenFOAM-ecb-active-flag t)
        (ecb-layout-switch "OF-2-m")))))

(defun OpenFOAM-deactivate-semantic ()
  "Deactivate semantic for the current session"
 (interactive)
  (cond
   ((equal OpenFOAM-semantic-active-flag t)
    (global-semantic-idle-scheduler-mode -1)
    (global-semanticdb-minor-mode -1)
    (global-semantic-idle-summary-mode -1)
    (global-senator-minor-mode -1)
    (global-semantic-decoration-mode -1)
    (global-semantic-stickyfunc-mode -1)
    (global-semantic-idle-completions-mode -1)
    (which-func-mode -1)
    (global-srecode-minor-mode -1)
    (global-semantic-mru-bookmark-mode -1)
    ;;(global-ede-mode nil)
    (if (equal OpenFOAM-ecb-active-flag t)
        (ecb-layout-switch "OF-2")))))

(defun OpenFOAM-semantic-create-ebrowse ()
  "Create and create the `semantic' database for `ebrowse'."
  (interactive)
  (setq semanticdb-ebrowse-file-match "\\.H")
  (dolist (path ecb-source-path)
    (semanticdb-create-ebrowse-database (car path))))

(defun OpenFOAM-semantic ()
  "Toggle the active status of semantic"
  (interactive)
  (cond
   ((null OpenFOAM-semantic-active-flag)
    (OpenFOAM-activate-semantic)
    (setq OpenFOAM-semantic-active-flag t))
   (t
    (OpenFOAM-deactivate-semantic)
    (setq OpenFOAM-semantic-active-flag nil))))

(defun OpenFOAM-activate-ecb ()
  "Activate ECB for the current session"
  (interactive)
  (cond
   ((null OpenFOAM-ecb-active-flag)
    (if (equal OpenFOAM-semantic-active-flag t)
        (ecb-layout-switch "OF-2-m")
      (ecb-layout-switch "OF-2"))
    (ecb-activate-2)
    (setq OpenFOAM-ecb-active-flag t))))

(defun OpenFOAM-deactivate-ecb ()
  "Deactivate ECB for the current session"
  (interactive)
  (cond
    ((equal OpenFOAM-ecb-active-flag t)
     (ecb-deactivate-2)
     (setq OpenFOAM-ecb-active-flag nil))))

(defun OpenFOAM-ecb ()
  "Toggle the active status of ecb"
  (interactive)
  (cond
   ((null OpenFOAM-ecb-active-flag)
    (OpenFOAM-activate-ecb)
    (setq OpenFOAM-ecb-active-flag t))
   (t
    (OpenFOAM-deactivate-ecb)
    (setq OpenFOAM-ecb-active-flag nil))))


(defun foam-create-C-file (className)
  (interactive "sclass name: ") ;   which is read with the Minibuffer.
  (shell-command
   (format "foamNew source C %s" className))
  (shell-command
   (format "edit %s.C" className)))

(defun foam-create-H-file (className)
  (interactive "sclass name: ") ;   which is read with the Minibuffer.
  (shell-command
   (format "foamNew source H %s" className))
  (shell-command
   (format "edit %s.H" className)))

(defun foam-create-I-file (className)
  (interactive "sclass name: ") ;   which is read with the Minibuffer.
  (shell-command
   (format "foamNew source I %s" className))
  (shell-command
   (format "edit %sI.H" className)))

(defun foam-create-IO-file (className)
  (interactive "sclass name: ") ;   which is read with the Minibuffer.
  (shell-command
   (format "foamNew source IO %s" className))
  (shell-command
   (format "edit %sIO.C" className)))

(defun foam-create-application-file (className)
  (interactive "sapplication name: ") ;   which is read with the Minibuffer.
  (shell-command
   (format "foamNew source App %s" className))
  (shell-command
   (format "edit %s.C" className)))

(defun foam-create-template-C-file (className templateArguments)
  (interactive "sclass name: \nstemplate args: ")
  (shell-command
   (format "foamNew template C %s %s" className templateArguments))
  (shell-command
   (format "edit %s.C" className)))

(defun foam-create-template-H-file (className templateArguments)
  (interactive "sclass name: \nstemplate args: ")
  (shell-command
   (format "foamNew template H %s %s" className templateArguments))
  (shell-command
   (format "edit %s.H" className)))

(defun foam-create-template-I-file (className templateArguments)
  (interactive "sclass name: \nstemplate args: ")
  (shell-command
   (format "foamNew template I %s %s" className templateArguments))
  (shell-command
   (format "edit %sI.H" className)))

(defun foam-create-template-IO-file (className templateArguments)
  (interactive "sclass name: \nstemplate args: ")
  (shell-command
   (format "foamNew template IO %s %s" className templateArguments))
  (shell-command
   (format "edit %sIO.C" className)))

(defun foam-create-wmake-files-options ()
  (interactive) ;   which is read with the Minibuffer.
  (shell-command "wmakeFilesAndOptions")
  (shell-command "edit Make/files")
  (shell-command "edit Make/options"))

(defconst OpenFOAM-style
  '(
    (c-basic-offset . 4)
    (c-tab-always-indent . t)
    (c-comment-only-line-offset . (0 . 0))
    (c-indent-comments-syntactically-p . t)
    (c-block-comments-indent-p . nil)
    ;;(comment-style . 'multi-line)
    (comment-start . "// ")
    (comment-end . "")
    (c-cleanup-list . '((defun-close-semi) (list-close-comma) (scope-operator)))
    (c-backslash-column . 79)
    (c-backslash-max-column . 79)
    (c-auto-align-backslashes . t)
    (c-toggle-auto-state . 1)
    (c-toggle-auto-hungry-state . 1)

    (c-offsets-alist
     ;;(c . +)                     ;; inside a multi-line C style block comment
     (defun-open . 0)            ;; brace that opens a function definition
     (defun-close . 0)           ;; brace that closes a function definition
     (defun-block-intro . +)     ;; the first line in a top-level defun
     (class-open . 0)            ;; brace that opens a class definition
     (class-close . 0)           ;; brace that closes a class definition
     (inline-open . 0)           ;; brace that opens an in-class inline method
     (inline-close . 0)          ;; brace that closes an in-class inline method
     (topmost-intro . 0)         ;; the first line in a topmost construct
     ;; definition
     (topmost-intro-cont . 0)    ;; topmost definition continuation lines
     (member-init-intro . +)     ;; first line in a member initialization list
     (member-init-cont . 0)      ;; subsequent member initialization list lines
     (inher-intro . 0)           ;; first line of a multiple inheritance list
     (inher-cont . +)            ;; subsequent multiple inheritance lines
     (block-open . 0)            ;; statement block open brace
     (block-close . 0)           ;; statement block close brace
     (brace-list-open . 0)       ;; open brace of an enum or static array list
     (brace-list-close . 0)      ;; open brace of an enum or static array list
     (brace-list-intro . +)      ;; first line in an enum or static array list
     (brace-list-entry . 0)      ;; subsequent lines in an enum or static array
     ;; list
     (statement . 0)             ;; a C/C++/ObjC statement
     (statement-cont . 0)        ;; a continuation of a C/C++/ObjC statement
     (statement-block-intro . +) ;; the first line in a new statement block
     (statement-case-intro . +)  ;; the first line in a case `block'
     (statement-case-open . +)   ;; the first line in a case `block'
     ;; starting with brace
     (substatement . +)          ;; the first line after an if/while/for/do/else
     (substatement-open . 0)     ;; the brace that opens a substatement block
     (case-label . +)            ;; a case or default label
     (access-label . -)          ;; C++ private/protected/public access label
     (label . -)                 ;; any non-special C/C++/ObjC label
     (do-while-closure . 0)      ;; the `while' that ends a do/while construct
     (else-clause . 0)           ;; the `else' of an if/else construct
     (comment-intro . 0)         ;; line containing only a comment introduction
     (arglist-intro . +)         ;; the first line in an argument list
     (arglist-cont . 0)          ;; subsequent argument list lines when no
     ;; subsequent argument list lines when no the
     ;; arglist opening paren
     (arglist-cont-nonempty . 0) ;; subsequent argument list lines when at
     ;; subsequent argument list lines when at line
     ;; as the arglist opening paren
     (arglist-close . 0)         ;; line as the arglist opening paren
     (stream-op . +)             ;; lines continuing a stream operator construct
     (inclass . +)               ;; the construct is nested inside a class
     ;; definition
     (cpp-macro . 0)             ;; the construct is nested inside a class
     ;; definition
     (friend . 0)                ;; a C++ friend declaration
     )
    )
  "OpenFOAM C++ Programming Style"
  )


(defun OpenFOAM-mode-hook ()

  (setq OPENFOAM_DIR (getenv "WM_PROJECT_DIR"))

  (c-add-style "OpenFOAM" OpenFOAM-style t)

  ;;(make-local-variable 'comment-start-skip)
  ;;(setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *") orig
  ;;(setq comment-start-skip "\\(//\\-+\\|/\\*+\\)\\s *")
  ;;(setq comment-start-skip "\\(//.[^ ]\\|/\\*+|/\\-\\)\\s *")

  (define-key c++-mode-map [backtab] 'insert-spaces)

  ;(setq special-display-buffer-names
  ;  (append special-display-buffer-names (list "*compilation*"))
  ;)

  (font-lock-mode 1)

  ;; Show trailing whitespace, tabs and lines > 80
  (whitespace-mode 1)

  ;; Show matching parentheses
  (show-matching-paren)

  ;; Switch off abbrev mode which cc-mode switches on
  (abbrev-mode -1)

  ;; Switch off "electric" indent mode which cc-mode switches on
  ;; This mode automatically indents line when "punctuation" is typed
  (setq-default c-electric-flag nil)

  ;; Switch on fly-spell mode in comments
  (flyspell-prog-mode)

  ;; Switch on auto-fill in comments
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face))))

  ;; Switch on the completion selection mode
  ;; and set the default completion-selection to dabbrev
  (completion-selection-mode t)
  (completion-selection-set 'complete-dabbrev-ordered)

  ;; Set the compile window to scroll as the output is generated
  (setq compilation-scroll-output t)

  ;; Map build commands to function keys
  (define-key c++-mode-map [f5] 'wmake)
  (define-key c++-mode-map [f6] 'Allwmake)
  (define-key c++-mode-map [f7] 'wclean)

  (require 'compile)

  (easy-menu-define
    OpenFOAM-menu c++-mode-map "OpenFOAM"
   '("OpenFOAM"
      ["ECB" OpenFOAM-ecb
       :style toggle :selected OpenFOAM-ecb-active-flag]
      ["Senator" OpenFOAM-semantic
       :style toggle :selected OpenFOAM-semantic-active-flag]
      ["Make Tags" OpenFOAM-make-tags]
      ["Tags" OpenFOAM-tags]
      ["Ebrowse" OpenFOAM-ebrowse]
      "---"
      ("Find Tag..."
       ["Declaration" OpenFOAM-dec]
       ["Dec Sig" OpenFOAM-dec-sig]
       ["Definition" OpenFOAM-def]
       ["Tag" OpenFOAM-tag]
       ["Use" OpenFOAM-use]
       ["Symbol" OpenFOAM-sym]
       )
      "---"
      ["wmake" wmake]
      ["Allwmake" Allwmake]
      ["wclean" wclean t]
      "---"
      ("New source file"
        ["{class name}.H" foam-create-H-file]
        ["{class name}.C" foam-create-C-file]
        ["{class name}I.H" foam-create-I-file]
        ["{class name}IO.C" foam-create-IO-file]
      )
      ("New templated source file"
        ["{class name}.H" foam-create-template-H-file]
        ["{class name}.C" foam-create-template-C-file]
        ["{class name}I.H" foam-create-template-I-file]
        ["{class name}IO.C" foam-create-template-IO-file]
      )
      ["New application" foam-create-application-file]
      ["New Make/files, options" foam-create-wmake-files-options]
    )
  )
  (easy-menu-add OpenFOAM-menu)

;;   ;; Set the CEDET project details
;;   (add-to-list 'semanticdb-project-roots OPENFOAM_DIR)

;;   (ede-cpp-root-project
;;    "OpenFOAM"
;;    :name "OpenFOAM"
;;    :version (getenv "WM_PROJECT_DIR")
;;    :file (concat OPENFOAM_DIR "/README.org")
;;    :include-path '( "/src/OpenFOAM/lnInclude" "/src/finiteVolume/lnInclude" ))

  ;; Set the ECB source paths
  ;;(add-to-list 'ecb-source-path
  ;;             (list (concat OPENFOAM_DIR "/src/OpenFOAM")
  ;;                   "OpenFOAM"))
  (add-to-list 'ecb-source-path
               (list (concat OPENFOAM_DIR "/src/OpenFOAM/lnInclude")
                     "OpenFOAMlnInclude"))
  ;;(add-to-list 'ecb-source-path
  ;;             (list (concat OPENFOAM_DIR "/src/finiteVolume")
  ;;                   "finiteVolume"))
  (add-to-list 'ecb-source-path
               (list (concat OPENFOAM_DIR "/src/finiteVolume/lnInclude")
                     "finiteVolumelnInclude"))

  (c-set-style "openfoam")
)

(add-hook 'c-mode-common-hook 'OpenFOAM-mode-hook)


(defun c-open-relational-file (how-open-type)
  (interactive "nOpen-Type: ")
  (defun get-opened-file-name-prefix (file-name)
    (string-match "/\\([^./]+\\)\\.[^.]+$" file-name)
    (match-string 1 file-name))
  (defun get-ext-type (file-name)
    (string-match "\\.\\([^.]+\\)$" file-name)
    (match-string 1 file-name))
  (defun get-opening-file-name (file-name-prefix ext-list)
    (let ((opening-file-name (concat file-name-prefix "." (car ext-list))))
      (cond ((null (car ext-list))             nil)
            ((file-exists-p opening-file-name) opening-file-name)
            (t                                 (get-opening-file-name file-name-prefix
                                                                      (cdr ext-list))))))
  (let* ((ext-map '(
                    ("h" . ("c" "cpp" "cxx" "cc" "c++"))
                    ("c" . ("h" "s"))
                    ("s" . ("c"))
                    ("cpp" . ("hpp" "h" "hxx" "h++"))
                    ("hpp" . ("cpp" "cxx" "cc" "c++"))
                    ))
         (opened-file-name (buffer-file-name (window-buffer)))
         (opened-file-name-prefix (get-opened-file-name-prefix opened-file-name))
         (opened-file-ext-type (get-ext-type opened-file-name))
         (opening-file-ext-type-list (cdr (assoc opened-file-ext-type ext-map)))
         (opening-file-name (get-opening-file-name opened-file-name-prefix
                                                   opening-file-ext-type-list))
         (opening-file-buffer (find-file-noselect opening-file-name)))
    (cond ((= how-open-type 1) (switch-to-buffer opening-file-buffer))
          ((= how-open-type 2) (progn (split-window-horizontally)
                                      (other-window 1)
                                      (switch-to-buffer opening-file-buffer)))
          (t                   (message "Illegal Type")))))


;; -----------------------------------------------------------------------------
;;;  c-mode

(defun my-c-mode-hook ()

  ;; Set the regexp used by outline-mode to find the headings
  (set (make-local-variable 'outline-regexp) "///[ ]+\\|(......")

  ;; Remove the number of "\\\" from the outline-level
  (set (make-local-variable 'outline-level) '(lambda () (- (outline-level) 3)))

  ;; Set the heading levels for promotion and demotion
  (setq outline-promotion-headings '("/// " "///  " "///   "))

  ;; Turn on outline minor mode by default
  (outline-minor-mode +1)
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)


;; -----------------------------------------------------------------------------
;;;  compilation-mode

(defun my-compilation-mode-hook ()
  (font-lock-mode 1)
  )

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)


;; -----------------------------------------------------------------------------
;;;  auto-complete-clang-async

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/popup-el"))
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/auto-complete"))
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/packages/emacs-clang-complete-async"))

(require 'auto-complete-config)
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-sources '(ac-source-clang-async))
  (launch-completion-proc)
  )

;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)

(defun my-clang-complete ()
  (interactive)
  (setq ac-sources '(ac-source-clang-async))
  (launch-completion-proc)
  (global-auto-complete-mode t))

(setq ac-clang-flags
      '("-DWM_DP"
        "-I/home/dm2/henry/OpenFOAM/OpenFOAM-dev/src/OpenFOAM/lnInclude"
        "-I."))


;; (require 'auto-complete-clang)

;; (setq ac-auto-start nil)
;; (setq ac-quick-help-delay 0.5)
;; ;; (ac-set-trigger-key "TAB")
;; ;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
;; (defun my-ac-config ()
;;   (interactive)
;;   (setq-default ac-sources
;;                 '(ac-source-abbrev
;;                   ac-source-dictionary
;;                   ac-source-words-in-same-mode-buffers))
;;   (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;   ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;   (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))
;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources
;;     (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
;; (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

;; -----------------------------------------------------------------------------
;;; init-OpenFOAM.el ends here
