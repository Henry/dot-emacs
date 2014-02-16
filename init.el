;;; init.el --- emacs initialisation top-level

;; -----------------------------------------------------------------------------
;;; Add personal lisp directory to the search path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-lisp/tree-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/paredit"))

;; -----------------------------------------------------------------------------
;;; Load the quick initialisation file
(load "qinit")

;; -----------------------------------------------------------------------------
;;; Personal key-map
(load "init-my-map")

;; -----------------------------------------------------------------------------
;;; Common initialisation settings
(load "init-common")

;; -----------------------------------------------------------------------------
;;; Personal key-bindings etc.
(load "init-personalisation")

;; -----------------------------------------------------------------------------
;;; Mouse configuration
(load "init-mouse")

;; -----------------------------------------------------------------------------
;;; Window navigation
(load "init-window-nav")

;; -----------------------------------------------------------------------------
;;; Controls for backup files
(load "init-backups")

;; -----------------------------------------------------------------------------
;;; Isearch configuration and extensions
(load "init-isearch")

;; -----------------------------------------------------------------------------
;;; flyspell: on-the-fly spell checking
(load "init-flyspell")

;; -----------------------------------------------------------------------------
;;; Calendar and Diary
(autoload 'calendar "init-diary" "Calendar/Diary" t)
(autoload 'autoload-init-diary "init-diary" "Calendar/Diary" t)
(autoload 'my-open-calendar "init-diary" "Calendar/Diary" t)

;; -----------------------------------------------------------------------------
;;; CEDET
;;(load "init-cedet")

;; -----------------------------------------------------------------------------
;;; OpenFOAM settings
(load "init-OpenFOAM")

;; -----------------------------------------------------------------------------
;;; ELisp settings
(load "init-elisp")

;; -----------------------------------------------------------------------------
;;; Lisp slime settings
;;(load "init-slime")

;; -----------------------------------------------------------------------------
;;; EuLisp settings
;;(load "init-eulisp")

;; -----------------------------------------------------------------------------
;;; GOOFIE settings
(load "init-goofie")

;; -----------------------------------------------------------------------------
;;; LaTeX settings
(load "init-latex")

;; -----------------------------------------------------------------------------
;;; Print settings
(load "init-printing")

;; -----------------------------------------------------------------------------
;;; Thing at point settings
(load "init-thingatpt")

;; -----------------------------------------------------------------------------
;;; Bookmark settings
(load "init-bookmark")

;; -----------------------------------------------------------------------------
;;; File-journal settings
(load "init-file-journal")

;; -----------------------------------------------------------------------------
;;; Run-time Initialisations
;; Initialisations which must be done at run-time, not built into eemacs:
;;     Windowing system properties which must be read at run-time
;;     Functions involving histories which must be read at run-time
(load "init-runtime" nil t) ;; Include in init-eemacs

;; -----------------------------------------------------------------------------
;;; Enable all save all histories function
(load "init-history-saving")

;; -----------------------------------------------------------------------------
;;; Ediff settings
(load "init-ediff")

;; -----------------------------------------------------------------------------
;;; Shell-mode settings
(load "init-shell")

;; -----------------------------------------------------------------------------
;;; Eshell-mode settings
;;(load "init-eshell")

;; -----------------------------------------------------------------------------
;;; GIT-Emacs: Emacs front-end for GIT repositories
(load "init-magit")

;; -----------------------------------------------------------------------------
;;; Insideous Big Brother Database
(load "init-bbdb")

;; -----------------------------------------------------------------------------
;;; gnuplot-mode
(load "init-gnuplot")

;; -----------------------------------------------------------------------------
;;; Org-mode: organiser
(load "init-org")

;; -----------------------------------------------------------------------------
;;; Wiki modes: modes for typesetting WIKI pages
(load "init-wiki")

;; -----------------------------------------------------------------------------
;;; outline-mode: outlining and folding
(load "init-outline")

;; -----------------------------------------------------------------------------
;;; Email client: Wanderlust
(load "init-wl")

;; -----------------------------------------------------------------------------
;;; wget: Asynchronous download of files
(load "init-wget")

;; -----------------------------------------------------------------------------
;;; Web-browser: w3m
(load "init-w3m")

;; -----------------------------------------------------------------------------
;;; Web-browser: general
(load "init-www")

;; -----------------------------------------------------------------------------
;;; openwith: Open files using specified application
(load "init-openwith")

;; -----------------------------------------------------------------------------
;;; oprofile: An oprofile callgraph viewer
;;(load "init-oprofile")

;; -----------------------------------------------------------------------------
;;; Tags: Initialise all things tags related
(load "init-tags")

;; -----------------------------------------------------------------------------
;;; Abbreviations
;;(autoload 'abbrev-mode "init-abbrev" "Abbrev" t)

;; -----------------------------------------------------------------------------
;;; Emms: Emacs Multimedia System
(autoload 'emms-browser "init-emms" "Emms" t)
(autoload 'emms-streams "init-emms" "Emms" t)

;; -----------------------------------------------------------------------------
;;; Dictionary and thesaurus
(load "init-dictionary")

;; -----------------------------------------------------------------------------
;;; Yet another snippets extension
;;(load "init-yasnippet")

;; -----------------------------------------------------------------------------
;;; Maxima interface
;;(load "init-imaxima")

;; -----------------------------------------------------------------------------
;;; completion-ui: Enhanced in-buffer completion packages
(load "init-completion")

;; -----------------------------------------------------------------------------
;;; Buffer selection menu
(load "init-ibuffer")

;; -----------------------------------------------------------------------------
;;; Anything: Show things that are now and have happened for selection
(load "init-anything")

;; -----------------------------------------------------------------------------
;;; ido: Interactive-do minibuffer completion packages
;;(load "init-ido")

;; -----------------------------------------------------------------------------
;;; Do Re Mi: Incremental change using arrow keys or mouse wheel
(load "init-doremi")

;; -----------------------------------------------------------------------------
;;; icicles: Enhanced minibuffer completion packages
(load "init-icicles")

;; -----------------------------------------------------------------------------
;;; E2WM
(load "init-e2wm")

;; -----------------------------------------------------------------------------
;;; Application generated customisations
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; -----------------------------------------------------------------------------
;;; init.el ends here
