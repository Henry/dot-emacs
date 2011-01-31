;;; init-anything.el ---  Initialize the anything package
;; -----------------------------------------------------------------------------
;;;  Anything: Show things that are now and have happened for selection

(provide 'anything-config-ext)

(add-to-list 'load-path (expand-file-name
                         "~/.emacs.d/packages/anything-config"))
(require 'anything)
(require 'anything-config)
(require 'anything-gtags)
(require 'anything-match-plugin)
;;(require 'anything-complete)
;;(require 'anything-traverse)

(add-to-list 'load-path (expand-file-name
                         "~/.emacs.d/packages/icicles"))
(require 'lacarte)

;; -----------------------------------------------------------------------------
;;; Create special sources

;; -----------------------------------------------------------------------------
;;;  Switch to a file in the file-journal

(defvar anything-c-source-file-journal
  '((name . "File Journal")
    (candidates . (lambda () (reduce 'append (mapcar 'cdr fj-journal))))
    (volatile)
    (type . file))
  "Source for `file-journal'.")

;; -----------------------------------------------------------------------------
;;;  Files in a specified directory

(defun anything-c-transform-file-name-nondirectory (files)
  (mapcar (lambda (f) (cons (file-name-nondirectory f) f)) files))

(defun anything-c-source-files-in-dir
  (desc dir &optional match skip-opened-file)
  `((name . ,desc)
    (candidates . (lambda () (directory-files ,dir t ,match)))
    (candidate-transformer
     . (lambda (candidates)
         (anything-c-compose (list candidates)
                             '(,@(if skip-opened-file
                                     (list 'anything-c-skip-opened-files))
                               anything-c-transform-file-name-nondirectory))))
    (type . file)))

(setq anything-c-source-elinit
      (anything-c-source-files-in-dir
       "Emacs init files" "~/.emacs.d/" "^.*\.el$"))

;; -----------------------------------------------------------------------------
;;;  Switch to one of a predefined set of directories

(defvar anything-c-source-switch-dir
  '((name . "Switch Directory")
    (candidates . switch-dir-alist)
    (action ("Change directory" . cd))))

(setq switch-dir-alist
      '(("OpenFOAM-dev" . "~/OpenFOAM/OpenFOAM-dev")
        ("OpenFOAM" . "~/OpenFOAM/OpenFOAM-dev/src/OpenFOAM/lnInclude")
        ))

;; -----------------------------------------------------------------------------
;;;  If exact match isn't found allow creation of buffer or search

(defvar anything-c-source-no-exact-match-in-buffers
  '((name . "No Exact Match")
    (candidates
     . (lambda ()
         (unless (member anything-input (mapcar 'buffer-name (buffer-list)))
           (list (cons (concat "Create or find "
                               "'" anything-input "'")
                       anything-input)))))
    (action . (("Create Buffer" . switch-to-buffer)
               ("Find File" .
                (lambda (file-name)
                  (setq icicle-initial-value file-name)
                  (icicle-file nil)))))
    (requires-pattern . 1)
    (volatile)))

;; -----------------------------------------------------------------------------
;;;  ELisp info

(defvar traverse-example-directory "~/.emacs.d/lisp")

(defvar anything-c-info-elisp-new nil)
(defvar anything-c-source-info-elisp-new
  `((name . "Info Elisp")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-elisp-new
                  (with-temp-buffer
                    (Info-find-node "elisp" "Index")
                    (setq anything-c-info-elisp-new
                          (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-elisp-new
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (("Goto Info Node" . (lambda (candidate)
                                     (Info-find-node "elisp" "Index")
                                     (Info-index
                                      (replace-regexp-in-string
                                       "* " "" candidate))))
               ("Find Example" . (lambda (candidate)
                                   (and (fboundp 'traverse-deep-rfind)
                                        (traverse-deep-rfind
                                         traverse-example-directory
                                         (replace-regexp-in-string
                                          "* " "" candidate)
                                         ".el"))))))
    (volatile)
    (requires-pattern . 2)))

;; -----------------------------------------------------------------------------
;;; Emacs variables

(define-anything-type-attribute 'variable
  '((action ("Describe variable" . anything-c-describe-variable)
            ("Add variable to kill ring" . anything-c-kill-new)
            ("Go to variable's definition" . anything-c-find-variable))
    (candidate-transformer anything-c-mark-interactive-functions))
  "Variable. (string or symbol)")

(defvar anything-c-source-emacs-variables
  '((name . "Emacs Variables")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (not (functionp a))
                                      (push (symbol-name a) commands))))
                      (sort commands 'string-lessp))))
    (volatile)
    (type . variable)
    (requires-pattern . 2))
  "Source for completing Emacs variables.")

;; -----------------------------------------------------------------------------
;;; anything-config-ext.el ends here
