;;; init-dictionary.el --- Initialize thesaurus
;; -----------------------------------------------------------------------------

(defvar my-word-map
  (let ((map (make-sparse-keymap)))
    (define-key my-map "w" map)
    (which-key-add-key-based-replacements "C-z w" "words")
    map))

;; -----------------------------------------------------------------------------
;;; Lookup word definition
(use-package define-word
  :ensure t
  :init
  (define-key my-word-map "d" 'define-word-at-point)
  (add-to-list 'which-key-replacement-alist
               '((nil .  "define-word-at-point") . (nil . "definition"))))

;; -----------------------------------------------------------------------------
;;; Thesaurus using the wn-org package
(use-package wn-org
  :init
  (define-key my-word-map "w" 'wn-org-search)
  (add-to-list 'which-key-replacement-alist
               '((nil .  "wn-org-search") . (nil . "wordnet"))))

;; -----------------------------------------------------------------------------
;;; Thesaurus using the synonyms package
(use-package synonyms
  :ensure t
  :init
  (setq synonyms-file (expand-file-name "Thesaurus/mthesaur.txt"
                                        user-emacs-directory)
        synonyms-cache-file (expand-file-name "/Thesaurus/cache.txt"
                                              user-emacs-directory))
  (define-key my-word-map "s" 'synonyms))

;; -----------------------------------------------------------------------------
;;; Acronyms using the rw-acronyms package
(use-package rw-acronyms
  :init
  (setq rw-acronyms-files-data
        (list (list (concat user-emacs-directory "Acronyms/vera.txt")
                    'iso-8859-1 "=" nil)))
  (define-key my-word-map "a" 'rw-acronyms-look-up)
  (add-to-list 'which-key-replacement-alist
               '((nil .  "rw-acronyms-look-up") . (nil . "acronym"))))

;; -----------------------------------------------------------------------------
;;; init-dictionary.el ends here
