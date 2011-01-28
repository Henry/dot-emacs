;;; init-dictionary.el --- Initialize thesaurus
;; -----------------------------------------------------------------------------
;;; Thesaurus using the synonyms package

(setq synonyms-file (expand-file-name "~/Emacs/Thesaurus/mthesaur.txt"))
(setq synonyms-cache-file (expand-file-name "~/Emacs/Thesaurus/cache.txt"))
(require 'synonyms)
;;(global-set-key "\C-ct" 'synonyms)

;; -----------------------------------------------------------------------------
;;; Thesaurus using the wn-org package

(require 'wn-org)
(define-key my-map "w" 'wn-org-search)

;; -----------------------------------------------------------------------------
;;; Acronyms using the rw-acronyms package

(require 'rw-acronyms)

(setq rw-acronyms-files-data
      '(("~/Emacs/Acronyms/vera.txt"
         iso-8859-1 "=" nil)))

;; -----------------------------------------------------------------------------
;;; init-dictionary.el ends here
