;;; init-dictionary.el --- Initialize thesaurus
;; -----------------------------------------------------------------------------
;;; Thesaurus using the synonyms package

(use-package synonyms
  :ensure t
  :init
  (setq synonyms-file (expand-file-name "~/Emacs/Thesaurus/mthesaur.txt")
        synonyms-cache-file (expand-file-name "~/Emacs/Thesaurus/cache.txt"))
  ;;:bind (("C-c t" . synonyms))
  )

;; -----------------------------------------------------------------------------
;;; Thesaurus using the wn-org package

(use-package wn-org
  :bind
  (:my-map ("w" . wn-org-search)))

;; -----------------------------------------------------------------------------
;;; Acronyms using the rw-acronyms package

(use-package rw-acronyms
  :init
  (setq rw-acronyms-files-data
        '(("~/Emacs/Acronyms/vera.txt" iso-8859-1 "=" nil))))

;; -----------------------------------------------------------------------------
;;; init-dictionary.el ends here
