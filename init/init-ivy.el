;;; init-ivy.el --- Initialize enhanced minibuffer completion package
;; -----------------------------------------------------------------------------

(use-package avy
  :ensure t
  :init
  ;; Set the avy-keys to the Dvorak home-row
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

  ;; :bind to a local map doesn't work
  ;; See https://github.com/jwiegley/use-package/issues/332#start-of-content
  (define-key my-nav-map (kbd "l") 'avy-goto-line)
  (define-key my-nav-map (kbd "c") 'avy-goto-word-1)
  (add-to-list 'which-key-replacement-alist
               '((nil .  "avy-goto-line") . (nil . "goto line")))
  (add-to-list 'which-key-replacement-alist
               '((nil .  "avy-goto-word-1") . (nil . "goto word"))))

(use-package ivy
  :ensure t
  :ensure avy
  :diminish ivy-mode
  :init
  (setq ivy-height 10       ;; number of result lines to display
        ivy-wrap t          ;; Wrap at first and last entry
        ivy-count-format "" ;; Don't count canditates

        ;; Add recent files and bookmarks to switch-buffer
        ivy-use-virtual-buffers t

        ;; Display candidates ...
        ivy-display-function nil ;; in minibuffer
        ;; ivy-display-function 'ivy-display-function-overlay ;; in overlay

        ivy-do-completion-in-region nil

        magit-completing-read-function 'ivy-completing-read
        )
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :ensure swiper
  :ensure smex
  :init
  (setq counsel-find-file-at-point t)

  :config
  ;; Add support for completing and expanding environment variables
  ;; See https://github.com/abo-abo/swiper/issues/776#issuecomment-260682059
  (defun counsel-env-res (res path)
    (let ((apath (abbreviate-file-name path)))
      (list (car res)
            (if (file-accessible-directory-p path)
                (file-name-as-directory apath)
              apath))))

  (defun counsel-env ()
    (delq nil
          (mapcar
           (lambda (s)
             (let* ((res (split-string s "=" t))
                    (path (cadr res)))
               (when (stringp path)
                 (cond ((file-exists-p path)
                        (counsel-env-res res path))
                       ((file-exists-p (expand-file-name path ivy--directory))
                        (counsel-env-res
                         res (expand-file-name path ivy--directory)))
                       (t nil)))))
           process-environment)))

  (defun counsel-expand-env ()
    (interactive)
    (if (equal ivy-text "")
        (progn
          (let ((enable-recursive-minibuffers t)
                (history (symbol-value (ivy-state-history ivy-last)))
                (old-last ivy-last)
                (ivy-recursive-restore nil))
            (ivy-read "Env: " (counsel-env)
                      :action (lambda (x)
                                (ivy--reset-state (setq ivy-last old-last))
                                (setq ivy--directory "")
                                (delete-minibuffer-contents)
                                (insert (cadr x))))))
      (insert "$")))

  :bind (("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("M-y" . counsel-yank-pop)
         ;;("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ;;([f2 ?i] . counsel-info-lookup-symbol)
         ;;("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)

         :map read-expression-map
         ("C-r" . counsel-expression-history)
         :map counsel-find-file-map
         ("$" . counsel-expand-env)))

;; -----------------------------------------------------------------------------
;;; Jump to links using avy tags

(use-package ace-link
  :ensure t
  :init
  (define-key my-nav-map (kbd "j") 'ace-link))

;; -----------------------------------------------------------------------------
;;; init-ivy.el ends here
