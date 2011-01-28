;; Code to integrate the w3m article buffer in gnus with org-mode.
;; Kudos to Andy Stewart (ManateeLazyCat on #emacs IRC) for provding
;; the code following a request. Can now paste w3m regions showing html
;; emails and newsletters directly into org-mode buffers with the URLs
;; transformed into org links. There is a default M-w key binding included.
;;
;; manatee.el for now prior to any integration.
;;
;; Oct 2008, rgr.

(defun w3m-get-buffer-with-org-style ()
  "Get current buffer content with `org-mode' style.
This function will encode `link-title' and `link-location' with `org-make-link-string'.
And move buffer content to lastest of kill ring.
So you can yank in `org-mode' buffer to get `org-mode' style content."
  (interactive)
  (let (transform-start
        transform-end
        return-content
        link-location
        link-title)
    (if mark-active
        (progn
          (setq transform-start (region-beginning))
          (setq transform-end (region-end))
          (deactivate-mark))
      (progn
        (setq transform-start (point-min))
        (setq transform-end (point-max))))
    (message "Start transform link to `org-mode' style, please wait...")
    (save-excursion
      (goto-char transform-start)
      ;; (goto-char (point-min))
      (while (and (< (point) transform-end)
                  (not (w3m-no-next-link-p))) ;if have next link in current buffer
        (if (not (w3m-anchor (point)))        ;don't move when current point have a valid url
            ;; get content between two links.
            (setq return-content (concat return-content (buffer-substring (point) (w3m-get-next-link-start)))))
        ;; get link location at current point.
        (setq link-location (w3m-anchor (point)))
        ;; get link title at current point.
        (setq link-title (buffer-substring (point) (w3m-get-anchor-end)))
        ;; concat `org-mode' style url to `return-content'.
        (setq return-content (concat return-content (org-make-link-string link-location link-title))))
      ;; concat rest context of current buffer
      (setq return-content (concat return-content (buffer-substring (point) transform-end)))
      (kill-new return-content)
      (message "Transform link completed. You can get it from lastest kill ring."))))

(defun w3m-get-anchor-start ()
  "Move and return `point' for thst start of the current anchor."
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'w3m-anchor-sequence) ;get start position of anchor
                 (point)))                                                      ;or current point
  (point))

(defun w3m-get-anchor-end ()
  "Move and return `point' after the end of current anchor."
  (interactive)
  (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence) ;get end position of anchor
                 (point)))                                                  ;or current point
  (point))

(defun w3m-get-next-link-start ()
  "Move and return `point' for that start of the current link."
  (interactive)
  (catch 'reach
    (while (next-single-property-change (point) 'w3m-anchor-sequence) ;jump to next anchor
      (goto-char (next-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))        ;return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun w3m-get-prev-link-start ()
  "Move and return `point' for that end of the current link."
  (interactive)
  (catch 'reach
    (while (previous-single-property-change (point) 'w3m-anchor-sequence) ;jump to previous anchor
      (goto-char (previous-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))        ;return point when current is valid link
        (throw 'reach nil))))
  (point))


(defun w3m-no-next-link-p ()
  "Return t if no next link after cursor.
Otherwise, return nil."
  (save-excursion
    (equal (point) (w3m-get-next-link-start))))

(defun w3m-no-prev-link-p ()
  "Return t if no prevoius link after cursor.
Otherwise, return nil."
  (save-excursion
    (equal (point) (w3m-get-prev-link-start))))

(define-key w3m-minor-mode-map (kbd "M-w")     'w3m-get-buffer-with-org-style)

(provide 'manatee)
