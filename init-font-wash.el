;; “wash out” the colours of the font-lock Faces.
;; It should bring the colours closer to the colour of the default face.
;; So that they differ from the default foreground colour only slightly.
;; ‘font-lock-warning-face’, however, remains as bright and outstanding as before.

(defun egoge-wash-out-colour (colour &optional degree)
  "Return a colour string specifying a washed-out version of COLOUR."
  (let ((basec (color-values
                (face-attribute 'default :foreground)))
        (col (color-values colour))
        (list nil))
    (unless degree (setq degree 2))
    (while col
      (push (/ (/ (+ (pop col)
                     (* degree (pop basec)))
                  (1+ degree))
               256)
            list))
    (apply 'format "#%02x%02x%02x" (nreverse list))))

(defun egoge-wash-out-face (face &optional degree)
  "Make the foreground colour of FACE appear a bit more pale."
  (let ((colour (face-attribute face :foreground)))
    (unless (eq colour 'unspecified)
      (set-face-attribute face nil
                          :foreground (egoge-wash-out-colour colour degree)))))

(defun egoge-find-faces (regexp)
  "Return a list of all faces whose names match REGEXP."
  (delq nil
        (mapcar (lambda (face)
                  (and (string-match regexp
                                     (symbol-name face))
                       face))
                (face-list))))

(defun egoge-wash-out-fontlock-faces (&optional degree)
  (mapc (lambda (elt)
          (egoge-wash-out-face elt degree))
        (delq 'font-lock-warning-face
              (egoge-find-faces "^font-lock"))))

(when (> (length (defined-colors)) 16)
  (egoge-wash-out-fontlock-faces 2))
