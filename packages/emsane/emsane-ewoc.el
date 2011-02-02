;;an ewoc to control emsane
(require 'ewoc)

;;there will be an ewoc def for emsane-process-state
;;

(defun emsane-ewoc-pp-process-state (data)
  ;;data is supposed to be an emsane-process-state
  (insert (format "section:%s page:%s" (oref data :section) (oref data :page))))

(defun emsane-ewoc ()
  (interactive)
  (switch-to-buffer
   (generate-new-buffer "emsane-ewoc"))
  (kill-all-local-variables)
  (erase-buffer)
  (buffer-disable-undo)

  (let ((data (apply 'vector (mapcar (lambda (n) (ash n -8))
                                     (color-values color))))
        (ewoc (ewoc-create 'emsane-ewoc-pp-process-state 
                           "\nColor Components\n\n"
                           (substitute-command-keys
                            "\n\\{colorcomp-mode-map}"))))
    (set (make-local-variable 'colorcomp-data) data)
    (set (make-local-variable 'colorcomp-ewoc) ewoc)
    (ewoc-enter-last ewoc 0)
    (ewoc-enter-last ewoc 1)
    (ewoc-enter-last ewoc 2)
    (ewoc-enter-last ewoc nil)))


