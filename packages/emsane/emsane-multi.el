(require 'emsane)
;; (C) FSF 2010, Joakim Verona, part of the emsane package

;;Here we deal with using several scanners to scan a single book, magazine etc.

(emsane-declare-instance-get multi-job           nil "list of multi scanner configurations.")
(emsane-declare-instance-get multi-section nil "list of scanner configurations to be used in multi  scanner setups.")


;;WTF???
(defclass emsane-multi-job  (emsane-tracker)
  ((tracking-symbol :initform 'emsane-multi-job-list)
   (multi-section-list :initarg :multi-section-list
                 :documentation "list of scanner buffer setups in this setup")
   (job :initarg :job
        :documentation "job, book, for instance"))
  "list of scanner configurations")

(defclass emsane-multi-section  (emsane-tracker ;;TODO is this needed?
                                   emsane-section-interface
                                   emsane-parent)
  ((tracking-symbol :initform 'emsane-multi-section-list)
   (start-section :initarg :start-section  :documentation "which section to start with within the job")
   )
  )


(defun emsane-multi-create-configs ()

  (dolist (job emsane-job-list)
    (let*
        ((jobname (oref  job :object-name))
         (bodyname (concat (oref  job :object-name) "-body")))
      (emsane-multi-job (concat jobname "-multi")
                        :job jobname
                        ;;TODO this should be configurable
                        :multi-section-list (list (emsane-multi-section "s1" :scanner "fujitsu1"
                                                                        :page 1
                                                                        :start-section bodyname)
                                                  (emsane-multi-section "s2" :scanner "fujitsu2"
                                                                        :start-section bodyname
                                                                        :page 1)
                                                  (emsane-multi-section "s3" :scanner "fujitsu3"
                                                                        :start-section bodyname
                                                                        :page   1)
                                                  (emsane-multi-section "s4" :scanner "fujitsu4"
                                                                        :start-section bodyname
                                                                        :page  1))))))


(defun emsane-multi-scan-start (multi-job job-id &optional section-overide)
  "Scan a single project with multiple scanners. Each scanner
will scan different sections of the material."
  (interactive (let*
                   ((mjob (emsane-multi-job-get (emsane-do-query (emsane-query-object "multijob" :prompt "multi-job" :object-type 'multi-job))))
                    (jobid (emsane-read-job-id (emsane-job-get (oref mjob :job))))
                    )
                 (list mjob jobid)))
  (let*
      ((msection-list (oref multi-job :multi-section-list))
       (job (emsane-job-get (oref multi-job :job)))
       (queue (emsane-postop-queue job-id
                                :default-directory (emsane-get-job-dir job job-id)
                                :process-buffer (get-buffer-create (format "*emsane postop %s*" job-id))))
       )
    (emsane-query-recall-reset job-state)   
    (mapcar (lambda (msection)
              (let
                  ((job-state (emsane-job-state job-id
                                                :job-id job-id
                                                :queue queue
                                                :job job))))
              (emsane-scan-start
               job-state
               (emsane-section-get (oref msection :start-section))
               msection   ;;section-overide should probably be made, per section 
               )
              ) msection-list)))

;; (defun emsane-section-chain (&rest sections)
;;   "gimme a bunch of sections and chain them together"
;;   (let* ((rv))
;;     (while sections
;;       (setq rv 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO functions for handling a list of multi-scans in sequence,
;;so that starting a new job is quick

;; (setq emsane-the-multi-batch
;;       '(("book-multi" "titel1" "a4" )
;;         ("book-multi" "titel2" "a3" )))
;; (setq emsane-the-multi-batch-index 0)


;; (defun emsane-multi-batch-next ()
;;   (interactive)
;;   (let* ((setting (nth emsane-the-multi-batch-index emsane-the-multi-batch)))
;;     (setq emsane-last-section-slots (emsane-section "dummy" :size (cdr setting))) ;;TODO refactor
;;     (emsane-multi-scan-start (car setting) (cdr setting))
;;     (setq emsane-the-multi-batch-index (+ 1 emsane-the-multi-batch-index ))))


(provide 'emsane-multi)
