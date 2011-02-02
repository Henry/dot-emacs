;; emsane.el -- Emacs SANE frontend

;;; Commentary:
;;

;; Emsane is an Emancs frontend for SANE.  Emsane runs scanadf with
;; various jobs, to make it easy to scan books, magazines etc.

;; SANE is an acronym for "scanner acces now easy", scanner support
;; available for free operating systems, see
;; http://www.sane-project.org/

;; Author: Joakim Verona, (C) FSF 2009, 2010, GPL

;; Some interesting Emsane features:
;; - convenient Emacs interface to scanning
;; - handles one or several scanners
;; - use different scanner settings for different sections of a book
;; - the power of tramp lets you run scanners remotely over ssh
;; - dired your scans
;; - postprocess scans:
;;   - format conversions, to djvu for instance
;;   - unpaper
;; - multi-scanner mode, scan different sections of a book with different scanners to speed up scanning

;; Quick usage:
;; - Define scanners and jobs in a file, "emsane-config.el"
;; for instance. See sample.
;; - load "emsane" and "emsane-config"
;; - start scanning with "emsane-scan-start
;; - check out the keybindings, but mostly you use "return" and "n" while scanning.

;; Some Emsane concepts:
;; scanner - a set of options describing a scanner
;; section - a list of scanner and postprocessing settings
;; job  - a list of sections

;; A section definition can contain:
;; - paper size; expressed as an alias(ISO sizes or common book sizes), or width x height
;; - front or duplex scan
;; - color/lineart
;; - resolution
;; - file name pattern
;; - postprocessing options

;; emsane.el is complemented by other Emacs packages such as dired,
;; and some of my own packages such as dired-sequence.el, and a patch
;; to the Emacs core to optionaly use imagemagick for image display,
;; so djvu files can be shown in Emacs.



;;; Code:

(require 'eieio)
(require 'eieio-base)
(require 'emsane-postop)
(require 'emsane-query)

(defcustom emsane-root-directory
  "~/my_scans"
  "Where to put jobs."
  :group 'emsane)


(defcustom emsane-notify-command
  "aplay /usr/share/sounds/linphone/rings/bigben.wav"
  "Command to execute when scanner needs attention."
  :group 'emsane)

(defcustom emsane-backend-supports-compression t
  "If your SANE installation is compiled with compression support, set to t."
  :group 'emsane)

(defvar emsane-paper-sizes
  "A list of paper sizes. Its also possible to add local size aliases to a section job,
 for instance for magazines which come in different sizes." )

(defvar emsane-iso-paper-sizes
  '(("a0"   (841 . 1189))
    ("a1"   (594 . 841))
    ("a2"   (420 . 594))
    ("a3"   (297 . 420))
    ("a4"   (210 . 297))
    ("a5"   (148 . 210))
    ("a6"   (105 . 148))
    ("a7"   (74 . 105))
    ("a8"   (52 . 74))
    ("a9"   (37 . 52))
    ("a10"  (26 . 37))
    ("b0"   (1000 . 1414))
    ("b1"   (707 . 1000))
    ("b2"   (500 . 707))
    ("b3"   (353 . 500))
    ("b4"   (250 . 353))
    ("b5"   (176 . 250))
    ("b6"   (125 . 176))
    ("b7"   (88 . 125))
    ("b8"   (62 . 88))
    ("b9"   (44 . 62))
    ("b10"  (31 . 44))
    ("c0"   (917 . 1297))
    ("c1"   (648 . 917))
    ("c2"   (458 . 648))
    ("c3"   (324 . 458))
    ("c4"   (228 . 324))
    ("c5"   (162 . 229))
    ("c6"   (114 . 162))
    ("c7"   (81 . 114.9))
    ("c8"   (57 . 81))
    ("c9"   (40 . 57))
    ("c10"  (28 . 40)))
  "ISO paper sizes from http://en.wikipedia.org/wiki/Paper_size.")

(defvar emsane-book-paper-sizes
  '(("Folio"         ( 382 . 305 ))
    ("Quarto"        ( 305 . 241.5 ))
    ("Octavo"        ( 228 . 152.5 ))
    ("Duodecimo"     ( 187 . 127))
    ("Twelvemo"      ( 187 . 127))
    ("Sextodecimo"   ( 171.5 . 101.5 ))
    ("Sixteenmo"     ( 171.5 . 101.5 ))
    ("Octodecimo"    ( 165 . 101.5))
    ("Eighteenmo"    ( 165 . 101.5))
    ("Trigesimo-secundo"( 140 . 90))
    ("Thirty-twomo"  ( 140 . 90))
    ("Sexagesimo-quarto"( 76 . 50))
    ("Sixty-fourmo"  ( 76 . 50)))
  "Book sizes http://en.wikipedia.org/wiki/Book_size")


(setq emsane-paper-sizes (append emsane-iso-paper-sizes emsane-book-paper-sizes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro to define named and tracked classes



(defmacro emsane-declare-instance-get (mname &optional fn-doc list-doc)
  "declares getter and instance lists"
  `(progn
     (defvar ,(intern (concat "emsane-" (symbol-name mname) "-list"))
       nil
       ,(concat "Tracker symbol for instances of " (symbol-name mname) "."))
     (defvar ,(intern (concat "emsane-" (symbol-name mname) "-history"))
       nil
       ,(concat "History symbol for instances of " (symbol-name mname) "."))
     (defun ,(intern (concat "emsane-" (symbol-name mname) "-get")) (name)
       ,(concat "Get " (symbol-name mname) " NAME from its tracker.")
       (let* ((instance-get-rv (emsane-instance-tracker-find name 'object-name
                                                             ',(intern (concat "emsane-" (symbol-name mname) "-list")))))
         (assert (not (null instance-get-rv)) nil  "object with key %s not found" name)
         instance-get-rv)
       )))



(emsane-declare-instance-get section);;inherits section interface
(emsane-declare-instance-get job)
(emsane-declare-instance-get query)
(emsane-declare-instance-get scanner)

(defun emsane-instance-tracker-find (key slot list-symbol)
  (eieio-instance-tracker-find key slot list-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eieio class definitions

;;TODO emsane-tracker isnt a stellar name, since we are named, tracked, and instance-inherited
(defclass emsane-tracker (eieio-named
                          eieio-instance-tracker
                          )
  (())
  "emsane-tracker works similar to eieio-instance-tracker, but it is also named, and the name works like a primary key.
there can only be one emsane-tracker object with a particular name.")

(defmethod initialize-instance :primary ((this emsane-tracker)
                                         &rest slots)
  "make sure only 1 object with a particular name on the tracker list."
  (let*
      ((sym (oref this tracking-symbol))
       (already-existing (object-assoc (oref this :object-name) :object-name (symbol-value sym))))
    (if already-existing (delete-instance already-existing))
    (call-next-method)
    ))

(defmethod clone ((obj emsane-tracker) &rest params)
  "enable instance tracking also for clones."
  ;;we want clone for eieio-instance-inheritor to execute
  ;;also clone doesnt do object init, so so it explicitly
  (assert (stringp (car params)) nil "1st arg to clone must be a new name!")
  (let ((theclone (call-next-method)))
    (initialize-instance theclone)
    theclone))


(defclass emsane-scanner (emsane-tracker)
  ((tracking-symbol :initform 'emsane-scanner-list)
   (scanwidth :initarg :scanwidth
              :documentation "physical width of scanner")
   (device :initarg :device
           :documentation "SANE device string of scanner")
   (options :initarg :options
            ;;:accessor emsane-get-options
            :documentation "options always used for this device")
   (image-type-options :initarg :image-type-options :initform nil
                       :documentation "an assoc list of options for a particular image type"
                       )
   (sources :initarg :sources
            :documentation "scanner sources (duplex,simplex, etc)")
   (modes :initarg :modes
          :documentation "scanner modes")
   ;;a couple of scanner quirk flags
   (topleft-adf :initarg :topleft-adf
                :initform t
                :documentation "t if scanner has centered adf, affects topleft calculation")
   (needs-pageheight :initarg :needs-pageheight
                     :initform nil
                     :documentation "t if scanner needs --page-height flag")
   (inhibit-adf :initarg :inhibit-adf
                :initform nil
                :documentation "t if scanner is a flatbed which scans forever withouth this flag")
   )
  "class describing a SANE scanner")


(defclass emsane-job (emsane-tracker   ;; store instantiated objects in a list
                      )
  ((tracking-symbol :initform 'emsane-job-list)
   (section-list :initarg :section-list
                 :accessor emsane-job-get-section-list
                 :documentation "get the list of section jobs")
   (job-id-template :initarg :job-id-template
                    :initform nil
                    :documentation "template when creating job id")
   ))



(defmethod emsane-read-job-id ((this emsane-job))
  "Prompt for a job id."
  (let
      ((template (oref this job-id-template))
       ;;(default-id (if (boundp 'emsane-current-job-id) emsane-current-job-id ""))
       )
    (if (null template)
        (read-string "job id:"
                     ;;TODO
                     ;;- complete id:s from contents of emsane-root-directory
                     ;; this should probably not go here, rather in a "emsane-scan-continue" function or something
                     ;;- [ and ] for example, are not allowed in job ids, guard against.
                     ;;- convert " " to "_" in jobids for convenience
                     ;;- GUID jobid sugestion when you dont have time to figure one out,
                     ;;also combine with some general xattr tagging interface?
                     nil
                     'emsane-job-id-history
                     ;;default-id
                     )
      (eval `(format (car template) ,@(emsane-read-values template))))))


(defclass emsane-section-interface ()
  ((size :initarg :size
         :documentation "paper size")
   (page :initarg :page
         :documentation "section start page")

   (scanner :initarg :scanner
            :documentation "scanner name")
   (source :initarg :source
           :accessor emsane-section-get-source
           :documentation "scanner source(duplex,simplex, etc)")
   (mode :initarg :mode
         :accessor emsane-section-get-mode
         :documentation "scanner mode(lineart,color etc)")
   (resolution :initarg :resolution
               :accessor emsane-section-get-resolution
               :documentation "scan resolution in dpi")
   (file-pattern :initarg :file-pattern
                 :accessor emsane-section-get-file-pattern
                 :documentation "how to name the files")
   (image-type :initarg :image-type
               :accessor emsane-section-get-image-type
               :documentation "image type(djvu,jpeg)")
   (operation-list :initarg :operation-list
                   :documentation "A list of operations to be used for every image scanned in this section")
   (subsection-idx :initarg :subsection-idx)
   (section-idx :initarg :section-idx)

   )
  :abstract t
  )



(defclass emsane-parent()
  ((parent :initarg :parent
           :initform emsane-the-section-defaults
           ;;:initform nil
           :documentation "parent object. this instances slots overrides the parent slots.")))

(defclass emsane-section (emsane-tracker   ;; store instantiated objects in a list(needs special clone override)
                          emsane-section-interface
                          emsane-parent
                          )
  ((tracking-symbol :initform 'emsane-section-list)
   )
  "class describing a section")

;;;value class
(defclass emsane-section-value (eieio-named emsane-section-interface emsane-parent)
  (
   ;;(object-name :initarg :object-name)
   )
  "val obj")

(defvar emsane-the-section-defaults
  (emsane-section-value "the-section-defaults"
                        :size (emsane-query-paper-size "paper-size" :prompt "Paper size" :values emsane-paper-sizes)
                        :page   (emsane-query-integer "page" :prompt "page")
                        :scanner (emsane-query-object "scanners" :prompt "Scanner" :object-type 'scanner )
                        :source  (emsane-query-atom "sources" :prompt "Source"  :values '(duplex simplex ))
                        :mode    (emsane-query-atom "modes" :prompt "Mode" :values '(lineart color ))
                        :resolution   (emsane-query-integer "resolution" :prompt "Resolution")
                        :file-pattern "%02d%02d-%%04d"
                        ;;(emsane-query-string  "file-pattern" :prompt "File pattern" :require-match nil)
                        :image-type   (emsane-query-atom "image-types"  :prompt "Image type"  :values '(djvu jpeg ))
                        :parent nil
                        :section-idx 0 :subsection-idx 0
                        )
  "default values for section slots")


(defclass emsane-job-state ()
  (
   ;;per job.
   (postop-queue :initarg :postop-queue)
   (job :initarg :job)
   (job-id :initarg :job-id)
   (missing-files :initarg :missing-files)
   (emsane-query-recall :initarg :emsane-query-recall :initform nil)
   ))

(defclass emsane-process-state ()
  (
   ;;per process buffer
   (section :initarg :section)
   (section-overide :initarg :section-overide) ;;TODO empty overide object
   (page :initarg :page)
   )
  )

(defmethod initialize-instance ((this emsane-process-state)  &rest slots)
  (oset this :section-overide (emsane-section-value "section-overide"))
  (call-next-method))

(defmethod initialize-instance ((this emsane-job-state)  &rest slots)
  (oset this :missing-files (emsane-postop-lifo "missing-files"))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;
(defmethod emsane-handle-slot ((this emsane-section-interface) job-state slot)
  (if (slot-boundp this slot)
      (let*
          ((sv (slot-value this slot))
           (query-result nil))

        (if (and (object-p sv) (object-of-class-p sv emsane-query))
            ;;if this exact query already was answered, return the previous value
            (if (assq sv (oref job-state :emsane-query-recall))
                (cdr (assq sv (oref job-state :emsane-query-recall)))
              ;;otherwise ask the question and store it
              (progn
                (setq query-result (emsane-do-query sv))
                (oset job-state :emsane-query-recall (append (list (cons sv query-result)) (oref job-state :emsane-query-recall)))
                query-result
                )
              )
          sv))
    (progn
      (emsane-handle-slot (cond
                           ((object-p (oref this :parent)) (oref this :parent))
                           ((symbolp (oref this :parent)) (eval (oref this :parent)))
                           (t (error "needed parent, but wasnt object or symbol"))
                           )
                          job-state
                          slot));;ask the parent for the value


    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;; getters for section-interface slots
(defmethod emsane-get-size ((this emsane-section-interface)  job-state &optional sizes)
  "accessor for a sections size slot, supports prompting and recall"
  ;;each slot supporting prompting and recall should look somewhat like this
  (unless sizes (setq sizes emsane-paper-sizes))
  (emsane-parse-paper-size (emsane-handle-slot this job-state 'size) sizes))

(defmethod emsane-get-page ((this emsane-process-state) job-state section)
  "accessor for a sections page slot, supports prompting and recall"
  (let*
      ((startnum (emsane-handle-slot section job-state 'page))
       )
    (cond
     ((eq 'continue startnum)  (if (slot-boundp this :page) (oref this :page )
                                 (emsane-do-query (emsane-query-integer "gimmeint" :prompt "Page number(1st continue)" :default 1))))
     (t startnum))))

(defmethod emsane-get-file-pattern ((this emsane-section-interface) job-state process-state)
  "return a file pattern looking like AABB-%04d.
AA is a section-idx from process-state, and BB subsection-idx"
  (let*
      ((pattern (emsane-handle-slot this job-state 'file-pattern)))
    (if (functionp pattern)
        (funcall pattern)
      (format pattern (emsane-handle-slot this job-state  :section-idx) (emsane-handle-slot this job-state :subsection-idx))
      )))

;;TODO these are query methods(because they ask the user stuff) so should go in the query file
(defmethod emsane-set-page ((this emsane-process-state) &optional page)
  (unless page (setq page (read-number "page:")))
  (oset this :page page)
  )

(defmethod emsane-set-subsection-idx ((this emsane-process-state) &optional subsection-idx)
  "used when the usual sections are too restrictive"
  (unless subsection-idx (setq subsection-idx (read-number "subsection-idx:")))
  (oset (oref this :section-overide) :subsection-idx subsection-idx)
  (emsane-set-page this 1)
  )

;; (defmethod emsane-set-section-idx ((this emsane-process-state) &optional section-idx)
;;   "normaly use the section setting instead"
;;   (unless section-idx (setq section-idx (read-number "section-idx:")))
;;   (oset this :section-idx section-idx)
;;   (emsane-set-page 1))


;;TODO refactor get-source and get-mode
(defmethod emsane-get-source ((this emsane-section-interface) job-state)
  (let*
      ((scanner (emsane-get-scanner this job-state)))
    (emsane-source-dealias scanner  (emsane-handle-slot this job-state 'source))))

(defmethod emsane-get-mode ((this emsane-section-interface) job-state)
  (let*
      ((scanner (emsane-get-scanner this job-state)))
    (emsane-mode-dealias scanner  (emsane-handle-slot this job-state 'mode))
    ))

(defmethod emsane-get-scanner ((this emsane-section-interface) job-state)
  ;;TODO getters that return objects can store object name referenses internaly
  ;;this should be generalized, otoh theres only get-scanner atm that actualy needs it
  (let* ((scanner (emsane-handle-slot this job-state 'scanner)))
    (cond ((object-p scanner) scanner)
          ((stringp scanner) (emsane-scanner-get scanner))
          (t (error "get-scanner on %s should return object or string but doesnt" this)))))


;; dealiases.
;;TODO probably needs support for more than 1 flag per mode/source/whatever
;;like image-type-options?

(defmethod emsane-source-dealias ((this emsane-scanner) source-alias)
  (let ((source-dealias-rv  (cadr (assoc source-alias (oref this sources)))))
    (assert (not (null source-dealias-rv)) nil "null not allowed for alias")
    source-dealias-rv))

(defmethod emsane-mode-dealias ((this emsane-scanner) mode-alias)
  (let ((mode-dealias-rv (cadr (assoc mode-alias (oref this modes)))))
    (assert (not (null mode-dealias-rv)) nil "null not allowed for alias")
    mode-dealias-rv))


(defmethod emsane-get-buffer-create ((this emsane-scanner))
  (let*
      ((buffer (get-buffer-create (format "*Emsane %s*" (oref this :object-name)))))
    (with-current-buffer buffer
      ;;dont reset locals if already in correct mode
      (unless (eq major-mode 'emsane-mode) (emsane-mode)))
    buffer))



(defmacro emsane-default-getter (slot)
  "declare a base version of a slot getter"
  `(defmethod ,(intern (concat "emsane-get-" (symbol-name slot))) ((this emsane-section-interface) job-state)
     (emsane-handle-slot this job-state  ',slot)))

(emsane-default-getter resolution)
(emsane-default-getter image-type)


(defmethod emsane-get-options ((this emsane-scanner) job-state section)
  (let* ((options (oref this :options))
         (image-type-options (car (emsane-get-image-type-options this
                                                                 (emsane-get-image-type section job-state);;(oref section :image-type)
                                                                 ))))
    (append options image-type-options)))


(defmethod  emsane-get-image-type-options ((this emsane-scanner) image-type)
  (cdr (assoc image-type (oref this :image-type-options))))

(defmethod  emsane-get-actual-image-type ((this emsane-scanner) image-type)
  "Give the actual image type a scanner produces, for a given IMAGE-TYPE."
  (let ((actual-img-type  (cadr (emsane-get-image-type-options this image-type))))
    ;;unless told otherwise scanadf produces .pnm files
    ;;scanadf can also produce .jpg if your scanner and Sane build allows it
    (unless actual-img-type (setq actual-img-type 'pnm))
    actual-img-type))


(defmethod emsane-get-section-names ((this emsane-job))
  (emsane-job-get-section-list this))

(defmethod emsane-next-section ((this emsane-job) current-section)
  "fetch the next logical section from the job"
  (let
      ((mylist (emsane-get-section-names this)))
    (while (not (equal (oref current-section :object-name) (car mylist) ))
      (setq mylist (cdr mylist)))
    (emsane-section-get (cadr mylist))))

(defmethod emsane-get-sections ((this emsane-job))
  "return a list of section job objects(rather than a list of strings)"
  (mapcar (lambda (x) (emsane-section-get x))
          (oref this section-list)))

(defmethod emsane-get-root-directory ((this emsane-job))
  ;;TODO figure out some convenient way to override this, to place similar scans in the same dir
  emsane-root-directory )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod emsane-get-job-dir  ((this emsane-job-state))
  "Return directory used to store scans.
Parent directories are created if needed."
  (let
      ((dir (concat (expand-file-name (emsane-get-root-directory (oref this :job)))
                    "/"
                    (oref this :job-id)
                    "/")))
    (mkdir dir t)
    dir))

;; ;;TODO refactor
;; (defmethod emsane-get-job-dir  ((this emsane-job) job-id)
;;   "Return directory used to store scans.
;; Parent directories are created if needed."
;;   (let
;;       ((dir (concat (expand-file-name (emsane-get-root-directory this))
;;                     "/"
;;                     job-id
;;                     "/")))
;;     (mkdir dir t)
;;     dir))


(defmethod emsane-set-section ((this emsane-process-state) job-state &optional section)
  "Set section. if SECTION is nil, prompt for one."

  (if (oref this :section-overide);;section-overide should be somewhat reset as well. a bit too ungeneral for my taste
      (progn
        (slot-makeunbound (oref this :section-overide) :page)
        (slot-makeunbound (oref this :section-overide) :subsection-idx)
        (slot-makeunbound (oref this :section-overide) :section-idx)
        ))

  ;;then figure out the section
  (unless section
    (setq section (emsane-section-get
                   (emsane-do-query (emsane-query-string "gimmesection" :prompt "Section" :values (oref (oref job-state :job) :section-list))))))
  ;;then set it
  (oset this :section section)
  ;;then take care of section overides
  (emsane-fixup-section-chain this)

  ;;now handle page

  (unless (equal (oref section :page) 'continue)
    (slot-makeunbound this :page))
  ;;ok, when :page in state is unbound, the section is queried
  ;;but, the section includes section-overide! and for fujitsu2 there is a query there, which is already answered
  ;;so page needs to be thoroughly reset in this mode

  ;;now handle idx:es
  ;;(oset this :section-idx (oref section :section-idx))
  ;;(oset this :subsection-idx (oref section :subsection-idx))

  )


(defmethod emsane-jump-to-dired ((this emsane-job-state) )
  (dired (emsane-get-job-dir this)))

(defun emsane-parse-paper-size (size-string sizes)
  "Return a size cons from SIZE-STRING.
SIZE-STRING is either an ISO paper size \"A4\" or a string like \"210 x 297\" (A4 in mm)."
  (if (consp size-string) size-string
    (let ((size (if (listp sizes) (cadr (assoc (downcase size-string) sizes)))))
      (if size
          size
        (if (string-match "\\([0-9]*\\)[ ]*x[ ]*\\([0-9]*\\)" size-string)
            (cons (string-to-number (match-string 1 size-string))
                  (string-to-number (match-string 2 size-string))))))))



(defvar emsane-job-history)
(defvar emsane-job-id-history)
(defvar emsane-scanner-history)

;;TODO have a look at using force-mode-line-update
(defun emsane-set-mode-line ()
  "Update the modeline with the current job, pagenumber, etc."
  (setq mode-line-buffer-identification
        (nconc (propertized-buffer-identification "%b")
               (list '(:eval (emsane-mode-line-string))))))

(defun emsane-mode-line-string ()
  (format " %s [%s %s] %s"
          (condition-case nil (oref emsane-current-job-state :job-id) (error "?"))
          (condition-case nil (oref (oref emsane-current-job-state :job) :object-name) (error "?"))
          (condition-case nil (oref (oref emsane-current-process-state :section) :object-name) (error "?"))
          (condition-case nil (oref emsane-current-process-state :page)(error "?"))
          ))

(defconst emsane-scan-file-suffix ".scn")

(defun emsane-scan-start (job-state &optional start-section section-overide)
  "start a new scan job."
  ;;piece together an emsane-process-state
  ;;start the scan
  (interactive
   (let*
       ((job (emsane-job-get (emsane-do-query (emsane-query-object "gimmejob" :prompt "job" :object-type 'job))))
        (job-id (emsane-read-job-id job))
        (start-section  (car (emsane-get-sections job)))
        (job-state (emsane-job-state job-id
                                     :job-id job-id
                                     :job job
                                     :postop-queue nil;;TODO
                                     ))
        (queue (emsane-postop-queue job-id
                                    :default-directory (emsane-get-job-dir job-state)
                                    :process-buffer (get-buffer-create (format "*emsane postop %s*" job-id)))))
     (oset job-state :postop-queue queue) ;; TODO
     (list job-state start-section)))
  (unless start-section (setq start-section (car (emsane-get-sections (oref job-state :job)))))
  (unless section-overide (setq section-overide (emsane-section-value "so")))
  (let*
      ((state (emsane-process-state (oref job-state :job-id)
                                    :section start-section
                                    :section-overide section-overide
                                    )))
    (emsane-fixup-section-chain state)
    (emsane-scan job-state state)))

(defun emsane-scan-again ()
  "scan again, only the job id will be re-read."
  (interactive)
  (oset emsane-current-job-state :job-id (emsane-read-job-id  (oref emsane-current-job-state :job)))
  (oset emsane-current-job-state :postop-queue
        (emsane-postop-queue (oref emsane-current-job-state :job-id)
                             :default-directory (emsane-get-job-dir emsane-current-job-state)
                             :process-buffer (get-buffer-create (format "*emsane postop %s*" (oref emsane-current-job-state :job-id)))))
  (emsane-scan-start emsane-current-job-state))


(defmethod emsane-fixup-section-chain ((this emsane-process-state))
  ;;:section-overide must be put in :section, and :section be made parent
  ;;TODO this is confusing, some clearer model should be found
  (let*
      ((section-overide (oref this :section-overide)))
    (if section-overide
        (progn
          (oset section-overide :object-name (oref (oref this :section) :object-name))
          (oset section-overide  :parent (oref this :section))
          (oset this :section section-overide))
      )))



(defun emsane-scan-continue (job-state process-state)
  (interactive (list emsane-current-job-state emsane-current-process-state))
  (emsane-scan job-state process-state))

(defmethod emsane-scan ((this emsane-job-state) process-state
                        &optional buffer the-sentinel the-filter)
  ;;TODO
  ;;- guard against file overwrites when scanning. ask user if overwriting is what she really wants.
  ;;  (not sure, overwriting has proven convenient)
  ;;- if a scan process is already running in buffer signal error and stop
  ;;the scanner buffer used is figured out from the scanner name by default

  ;;TODO :section-overide doesnt fcking work
  (let*
      ((section (oref process-state :section)))
    (unless buffer (setq buffer (emsane-get-buffer-create (emsane-get-scanner section this))))
    (unless the-sentinel (setq the-sentinel 'emsane-sentinel))
    (unless the-filter (setq the-filter 'emsane-filter))
    (with-current-buffer buffer
      (if (emsane-process-running) (error "scanner process already running in this buffer"))
      (setq emsane-current-process-state process-state) ;;TODO is this the right place really?
      (setq emsane-current-job-state job-state) ;;TODO is this the right place really?
      (let*
          ((postop-queue (oref this :postop-queue))

           (job-dir (oref postop-queue :default-directory)) ;;TODO cleanup these bindings a bit, they happened due to refactoring
           (dummy-dirok (assert (equal (substring job-dir -1) "/") nil "dir must end with /"));;It took a lot of time before I realized this is necessary
           (default-directory  job-dir)
           (scanner (emsane-get-scanner section this))
           (options (emsane-get-options scanner this section))
           (dealiased-source (emsane-get-source section this))
           (resolution (emsane-get-resolution section this))
           (dealiased-mode (emsane-get-mode section this))
           (file-pattern (emsane-get-file-pattern section this process-state))
           (startcount  (if (slot-boundp process-state :page) (oref process-state :page) (emsane-get-page process-state this section)))
           (dummy-startcount (oset process-state :page startcount)) ;;i suppose these "dummy" bindings arent stellar
           (imgtype (emsane-get-image-type section this))
           (size (emsane-get-size section this))
           (paperwidth (car size))
           (paperheight (cdr size))
           (topleft1 (- (/ (oref scanner :scanwidth) 2)
                        (/ paperwidth 2)))
           (topleft (if (oref scanner :topleft-adf)
                        topleft1
                      0))
           ;;TODO this is for croping from center, which maybe not all adf scanners need.
           ;;for flatbeds, it would be: paperwidth

           ;;(imgtype (emsane-section-get-image-type emsane-current-section))

           ;; only if hw supports it and only if jpg-color-hw option
           ;;normaly sane 1.0.20 must be recompiled to get get this

           ;; funnily the "fujitsu" requires --page-height, while the "brother" "test" and "epson" fails with it.
           (page-height (if  (oref scanner :needs-pageheight)
                            (list "--page-height" (number-to-string paperheight))))
           (scanscript (if (equal emsane-scan-file-ready-notifier 'emsane-scanadf-emacslient-notify)
                           (list "--script-wait" "--scan-script" emsane-scanadf-emacslient-path)))
           (args `("scanadf" ,buffer "scanadf"
                   "--device-name" ,(oref scanner :device)
                   ,@(if dealiased-source (list "--source" dealiased-source))
                   "--mode" ,dealiased-mode;;(emsane-mode-dealias scanner  mode)
                   ,@options;;(emsane-get-options scanner  emsane-current-section) ;;scanner specific options
                   ,@scanscript
                   "--resolution"  ,(number-to-string resolution)
                   "--output-file" ,(concat file-pattern emsane-scan-file-suffix)
                   "--start-count" ,(number-to-string startcount)
                   ,@(if (oref scanner :inhibit-adf) (list "--end-count" (number-to-string startcount) ))
                   "-l" ,(number-to-string topleft)
                   "-t" ,(number-to-string 0)
                   "-x" ,(number-to-string paperwidth)

                   ;;TODO figure out how to handle height better.
                   ;; y is height of scan-area
                   ;; page-height is height of scanner physical scan area, but this is less than max by default.
                   ;;y <= paperheight
                   "-y" ,(number-to-string paperheight)
                   ,@page-height))
           (dbg-process
            (format "scan command: %s\n" args
                    ;;(mapconcat (lambda (x) x) (cddr args) " ")

                    ))
           ;;TODO verify the type of each element of arg. no nil:s for instance
           ;;(all-not-nil  (mapconcat (lambda (x) (stringp x)) (cddr args) " "))
           (envdummy (setenv "EMSANE_STATE" (buffer-name)));;so scanadf will have a buffer id, which then scan-script will know!
           (scan-process
            (apply 'start-file-process
                   args
                   )))
        (insert dbg-process)
        (insert (format "job-dir:%s\n" job-dir))
        (set-process-sentinel scan-process the-sentinel)
        (set-process-filter scan-process the-filter)
        (process-put scan-process 'emsane-process-state process-state)
        (process-put scan-process 'emsane-job-state this)
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode setup
;;TODO do something about these tedious wrappers, like a macro on defmethod or somesuch

;; "-cps" means "-current-process-state" which is a buffer local
;; or also curent job state

(defun emsane-set-section-cps ()
  (interactive)
  (emsane-set-section emsane-current-process-state emsane-current-job-state)
  )

(defun emsane-set-page-cps ()
  (interactive)
  (emsane-set-page emsane-current-process-state)
  )

(defun emsane-set-subsection-idx-cps ()
  (interactive)
  (emsane-set-subsection-idx emsane-current-process-state)
  )

(defun emsane-set-section-idx-cps ()
  (interactive)
  (emsane-set-section-idx emsane-current-process-state)
  )


(defun emsane-jump-to-dired-cps ()
  "Dired the current scan project."
  (interactive)
  (emsane-jump-to-dired emsane-current-job-state))




(defvar emsane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m"        'emsane-scan-continue)
    (define-key map "s"           'emsane-scan-start)
    (define-key map "a"           'emsane-scan-again)
    (define-key map "m"           'emsane-multi-scan-start)
    (define-key map "n"           'emsane-set-section-cps)
    (define-key map "p"           'emsane-set-page-cps)
    (define-key map "i"           'emsane-set-subsection-idx-cps) ;; i for index
    (define-key map "d"           'emsane-jump-to-dired-cps)
    (define-key map "q"           'emsane-scan-quit)
    ;;TODO keys:
    ;;(define-key map "a"           'emsane-add-scanner-buffer) ;;add a new scanner, if single scanner job, become multi scan job
    ;;TODO these are the same keys also in postop and ctrl buffers
    ;;    (define-key map "o"           'emsane-jump-to-postop-buffer)
    ;;(define-key map "1"           'emsane-jump-to-scanner-buffer) ;;which should somehow use the key as argument, so 1 ... 9 jumps to scanners
    map)
  "Keymap for `emsane-mode'.")

(define-derived-mode emsane-mode fundamental-mode
  "emsane-mode"
  "scanner frontend mode"
  (set (make-local-variable 'emsane-current-process-state) nil)
  (set (make-local-variable 'emsane-current-job-state) nil)
  (emsane-set-mode-line)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process control support


(defun emsane-process-running ()
  "check if a process is running in the current buffer"
  ;;the code is a little hard to read. (process-status) returns a symbol or throws an error
  ;; - if the symbol is 'run, there is a running symbol, the other symbols I assume to mean
  ;; - if an
  (condition-case nil
      (if (equal 'run (process-status nil)) t nil)
    (error nil)))


(defun emsane-scan-quit ()
  "quit running scan process"
  ;;TODO should also deal with buffer process state
  (interactive)
  (if (emsane-process-running)
      (delete-process (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scanner process sentinel and filter


(defun emsane-sentinel (scan-process msg)
  "Called when scanadf is finished.
Argument SCAN-PROCESS is the previously created scanadf process.
Argument MSG is the exit code."
  (call-process-shell-command emsane-notify-command nil 0)
  (message (format "scan finished with code:%s in buffer %s" msg (process-buffer scan-process))))

;; TODO Also react on these type of strings:
;; Scanned document 00-0001 ;;start post-processing
;; scanadf: sane_start: Document feeder jammed
;; Document feeder jammed
;; scan finished?

(defvar emsane-scan-file-ready-notifier 'emsane-scanadf-line-handler)
;;(setq emsane-scan-file-ready-notifier 'emsane-scanadf-emacslient-notify)

(defvar emsane-scanadf-emacslient-path   "/home/joakim/.elisp/emsane/emsane-client.sh")

;;TODO define a class for notifiers, line-based or emacsclient-based
;; - the line based handler seems to suffer from race conditions. scanadf reports files too early sometimes
;; - emacsclient-notify seems robust, but a small separate wrapper is needed

(defun emsane-scanadf-emacsclient-notify (filename state-id)
  ;;(message "oh a file was ready! %s and i got a state id too! %s" filename state-id)
  ;;the state-id is currently the scanner buffer, because that happened to be convenient
  (with-current-buffer state-id
    (emsane-filename-handler filename emsane-current-process-state)))



(defun emsane-filter (proc string)
  "Filters scanadf output.
Argument PROC scanadf process.
Argument STRING output from scanadf."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc)))
          (state (process-get proc 'emsane-process-state))
          (job-state (process-get proc 'emsane-job-state)))
      (save-excursion
        (assert (not (null state)) nil "state cant be nil")
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        ;;break up string in lines and handle each
        ;;if we use the callback notifier this is handled elsewhere
        (if (equal emsane-scan-file-ready-notifier 'emsane-scanadf-line-handler)
            (mapc (lambda (line) (emsane-scanadf-line-handler line state job-state))
                  (split-string string "\n" t)));;TODO doesnt handle the case when lines are split "in the middle"
        (insert (format "filter:<<%s>>\n" ;;TODO should be possible to visit image files in the scanadf buffer!
                        (substring string 0 -1);;remove linefeed
                        ))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun emsane-scanadf-line-handler (string state job-state)
  "Process a single line STRING of scanadf output."
  (cond
   ((string-match "Scanned document \\(.*\\)" string) ;;a string emitted by scanadf
    (emsane-filename-handler (match-string 1 string) state job-state)
    )))

(defun emsane-filename-handler (filename state job-state)
  "Notify emsane a new FILENAME is available."
  (if (file-exists-p (concat (emsane-get-job-dir job-state) filename)) ;;check that the file really is there 1st
      (let*
          ((section (oref state :section)))
        ;;TODO add back old :operation-list support that didnt work
        (emsane-postop-push (oref job-state :postop-queue) (emsane-mkpostop-default filename section job-state))
        (string-match (concat "\\([0-9a-zA-Z]*\\)-\\([0-9]*\\)" emsane-scan-file-suffix) filename)
        (oset state  :page (+ 1 (string-to-number (match-string 2 filename))))

        ;;TODO trying to both add files to the queue, and nudging the queue to run simply doesnt work
        (emsane-postop-go (oref job-state :postop-queue))
        ;;(if (oref state :continue-go-loop) (emsane-postop-go (oref state :postop-queue)))
        )
    (emsane-postop-push (oref job-state :missing-files) filename)))

(defun emsane-dired-notifier ()
  ;;adding files from dired, mostly for debugging
  (interactive)
  (let* ((all-of-them (dired-get-marked-files t)
                      ))
    (with-current-buffer "*Emsane fujitsu2*";;TODO interactive
      (mapc (lambda (filename)
              (emsane-filename-handler
               filename
               emsane-current-process-state)) all-of-them)
      )
    ))


(setq emsane-dust-area-height 30)
;;150px should be 1/2 inch at 300 dpi = 1.27 cm
;;turns out only 30px is suitable for this measurment! 30px = 1/10 inch = 2.54/10 cm
(defun emsane-mkpostop-dustdetect (section)
  "detect dust adf scanner glass and quit"
  ;;- preop: add 150px to scan area somehow(just hardcode for now)
  ;;- postop:
  ;;- split image in 2 parts, "image.jpg" and "dust.jpg", at 150px from bottom
  ;;- "dust dust.jpg" is added to tx, if it fails, transaction fails, job fails
  ;;- normal postprocessing of "image" (emsane-mkpostop-convert)
  (list
   (emsane-postop-lisp-operation
    "size"
    :operation-lambda  (lambda (tx q)
                         (emsane-postop-setenv tx
                                               'CROP_AT_Y
                                               (- (cdr (emsane-image-size (emsane-postop-getenv tx 'SCANFILE)))
                                                  emsane-dust-area-height))))
   (emsane-postop-simple-shell-operation "crop-dust"  :operation-shell-command "convert +repage -crop +0+${CROP_AT_Y} ${SCANFILE} ${SCANFILE}.dust" )
   (emsane-postop-simple-shell-operation "crop-img"   :operation-shell-command (format "convert +repage -crop 0x0+0-%s ${SCANFILE} ${SCANFILE}"
                                                                                       emsane-dust-area-height) )
   (emsane-mkpostop-convert section job-state)
   (emsane-postop-simple-shell-operation "dust"  :operation-shell-command "dust ${SCANFILE}.dust"))
  )

(defvar emsane-postop-thumbnail nil "View thumbnails in postop
buffer. Is cpu intensive, and requires ImageMagick support in
Emacs")

(defun emsane-toggle-postop-thumbnail ()
  (interactive)
  (if  emsane-postop-thumbnail
      (setq emsane-postop-thumbnail nil)
    (setq emsane-postop-thumbnail t)))

(defun emsane-mkpostop-default (filename  section job-state)
  (let*
      ((tx (emsane-postop-transaction "tx")))
    (emsane-postop-setenv tx 'SCANFILE filename)
    (emsane-postop-setenv tx 'SCANFILEBASE (substring filename 0 ( - (length emsane-scan-file-suffix))))
    (emsane-postop-setenv tx 'SCANFILECONVERTED (format "%s.%s"
                                                        (emsane-postop-getenv tx 'SCANFILEBASE)
                                                        (emsane-get-image-type section job-state)))
    ;;push default op:s. these converts and deletes original.
    (emsane-postop-push tx (emsane-mkpostop-convert section job-state))
    (emsane-postop-push tx (emsane-postop-lisp-operation
                            "op"
                            :operation-lambda
                            (lambda (tx q) (delete-file (emsane-postop-getenv tx 'SCANFILE)))))
    (if emsane-postop-thumbnail (emsane-postop-push tx (emsane-postop-lisp-operation ;;TODO flag croping with defcustom
                                                        "crop"
                                                        :operation-lambda
                                                        `(lambda (tx q)
                                                           (with-current-buffer (oref q :process-buffer)
                                                             (emsane-insert-crop (concat (oref q :default-directory) (emsane-postop-getenv tx 'SCANFILECONVERTED))))))))
    ;;TODO .jpg is hardcoded
    tx))

;;this is highly experimental and relies on the imagemagick branch of emacs
;; even an undocumented interface in the branch :)
(defun emsane-insert-crop (filename)
  (let ((start (point)))
    (insert filename)
    (add-text-properties start (point)
                         `(display
                           (image :type imagemagick :file ,filename :crop (10000 40 0 0) )
                           help-echo ,filename
                           rear-nonsticky (display)))
    (insert "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; postprocessing support

(defvar emsane-image-type-suffixes
  '((jpg-gray jpg) (jpg-color jpg)(djvu-color djvu)))


(defun emsane-mk-conversion-command (filenamebase current-suffix type desired-type)
  "Produce a command line string to convert FILENAMEBASE from TYPE to DESIRED-TYPE.
FILENAMEBASE concatenated with CURRENT-SUFFIX gives the current concrete file name."
  (let*
      ((filename (concat filenamebase current-suffix))
       (replacesuffix (cadr (assoc desired-type emsane-image-type-suffixes)))
       (suffix (if replacesuffix replacesuffix desired-type)))
    (cond
     ((equal desired-type type)
      (format
       "cp %s %s.%s"  filename filenamebase suffix));;cp is for concistency, TODO find more efficient way
     ((eq desired-type 'djvu)
      (format "cjb2 -lossy %s %s.%s" filename  filenamebase suffix))
     ((eq desired-type 'djvu-color)
      (format "c44 %s %s.%s" filename  filenamebase suffix))
     ((eq desired-type 'jpg-gray)
      (format "convert -type Grayscale %s %s.%s"  filename  filenamebase suffix))
     ((eq desired-type 'jpg-color)
      (format "convert %s %s.%s"  filename  filenamebase suffix))
     (t
      (format "convert %s %s.%s"   filename  filenamebase suffix)))))


(defun emsane-mkpostop-convert (section job-state)
  "mk conversion postop"
  (let*
      ((imgtype (emsane-get-image-type section job-state))
       (actual-imgtype (emsane-get-actual-image-type (emsane-get-scanner section job-state) imgtype))
       (convcmd (emsane-mk-conversion-command
                 "${SCANFILEBASE}"
                 emsane-scan-file-suffix
                 actual-imgtype
                 imgtype)))
    (emsane-postop-simple-shell-operation "op1"
                                          :operation-shell-command convcmd)))

(defun emsane-image-size (image-file)
  "Return the size of IMAGE-FILE as a cons."
  ;;TODO I snipped this from "dragbox.el" so it might be reusable
  (with-current-buffer (get-buffer-create "*imagemagic identify*")
    (erase-buffer)
    (call-process "identify" nil "*imagemagic identify*" nil "-verbose" image-file) ;; "-ping" sometimes segfaults for me
    (goto-char (point-min))
    (re-search-forward "Geometry: \\([0-9]+\\)x\\([0-9]+\\)")
    (cons (string-to-number (match-string 1))
          (string-to-number (match-string 2)))))



;;TODO recovering a sad postop q
;;(progn (oset (oref emsane-current-process-state :postop-queue) :state nil) (emsane-postop-go (oref emsane-current-process-state :postop-queue) ))
;;the postop q rarely goes sad now.


(provide 'emsane)

;;; emsane.el ends here
