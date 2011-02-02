(require 'emsane)
(require 'emsane-postop)
(require 'emsane-multi)

;;TODO refactor local and common settings

;;the scanners are connected on another machine
;;(setq emsane-root-directory  "/ssh:joakim@claire:my_scans")
(setq emsane-root-directory  "~/my_scans")

;; properties specific for a fujitsu fi-5120c in my case



(emsane-scanner "fujitsu1"
                :scanwidth 216
                :device "fujitsu:fi-5120Cdj:173978" ;;this ID is uniqe per fujitsu device, so no usb-alias is needed
                ;;--df-X options are for double feed detection particular for the fujitsu backend
                :options '( "--df-action=Stop"
                            "--df-skew=yes"
                            "--df-thickness=yes"
                            "--df-length=yes"
                            "--bgcolor=White")
                :sources '((duplex "ADF Duplex")
                           (simplex "ADF Front")
                           (back "ADF Back"))
                :modes '((lineart "lineart")
                         (color "color")
                         (gray "gray"))
                :image-type-options `((jpg        ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg)
                                      (jpg-gray   ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg-gray)
                                      (jpg-color  ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg-color)
                                      (jpg-bw     ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg-bw))
                :needs-pageheight t
                )

(emsane-scanner "fujitsu1-no-df"
                :scanwidth 216
                :device "fujitsu:fi-5120Cdj:173978" ;;this ID is uniqe per fujitsu device, so no usb-alias is needed
                ;;--df-X options are for double feed detection particular for the fujitsu backend
                :options '( "--df-action=Continue")
                :sources '((duplex "ADF Duplex")
                           (simplex "ADF Front")
                           (back "ADF Back"))
                :modes '((lineart "lineart")
                         (color "color")
                         (gray "gray"))
                :image-type-options `((jpg        ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg)
                                      (jpg-gray   ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg-gray)
                                      (jpg-color  ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg-color)
                                      (jpg-bw     ,(if emsane-backend-supports-compression '("--compression" "JPEG" )) jpg-bw))
                :needs-pageheight t
                )




;;I have 3 fujitsu scanners,
(clone (emsane-scanner-get "fujitsu1") "fujitsu2" :device "fujitsu:fi-5120Cdj:25719")
(clone (emsane-scanner-get "fujitsu1") "fujitsu3" :device "fujitsu:fi-5120Cdj:152211")
;;and sometimes a 4th
(clone (emsane-scanner-get "fujitsu1") "fujitsu4" :device "fujitsu:fi-6130dj:105607")

;;this is the same as "test1...3" in emsane-test, but its convenient
;;to have one here as well some times.
(emsane-scanner "testc"
                :device "test"
                :scanwidth 216
                ;;:mode ;;TODO Color or Gray and depth options for test
                :modes '((color "Color")
                         (gray "Gray")
                         (lineart "Gray")) ;;TODO should have a mode specific flag --depth 1
                
                :options '("--test-picture"
                           "Color pattern")
                :sources '((duplex  "Automatic Document Feeder")
                           (simplex  "Automatic Document Feeder"))
                :image-type-options nil
                )



;;I also happen to have a brother mfc3240. It sucks in many ways, buy a Fujitsu instead.
;;  this boring device has no interesting options
(emsane-scanner "brother"
                :scanwidth 216
                :device "brother2:bus4;dev1"
                ;;:device "brother2:bus3;dev1" ;;the usb id jumps around, so use an alias TODO
                :options '( )
                :sources '((simplex "Automatic Document Feeder")) ;;only simplex on this one
                :modes '((lineart "Black & White")
                         (color "24bit color")
                         (gray "True Gray"))
                :image-type-options '((jpg       () jpg)
                                      (jpg-gray  () jpg-gray)
                                      (jpg-color () jpg-color)
                                      (jpg-bw    ( ) jpg-bw)))

;;Indeed an Epson flatbed also decorates the junkyard that is my office
(emsane-scanner "epson"
                :scanwidth 216
                :device "epson2:libusb:001:004" ;;the usb id jumps around, so use an usb-alias TODO
                :topleft-adf nil
                :options '()
                :inhibit-adf t ;;this is necessary for (some) flatbeds when using scanadf
                :sources '((simplex nil)) ;;only simplex on this  one, and no sources flag then
                :modes '((lineart "Binary")
                         (color "Color")
                         (gray "Gray"))
                :image-type-options '((jpg       () jpg)
                                      (jpg-gray  () jpg-gray)
                                      (jpg-color () jpg-color)
                                      (jpg-bw    () jpg-bw)))



;;global default scanner, it can be modified per buffer later
;;(setq emsane-default-scanner (emsane-scanner-get "fujitsu1"))

;;Scan jobs

;; I agonized about the conventions quite a bit.
;; currently I use filename prefixes like this:

;; 0000 - cover matter
;; 0001 - prefaces, TOC etc, not in the body sequence
;; 0100 - body
;; 0201 - color inlays not in the body sequence

;; The conventions are designed to have the pages come in the same
;; view order as the original, and the book page numbers to match up
;; with the image file names, which makes it much easier to validate
;; the scan.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; book

(emsane-job "book"
            :section-list
            '("book-body"
              "book-front-matter"
              "book-cover-duplex" ;;choose one of duplex or simplex
              "book-cover-simplex"
              "book-color-inlay"))


(emsane-job "book-gray"
            :section-list
            '("book-body-gray"
              "book-front-matter"
              "book-cover-duplex" ;;choose one of duplex or simplex
              "book-cover-simplex"
              "book-color-inlay"))



(emsane-section "book-body-gray"
                :source 'duplex
                :mode 'gray
                :resolution 300
                :image-type 'pnm
                :page 1
                :section-idx 1
                :subsection-idx 0
                )


(emsane-section "book-body"
                :source 'duplex
                :mode 'lineart
                ;;:resolution (emsane-query-named-values "s-m-l" :prompt "s-m-l" :values '(("s" 300) ("l"  600) ("m"  450)))
                :resolution 300
                :image-type 'djvu
                :page 1
                :section-idx 1
                :subsection-idx 0
                )




(clone (emsane-section-get "book-body")
       "book-front-matter"
       :section-idx 1
       :subsection-idx 0
       )

(clone (emsane-section-get "book-body")
       "book-cover-duplex";; color for the cover, duplex
       :mode 'color
       :image-type 'djvu-color
       :section-idx 0
       :subsection-idx 0
       )

(clone (emsane-section-get "book-body")
       "book-cover-simplex";; color for the cover,  simplex
       :mode 'color
       :source 'simplex
       :image-type 'djvu-color
       :section-idx 0
       :subsection-idx 0
       )

(clone (emsane-section-get "book-body")
       "book-color-inlay";; color for inlay, duplex
       :mode 'color
       :image-type 'djvu-color
       :section-idx 2
       :subsection-idx 1
       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bw-magazine
;;    magazine, greyscale body, color cover
;; TODO make it usable for books with a "front matter" section
(emsane-job "bw-magazine"
            :section-list   '("bw-magazine-body" "bw-magazine-cover"))

(emsane-section
 "bw-magazine-body"
 :source 'duplex
 :mode 'gray
 :image-type 'jpg-gray
 :resolution 300
 :section-idx 01
 :subsection-idx 00)

(clone (emsane-section-get "bw-magazine-body")
       "bw-magazine-cover"
       :page 1
       :mode 'color
       :image-type 'jpg-color
       :section-idx 00
       :subsection-idx 00)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; color-magazine
;;    magazine, color body, color cover
(emsane-job "color-magazine"
            :section-list   '("color-magazine-body" "color-magazine-cover"))

(emsane-section
 "color-magazine-body"
 :source 'duplex
 :mode 'color
 :image-type 'jpg-color
 :resolution 300
 :section-idx 01
 :subsection-idx 00)

(clone (emsane-section-get "color-magazine-body")
       "color-magazine-cover"
       :page 1
       :mode 'color
       :section-idx 00
       :subsection-idx 00)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; photo
;;    
(emsane-job "photo"
            :section-list   '("photo-portrait" "photo-landscape"))
;;TODO
;; - subsection support
;; - more std sizes
;; - asume lansdcape, and rotate 90 deg
;; - make jpg scan work again

;;landscape is handled by physically rotating the photos for now
(emsane-section
 "photo-portrait"
 ;; :size '(("110x150" (110 . 150))) ;;TODO
 :size "100x150"
 :source 'simplex
 :mode 'color
 :image-type 'jpg  ;;TODO jpg doesnt work atm
 :resolution 600
 ;; :page 1
 :page'continue ;;TODO
 :section-idx 01
 :subsection-idx 00)

(clone (emsane-section-get "photo-portrait") "photo-landscape"
       :size "150x100"
       ;;:page 1
       :page 'continue ;;TODO
       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scanning old exams

(emsane-job "exam"
            :job-id-template '("exam-%s-%s" ("Course:"  string) ("Date:" string))
            :section-list '("exam-questions" "exam-frontmatter" "exam-solutions" "exam-mynotes" ))

(emsane-section "exam-questions"
                :size "a4"
                :source 'simplex
                :mode 'lineart
                :resolution 600
                :image-type 'djvu
                :section-idx 01
                :subsection-idx 00
                )

(clone (emsane-section-get "exam-questions")
       "exam-frontmatter"
       :section-idx 00
       :subsection-idx 00
       )
(clone (emsane-section-get "exam-questions")
       "exam-solutions"
       :section-idx 01
       :subsection-idx 01
       )
(clone (emsane-section-get "exam-questions")
       "exam-mynotes"
       :section-idx 01
       :subsection-idx 02
       )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; just a bunch of pages, the subsection number is given by a counter rather than a fixed id
;; useful for documents withouth a weldefined structure, like a compendium


;;bunch o stuff
;;id like a template that lets me scan a bunch of vaguely similar stuff
;;- same size
;;- duplex or front
;;- color jpg or grayscale jpg
;;- change filename template per section quickly
;;seems like all values should be "ask", and some quick way to change a particular setting

(emsane-job "pages-a4-bw"
            :section-list
            '("pages-a4-bw-simplex" "pages-a4-bw-duplex"        "pages-a4-bw-simple-jpg"))

(emsane-section "pages-a4-bw-simplex"
                :page 1
                :size "a4"
                :source 'simplex
                :mode 'lineart
                :resolution 300
                :image-type 'djvu
                ;;:file-pattern (emsane-subsection-filepattern-lambda 1);;TODO
                :section-idx 01
                :subsection-idx 00
                )

(clone (emsane-section-get "pages-a4-bw-simplex")
       "pages-a4-bw-duplex"
       :source 'duplex)
(clone (emsane-section-get "pages-a4-bw-simplex")
       "pages-a4-bw-simple-jpg"
       :image-type 'jpg
       :source 'duplex)


;; (emsane-multi "pages-a4-bw-book-multi"
;;               :job "pages-a4-bw"
;;               :scanner-list (list (emsane-multi-scan-conf "s1" :scanner "fujitsu1"
;;                                                           :start-section "pages-a4-bw-simplex"
;;                                                           :buffer-name "emsane-fujitsu1")
;;                                   (emsane-multi-scan-conf "s2" :scanner "fujitsu2"
;;                                                           :start-section "pages-a4-bw-simplex"
;;                                                           ;;:page 'ask ;;TODO
;;                                                           :buffer-name "emsane-fujitsu2")))


(defun emsane-new-magazine (name root size page &optional name-template)
  (unless name-template (setq name-template  (list (concat name "-%02d-%02d") '("Year:" number) '("Issue:" number))))
  (emsane-job name
              :section-list   (list (concat name "-body") (concat name "-cover"))
              :job-id-template name-template)
  (clone (emsane-section-get (concat root "-magazine-body"))
         (concat name "-body")
         :page page
         :size size)
  (clone (emsane-section-get (concat root "-magazine-cover"))
         (concat name "-cover")
         :size size))

(provide 'emsane-config)
