(defpackage :overlord/target-protocol
  (:use :cl :alexandria :serapeum)
  (:export
   #:root-target
   #:target-stamp
   #:target-exists?
   #:target=
   #:hash-target
   #:target-build-script
   #:target-default-build-script
   #:build-script-target
   #:run-script
   #:record-prereq
   #:record-prereqne
   #:target-in-db?
   #:target-saved-prereqs
   #:target-saved-prereqsne
   #:saved-prereq-target
   #:saved-prereq-stamp
   #:target-up-to-date?
   #:save-temp-prereqs
   #:clear-temp-prereqs
   #:save-temp-prereqsne
   #:clear-temp-prereqsne
   #:generate-impossible-target
   #:call-with-target-locked))
(in-package :overlord/target-protocol)

(defgeneric root-target ())
(defgeneric target-stamp (target))
(defgeneric target-exists? (target))
(defgeneric target= (target1 target2))
(defgeneric hash-target (target))
(defgeneric target-build-script (target))
(defgeneric target-default-build-script (target))
(defgeneric build-script-target (script))
(defgeneric run-script (task))
(defgeneric record-prereq (target))
(defgeneric save-temp-prereqs (target))
(defgeneric record-prereqne (target))
(defgeneric save-temp-prereqsne (target))
(defgeneric target-in-db? (target))
(defgeneric target-saved-prereqs (target))
(defgeneric target-saved-prereqsne (target))
(defgeneric saved-prereq-target (prereq))
(defgeneric saved-prereq-stamp (prereq))
(defgeneric target-up-to-date? (target))
(defgeneric (setf target-up-to-date?) (value target))
(defgeneric clear-temp-prereqs (target))
(defgeneric clear-temp-prereqsne (target))
(defgeneric generate-impossible-target ())
(defgeneric call-with-target-locked (target fn))
