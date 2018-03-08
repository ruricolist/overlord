(defpackage :overlord/target-protocol
  (:use :cl :alexandria :serapeum)
  (:import-from :overlord/stamp :target-timestamp)
  (:export
   #:root-target
   #:target-stamp
   #:target-timestamp
   #:target-exists?
   #:target=
   #:hash-target
   #:resolve-target
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
   #:call-with-target-locked
   #:target-being-built-string))
(in-package :overlord/target-protocol)

(defgeneric root-target ())
(defgeneric target-stamp (target))
(defgeneric target-timestamp (target))
(defgeneric (setf target-timestamp) (timestamp target))
(defgeneric target-exists? (target))
(defgeneric target= (target1 target2)
  (:method (t1 t2)
    (eql t1 t2)))
(defgeneric hash-target (target))
(defgeneric resolve-target (target base))
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
(defgeneric target-being-built-string (target))
