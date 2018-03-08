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

(defgeneric root-target ()
  (:documentation "Return the root target."))

(defgeneric target-stamp (target)
  (:documentation "Return the stamp of TARGET.")
  (:method (target)
    (target-timestamp target)))

(defgeneric target-timestamp (target)
  (:documentation "Return the timestamp of TARGET."))

(defgeneric (setf target-timestamp) (timestamp target)
  (:documentation "Set the timestamp of TARGET.
Not every target type supports this."))

(-> target-exists? (t) boolean)
(defgeneric target-exists? (target)
  (:documentation "Does TARGET exists?")
  (:method :around (target)
    (true (call-next-method))))

(defgeneric target= (target1 target2)
  (:documentation "Are TARGET1 and TARGET2 the same?")
  (:method (t1 t2)
    (eql t1 t2))
  (:method :around (t1 t2)
    (or (eql t1 t2)
        (call-next-method))))

(defgeneric hash-target (target)
  (:documentation "Hash TARGET.

Two targets that are equal under `target=' should always have the same
hash \(though the reverse is not necessarily true)."))

(defgeneric resolve-target (target base)
  (:documentation "Resolve any relative pathnames in TARGET, using
  BASE for defaults.")
  (:method (target base)
    (declare (ignore base))
    target))

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

(defgeneric call-with-target-locked (target fn)
  (:documentation "Call FN holding the target-specific lock for TARGET."))

(defgeneric target-being-built-string (target)
  (:documentation "Return a string suitable for logging \(for humans) what target is being built.")
  (:method :around (target)
    (declare (ignore target))
    (assure string
      (call-next-method)))
  (:method (target)
    (princ-to-string target)))
