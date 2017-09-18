(defpackage :overlord/time-tuple
  (:use :cl :alexandria :serapeum :local-time)
  (:export
   :time-tuple
   :time-tuple-universal-time
   :time-tuple-real-time
   :time-tuple=
   :time-tuple>
   :time-tuple<
   :time-tuple->timestamp)
  (:documentation "Timestamp as a tuple of the universal time and
  internal real time."))
(in-package :overlord/time-tuple)

;;; Let's be clear about our assumptions: the following is only useful
;;; is we can expect that a tuple of (universal time, internal real
;;; time) is likely to be unique. Or, to put it another way, if we can
;;; expect that the internal real time clock is reset less often than
;;; once per second.

(defstruct-read-only
    (time-tuple
     :constructor                       ;Default constructor
     (:constructor %time-tuple (universal-time real-time)))
  "A timestamp consisting of a universal time and a remainder of
internal time units."
  (universal-time :type (integer 0 *))
  (real-time :type (integer 0 #.internal-time-units-per-second)))

#+sbcl (declaim (sb-ext:freeze-type time-tuple))

(-> time-tuple () time-tuple)
(defun time-tuple ()
  (let ((univ (get-universal-time))
        (real (get-internal-real-time)))
    (let ((rem (rem real internal-time-units-per-second)))
      (%time-tuple univ rem))))

(defmethod make-load-form ((self time-tuple) &optional env)
  (declare (ignore env))
  `(%time-tuple ,(time-tuple-universal-time self)
                ,(time-tuple-real-time self)))

(defsubst time-tuple= (tup1 tup2)
  (declare (time-tuple tup1 tup2))
  (nor (time-tuple< tup1 tup2)
       (time-tuple> tup1 tup2)))

(defmacro time-tuple-compare (comparator tup1 tup2)
  (once-only (tup1 tup2)
    `(let ((univ1 (time-tuple-universal-time ,tup1))
           (univ2 (time-tuple-universal-time ,tup2))
           (real1 (time-tuple-real-time ,tup1))
           (real2 (time-tuple-real-time ,tup2)))
       (or (,comparator univ1 univ2)
           (and (= univ1 univ2)
                (,comparator real1 real2))))))

(defsubst time-tuple< (tup1 tup2)
  (declare (time-tuple tup1 tup2))
  (time-tuple-compare < tup1 tup2))

(defsubst time-tuple> (tup1 tup2)
  (declare (time-tuple tup1 tup2))
  (time-tuple-compare > tup1 tup2))

(-> real-time-nsecs
    ((integer 0 #.internal-time-units-per-second))
    (integer 0 #.(round 1d9)))
(defun real-time-nsecs (real-time-units)
  (let* ((unit-factor (/ internal-time-units-per-second))
         (seconds (* unit-factor real-time-units))
         (nsecs (/ seconds 1d-9)))
    (round nsecs)))

(assert (= 1d9 (real-time-nsecs internal-time-units-per-second)))

(-> time-tuple->timestamp (time-tuple) timestamp)
(defun time-tuple->timestamp (time-tuple)
  (let* ((univ (time-tuple-universal-time time-tuple))
         (timestamp (universal-to-timestamp univ))
         (real (time-tuple-real-time time-tuple))
         (nsecs (real-time-nsecs real)))
    (timestamp+ timestamp
                nsecs :nsec)))

(defsubst time-tuple->universal-time (time-tuple)
  (time-tuple-universal-time time-tuple))
