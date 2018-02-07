;;; Timestamps and other stamps. This package handles getting and
;;; comparing timestamps with different (possibly
;;; implementation-dependent) precisions.
(defpackage :overlord/stamp
  (:use :cl :alexandria :serapeum
    :local-time)
  (:import-from :overlord/types
    :universal-time)
  (:import-from :overlord/redo
    :stamp=)
  (:import-from :overlord/util :compare)
  (:import-from :fset)
  (:shadowing-import-from :trivial-file-size
    :file-size-in-octets)
  (:export
   #:never
   #:far-future
   #:file-meta
   #:target-timestamp
   #:stamp
   #:deleted
   #:timestamp-newer?
   #:target-timestamp=
   #:stamp=))
(in-package :overlord/stamp)

;;; Timestamps can be exact timestamps (from local-time), universal
;;; times, the singleton `never' (which means the target
;;; unconditionally needs building) and the singleton `far-future'
;;; (which means the target unconditionally does not need building).

(defunit never)
(defunit far-future)

(let ((local-time-resolution-bad?
        #.(loop repeat 1000
                for timestamp = (now)
                always (zerop (timestamp-microsecond
                               timestamp)))))
  (when local-time-resolution-bad?
    (warn "Local time resolution is too low to be useful.")))

(deftype target-timestamp ()
  "Possible formats for the timestamp of a target."
  '(or timestamp
    universal-time
    never
    far-future))

(defconstructor file-meta
  "Metadata to track whether a file has changed."
  ;; TODO hash?
  (size (integer 0 *))
  (timestamp target-timestamp))

(defun file-meta= (x y)
  (and (typep x 'file-meta)
       (typep y 'file-meta)
       (compare #'= #'file-meta-size x y)
       (compare #'target-timestamp= #'file-meta-timestamp x y)))

(defmethod fset:compare ((x file-meta) (y file-meta))
  (if (file-meta= x y)
      :equal
      :unequal))

(defconst deleted :deleted)

(deftype stamp ()
  `(or target-timestamp
       (eql ,deleted)
       string
       file-meta))

;; NB Note that conversion from timestamp to universal rounds down
;; (loses nsecs), so when comparing one of each, whether you convert
;; the universal time to a timestamp, or the timestamp to a universal
;; time, actually matters. What we do is to round the more precise to
;; match the less precise. It might seem perverse to lose information,
;; but think about it in terms of subtyping relationships. If Y is a
;; subtype of X, and X has an equality predicate defined on it, then
;; comparing an instance of X and an instance of Y will only take into
;; account the information they have in common, and lose the extra
;; information in Y.

(defun timestamp-newer? (ts1 ts2)
  "Is TS1 greater than TS2?"
  (dispatch-case ((ts1 target-timestamp)
                  (ts2 target-timestamp))
    ((target-timestamp never) t)
    ((target-timestamp far-future) nil)
    ((never target-timestamp) nil)
    ((far-future target-timestamp) t)

    ((timestamp timestamp)
     (timestamp> ts1 ts2))
    ((timestamp universal-time)
     (> (timestamp-to-universal ts1) ts2))

    ((universal-time universal-time)
     (> ts1
        ts2))
    ((universal-time timestamp)
     (> ts1 (timestamp-to-universal ts2)))))

(defun target-timestamp= (ts1 ts2)
  "Is TS1 greater than TS2?"
  (dispatch-case ((ts1 target-timestamp)
                  (ts2 target-timestamp))
    ((timestamp timestamp)
     (timestamp= ts1 ts2))
    ((timestamp universal-time)
     (= (timestamp-to-universal ts1) ts2))

    ((universal-time universal-time)
     (= ts1 ts2))
    ((universal-time timestamp)
     (= ts1 (timestamp-to-universal ts2)))

    ;; This might seem weird, but it's necessary for impossible
    ;; targets to always show up as changed.
    ((never never) nil)
    ((far-future far-future) t)
    ((target-timestamp target-timestamp) nil)))

(defmethod stamp= (s1 s2)
  (dispatch-case ((s1 stamp)
                  (s2 stamp))
    ((target-timestamp target-timestamp)
     (target-timestamp= s1 s2))
    ((target-timestamp stamp) nil)

    ((string string)
     (string= s1 s2))
    ((string stamp) nil)

    ((file-meta file-meta)
     (file-meta= s1 s2))
    ((file-meta target-timestamp)
     (stamp= (file-meta-timestamp s1) s2))
    ((target-timestamp file-meta)
     (stamp= s1 (file-meta-timestamp s2)))
    ((file-meta stamp) nil)

    (((eql #.deleted) stamp) nil)))
