;;; Timestamps and other stamps. This package handles getting and
;;; comparing timestamps with different (possibly
;;; implementation-dependent) precisions.
(defpackage :overlord/stamp
  (:use :cl :alexandria :serapeum
    :local-time)
  (:import-from :overlord/types
    :universal-time
    :file-pathname)
  (:import-from :overlord/util :compare)
  ;; (:import-from :overlord/version
  ;;   :version :version-spec :version= :version-compatible?)
  (:import-from :fset)
  (:shadowing-import-from :trivial-file-size
    :file-size-in-octets)
  (:export
   #:never
   #:far-future
   #:file-meta
   #:file-hash
   #:resolved-file
   #:target-timestamp
   #:stamp
   #:timestamp-newer?
   #:target-timestamp=
   #:stamp=
   #:stamp-satisfies-p
   #:round-down-to-nearest-second))
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
  (fset:equal? x y))

(defmethod fset:compare ((x file-meta) (y file-meta))
  ;; NB Fset doesn't know how to compare target timestamps.
  ;; (fset:compare-slots x y #'file-meta-size #'file-meta-timestamp)
  ;; Sort first based on size, then on timestamp.
  (let* ((size1 (file-meta-size x))
         (size2 (file-meta-size y))
         (size-order (fset:compare size1 size2)))
    (if (not (eql size-order :equal))
        size-order
        (let ((ts1 (file-meta-timestamp x))
              (ts2 (file-meta-timestamp y)))
          (if (target-timestamp= ts1 ts2)
              :equal
              :unequal)))))

(defconstructor file-hash
  "The hash of a file.
We store both the size and the hash of the file to further reduce the
already negligible possibility of a collision."
  (size (integer 0 *))
  (hash string))

(defun file-hash= (x y)
  (fset:equal? x y))

(defmethod fset:compare ((x file-hash) (y file-hash))
  (fset:compare-slots x y
                      #'file-hash-size
                      #'file-hash-hash))

(defconstructor resolved-file
  "A resolved file.

This enables a relative file as a target to register as changed if the
file it resolves to changes.

This is intended for cases (like the `system-resource' target class)
where `redo-ifcreate' isn't enough to detect when a resource has been
shadowed."
  (path file-pathname)
  (meta (or file-meta file-hash)))

(defun resolved-file= (x y)
  (fset:equal? x y))

(defmethod fset:compare ((x resolved-file) (y resolved-file))
  (fset:compare-slots x y
                      #'resolved-file-path
                      #'resolved-file-meta))

(deftype stamp ()
  `(or target-timestamp
       string
       file-meta
       file-hash
       ;; version-spec
       resolved-file))

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
    ((never never) nil)
    ((target-timestamp never) t)
    ((never target-timestamp) nil)
    ((target-timestamp far-future) nil)
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

(defun round-down-to-nearest-second (ts)
  (etypecase-of target-timestamp ts
    ((or never far-future universal-time) ts)
    (timestamp
     (adjust-timestamp ts
       (set :nsec 0)))))

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
    ;; targets to always show up as changed, as well as for files that
    ;; have been deleted.
    ((never never) nil)
    ((far-future far-future) t)
    ((target-timestamp target-timestamp) nil)))

(defun stamp= (s1 s2)
  (dispatch-case ((s1 stamp)
                  (s2 stamp))
    ((target-timestamp target-timestamp)
     (target-timestamp= s1 s2))
    ((target-timestamp stamp) nil)

    ((string string)
     (string= s1 s2))
    ((string stamp) nil)

    ((file-hash file-hash)
     (file-hash= s1 s2))
    ((file-hash stamp) nil)

    ((file-meta file-meta)
     (file-meta= s1 s2))
    ((file-meta target-timestamp)
     (stamp= (file-meta-timestamp s1) s2))
    ((target-timestamp file-meta)
     (stamp= s1 (file-meta-timestamp s2)))
    ((file-meta stamp) nil)

    ;; ((version-spec version-spec)
    ;;  (version= s1 s2))
    ;; ((version-spec stamp) nil)

    ((resolved-file resolved-file)
     (resolved-file= s1 s2))
    ((resolved-file stamp) nil)))

(defun stamp-satisfies-p (new old)
  "Is stamp NEW practically equivalent to (but not necessarily the
same as) OLD?"
  ;; Resist the temptation to compare timestamps chronologically here:
  ;; that would plunge us back into the hell of time zones, clock
  ;; skew, &c.
  (dispatch-case ((new stamp)
                  (old stamp))
    ;; NB You may want to restore this if we end up supporting semver
    ;; in the future.
    #+(or) ((version version)
            (version-compatible? new old))
    ((stamp stamp)
     (stamp= new old))))
