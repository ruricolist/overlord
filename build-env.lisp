(defpackage :overlord/build-env
  (:documentation "Build environment, including (but not limited to)
  caching already built targets.")
  (:use :cl :alexandria :serapeum
    :overlord/target-table
    :overlord/target-protocol)
  (:import-from :bordeaux-threads
    :make-lock)
  (:export
   #:with-build-env
   #:*use-build-cache*
   #:cached-stamp
   #:target-exists?/cache
   #:target-stamp/cache))
(in-package :overlord/build-env)

(defvar *use-build-cache* t
  "Should we cache which targets are already built?

Note that this can safely be rebound around part of a build when
non-caching behavior is desired.")

(defvar-unbound *build-env*
  "Environment for the current build.")

(defsubst use-build-cache? ()
  *use-build-cache*)

(defsubst build-env-bound? ()
  (boundp '*build-env*))

(defclass build-env ()
  ((lock :initform (make-lock)
         :reader monitor)
   (target-info
    :initform (make-target-table)
    :reader build-env.table))
  (:documentation "Metadata for the build run."))

(defstruct (target-meta
            (:conc-name target-meta.)
            (:constructor make-target-meta (target)))
  (target (error "No target")
   :read-only t)
  (stamp nil))

(defun call/build-env (fn)
  (if (build-env-bound?)
      (funcall fn)
      (let ((*build-env* (make 'build-env)))
        (funcall fn))))

(defmacro with-build-env ((&key) &body body)
  (with-thunk (body)
    `(call/build-env ,body)))

(defun target-meta (target)
  (let* ((table (build-env.table *build-env*)))
    (or (target-table-ref table target)
        (synchronized (table)
          (ensure (target-table-ref table target)
            (make-target-meta target))))))

(defun cached-stamp (target)
  (if (use-build-cache?)
      (target-meta.stamp (target-meta target))
      ;; If we are not using the cache, then we still want to
      ;; invalidate it, so subsequent build steps will be consistent
      ;; with the current one.
      (setf (cached-stamp target) nil)))

(defun (setf cached-stamp) (value target)
  (prog1 value
    (setf (target-meta.stamp (target-meta target))
          (if (use-build-cache?)
              value
              ;; If we are not using the cache, then the cache should
              ;; still be invalidated, so subsequent build steps are
              ;; consistent with the current one.
              nil))))

(defun target-exists?/cache (target)
  "Skip hitting the filesystem to check if a target exists if we
already built it."
  (if (and (build-env-bound?)
           (use-build-cache?))
      (true (cached-stamp target))
      (target-exists? target)))

(defun target-stamp/cache (target)
  "Skip hitting the filesystem to check a target's stamp if we already
built it."
  (if (and (build-env-bound?)
           (use-build-cache?))
      (cached-stamp target)
      (target-stamp target)))
