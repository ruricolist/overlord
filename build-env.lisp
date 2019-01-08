(defpackage :overlord/build-env
  (:documentation "Environment for builds, including (but not limited
  to) caching already built targets.")
  (:use :cl :alexandria :serapeum
    :overlord/target-table
    :overlord/target-protocol)
  (:import-from :bordeaux-threads
    :make-lock :make-recursive-lock
    :with-lock-held :with-recursive-lock-held)
  (:import-from :overlord/kernel :nproc)
  (:import-from :overlord/specials
    :use-threads-p
    :register-worker-special)
  (:import-from :overlord/message :message)
  (:import-from :overlord/db
    :require-db
    :saving-database)
  (:import-from :lparallel :make-kernel :*kernel*)
  (:export
   :with-build-env
   :*use-build-cache*
   :build-env-bound?
   :cached-stamp
   :target-exists?/cache
   :target-stamp/cache))
(in-package :overlord/build-env)

(defvar *use-build-cache* t
  "Should we cache which targets are already built?

Note that this can safely be rebound around part of a build when
non-caching behavior is desired.")
(register-worker-special '*use-build-cache*)

(defvar *build-id* 0)

(defun next-build-id ()
  (synchronized ()
    (incf *build-id*)))

(defvar-unbound *build-env*
  "Environment for the current build.")
(register-worker-special '*build-env*)

(defun use-build-cache? ()
  *use-build-cache*)

(defun build-env-bound? ()
  (boundp '*build-env*))

(defclass build-env ()
  ((lock :initform (make-recursive-lock)
         :reader monitor)
   (id :type (integer 1 *)
       :initform (next-build-id))
   (table
    :initform (make-target-table)
    :reader build-env.table))
  (:documentation "Metadata for the build run."))

(defclass threaded-build-env (build-env)
  ((kernel :initform nil
           :reader build-env.kernel)))

(defmethod initialize-instance :after ((env threaded-build-env) &key)
  (with-slots (id kernel) env
    (message "Initializing threads for build ~a." id)
    (setf kernel
          (make-kernel nproc
                       :name (fmt "Kernel for build ~a." id)))))

(defun make-build-env ()
  (if (use-threads-p)
      (make 'threaded-build-env)
      (make 'build-env)))

(defstruct (target-meta
            (:conc-name target-meta.)
            (:constructor make-target-meta (target)))
  (target (error "No target")
   :read-only t)
  (stamp nil)
  (lock (bt:make-lock)))

(defun call/build-env (fn)
  (if (build-env-bound?)
      (funcall fn)
      (let ((env (make-build-env)))
        (call-in-build-env env fn))))

(defgeneric call-in-build-env (env fn))

(defmethod call-in-build-env (env fn)
  (with-slots (id) env
    (let ((*build-env* env))
      (saving-database
        ;; The DB cannot be loaded from within worker threads.
        (funcall fn)))))

(defmethod call-in-build-env ((env threaded-build-env) fn)
  (declare (ignore fn))
  (require-db)
  (with-slots (kernel id) env
    (message "Initializing threads for build ~a." id)
    (let ((kernel-name (fmt "Kernel for build ~a." id)))
      (lparallel.kernel-util:with-temp-kernel (nproc :name kernel-name)
        (call-next-method)))))

(defmacro with-build-env ((&key) &body body)
  (with-thunk (body)
    `(call/build-env ,body)))

(defun target-meta (target)
  (let* ((table (build-env.table *build-env*)))
    (or (target-table-ref table target)
        (synchronized (table)
          (ensure (target-table-ref table target)
            (make-target-meta target))))))

(defmacro with-target-meta-locked ((target &key) &body body)
  (with-thunk (body)
    `(call-with-target-meta-locked ,target ,body)))

(defun call-with-target-meta-locked (target fn)
  (if (not (build-env-bound?)) (funcall fn)
      (let* ((meta (target-meta target))
             (lock (target-meta.lock meta)))
        (bt:with-lock-held (lock)
          (funcall fn)))))

(defmethod call-with-target-locked (target fn)
  "Make call-with-target-meta-locked the default for call-with-target-locked."
  (with-target-meta-locked (target)
    (funcall fn)))

(defun cached-stamp (target)
  (when (build-env-bound?)
    (if (use-build-cache?)
        (target-meta.stamp (target-meta target))
        ;; If we are not using the cache, then we still want to
        ;; invalidate it, so subsequent build steps will be consistent
        ;; with the current one.
        (setf (cached-stamp target) nil))))

(defun (setf cached-stamp) (value target)
  (prog1 value
    (when (build-env-bound?)
      (setf (target-meta.stamp (target-meta target))
            (if (use-build-cache?)
                value
                ;; If we are not using the cache, then the cache should
                ;; still be invalidated, so subsequent build steps are
                ;; consistent with the current one.
                nil)))))

(defun target-exists?/cache (target)
  "Skip hitting the filesystem to check if a target exists if we
already built it."
  (or (and (build-env-bound?)
           (use-build-cache?)
           (true (cached-stamp target)))
      (target-exists? target)))

(defun target-stamp/cache (target)
  "Skip hitting the filesystem to check a target's stamp if we already
built it."
  (or (and (build-env-bound?)
           (use-build-cache?)
           (cached-stamp target))
      (target-stamp target)))
