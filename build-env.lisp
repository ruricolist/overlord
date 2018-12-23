(uiop:define-package :overlord/build-env
    (:documentation "Environment for builds, including (but not limited
  to) caching already built targets.")
  (:use :cl :alexandria :serapeum
    :overlord/target-table
    :overlord/target-protocol)
  (:import-from :bordeaux-threads
    :make-lock :make-recursive-lock
    :with-lock-held :with-recursive-lock-held)
  (:import-from :overlord/kernel :nproc)
  (:import-from :overlord/specials :use-threads-p)
  (:import-from :overlord/message :message)
  (:import-from :overlord/db :saving-database)
  (:import-from :lparallel :make-kernel :*kernel*)
  (:export
   #:with-build-env
   #:*use-build-cache*
   #:cached-stamp
   #:target-exists?/cache
   #:target-stamp/cache
   #:build-env-closure))
(in-package :overlord/build-env)

(defvar *use-build-cache* t
  "Should we cache which targets are already built?

Note that this can safely be rebound around part of a build when
non-caching behavior is desired.")

(defvar *build-id* 0)

(defvar-unbound *build-env*
  "Environment for the current build.")

(defun build-env-closure (fn)
  (dynamic-closure
   '(*use-build-cache*
     *build-env*)
   fn))

(defsubst use-build-cache? ()
  *use-build-cache*)

(defsubst build-env-bound? ()
  (boundp '*build-env*))

(defclass build-env ()
  ((lock :initform (make-recursive-lock)
         :reader monitor)
   (id :type (integer 1 *)
       :initform (synchronized ()
                   (incf *build-id*)))
   (table
    :initform (make-target-table)
    :reader build-env.table)
   (kernel :initform nil
           :reader build-env.kernel))
  (:documentation "Metadata for the build run."))

(defmethod initialize-instance :after ((env build-env) &key)
  (with-slots (id kernel) env
    (when (use-threads-p)
      (message "Initializing threads for build ~a." id)
      (setf kernel
            (make-kernel nproc
                         :name (fmt "Kernel for build ~a." id))))))

(defsubst make-build-env ()
  (make 'build-env))

(defstruct (target-meta
            (:conc-name target-meta.)
            (:constructor make-target-meta (target)))
  (target (error "No target")
   :read-only t)
  (stamp nil)
  (lock (bt:make-lock)))

(defun call-in-build-env (fn)
  (if (build-env-bound?)
      (funcall fn)
      (let* ((env (make-build-env)))
        (with-slots (kernel id) env
          (let ((*build-env* env)
                (*kernel* kernel))
            (saving-database
              (unwind-protect
                   (funcall fn)
                (when kernel
                  (message "Terminating threads for build ~a." id)
                  (lparallel:end-kernel :wait t)))))))))

(defmacro with-build-env ((&key) &body body)
  (with-thunk (body)
    `(call-in-build-env ,body)))

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
