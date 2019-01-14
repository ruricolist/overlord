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
  (:import-from :lparallel
    :task-handler-bind
    :invoke-transfer-error
    #:make-channel
    #:submit-task
    #:receive-result)
  (:import-from #:lparallel.queue
    #:make-queue
    #:try-pop-queue
    #:push-queue)
  (:import-from #:lparallel.kernel-util
    #:with-temp-kernel)
  (:import-from #:lparallel.queue
    #:queue-count)
  (:export
   :with-build-env
   :*use-build-cache*
   :build-env-bound?
   :cached-stamp
   :target-exists?/cache
   :target-stamp/cache
   :ask-for-token
   :run-or-spawn-job
   :run-or-spawn-jobs))
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

(defmethod print-object ((self build-env) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (id) self
      (format stream "#~a" id))))

(defclass threaded-build-env (build-env)
  ((jobs :initarg :jobs :type (integer 1 *))
   (tokens :reader build-env-tokens)
   (jobs-used :initform 1)
   (handler
    :type function
    :initarg :handler))
  (:default-initargs
   :jobs nproc
   :handler #'invoke-transfer-error))

(defmethod track-jobs-used ((env threaded-build-env))
  "This should be used after a token is obtained to track how many
threads are being used.

The idea is to be able to tell how many of the allocated threads are
actually being used, so we know how many to allocate for the next run."
  (with-slots (jobs-used jobs tokens) env
    (let* ((length (1- jobs))
           (count (queue-count tokens))
           (used (- length count)))
      (maxf jobs-used (1+ used)))))

(defmethod initialize-instance :after ((self threaded-build-env) &key)
  (with-slots (jobs tokens) self
    (setf tokens (make-token-pool (1- jobs)))))

(defun make-build-env (&key jobs handler)
  (if (use-threads-p)
      (make 'threaded-build-env :jobs jobs :handler handler)
      (make 'build-env)))

(defstruct (target-meta
            (:conc-name target-meta.)
            (:constructor make-target-meta (target)))
  (target (error "No target")
   :read-only t)
  (stamp nil)
  (lock (bt:make-lock)))

(defun call/build-env (fn &key jobs debug)
  (if (build-env-bound?)
      (funcall fn)
      (let* ((handler
               (if debug
                   #'invoke-debugger
                   #'invoke-transfer-error))
             (env (make-build-env :jobs jobs
                                  :handler handler)))
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
  (with-slots (jobs id tokens jobs-used handler) env
    (let ((thread-count (1- jobs))
          (kernel-name (fmt "Kernel for build ~a." id)))
      (message "Initializing ~a thread~:p for build ~a."
               thread-count
               id)
      (if (zerop thread-count) (call-next-method)
          (with-temp-kernel (thread-count :name kernel-name)
            (task-handler-bind ((error handler))
              (multiple-value-prog1 (call-next-method)
                (message "A maximum of ~a/~a simultaneous job~:p were used."
                         jobs-used jobs))))))))

(defmacro with-build-env ((&key (jobs 'nproc) debug) &body body)
  (with-thunk (body)
    `(call/build-env ,body :jobs ,jobs :debug ,debug)))

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

;;; How jobs are parallelized. This is intended to be spiritually (and
;;; eventually technically) compatible with Make's jobserver protocol.
;;; We are still using Lparallel, but only as a thread pool; we ignore
;;; its scheduler. Instead, we use a fixed pool of tokens; when we try
;;; to run a job, we try to grab a token; if we can grab a token, we
;;; execute the job in the background (returning a channel) and return
;;; the token when we are finished; if we can't, we do the job in the
;;; current thread and return nothing.

(deftype token ()
  '(integer 0 *))

(defun make-token-pool (n)
  (make-queue :fixed-capacity n
              :initial-contents (range n)))

(-> ask-for-token (t) (or token null))
(defun ask-for-token (env)
  (lret* ((queue (build-env-tokens env))
          (token (try-pop-queue queue)))
    (track-jobs-used env)))

(-> return-token (t token) (values))
(defun return-token (env token)
  (push-queue token (build-env-tokens env))
  (values))

(defun run-or-spawn-job (fn)
  "Run FN in the background, if possible, otherwise in the foreground.

This is intended to be spiritually (and eventually technically)
compatible with Make's jobserver protocol. There is a fixed pool of
tokens; when we try to run a job, we try to grab a token; if we can
grab a token, we execute the job in the background (returning a
channel) and return the token when we are finished; if we can't, we do
the job in the current thread and return nothing.

We still use Lparallel, but only as a thread pool.

Note that you must call receive-result on each channel."
  (let* ((env *build-env*)
         (token (ask-for-token env)))
    (if (no token)
        (progn (funcall fn)
               nil)
        (lret ((ch (make-channel)))
          (submit-task ch
                       (lambda ()
                         (unwind-protect
                              (funcall fn)
                           (return-token env token))))))))

(defun run-or-spawn-jobs (fns)
  "Like `run-or-spawn-jobs'.
Return a vector of open channels."
  (remove nil
          (map 'vector
               #'run-or-spawn-job
               fns)))
