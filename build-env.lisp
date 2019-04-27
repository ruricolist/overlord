(defpackage :overlord/build-env
  (:documentation "Environment for builds, including (but not limited
  to) caching already built targets.")
  (:use :cl :alexandria :serapeum
    :overlord/target-table
    :overlord/target-protocol)
  (:import-from :fset)
  (:import-from :bordeaux-threads
    :make-lock :make-recursive-lock
    :with-lock-held :with-recursive-lock-held)
  (:import-from :overlord/kernel :nproc)
  (:import-from :overlord/specials
    :use-threads-p
    :register-worker-special)
  (:import-from :overlord/message :message :*message-stream*)
  (:import-from :overlord/db
    :require-db
    :saving-database)
  (:import-from :lparallel
    #:end-kernel
    #:make-kernel
    #:no-kernel-error
    #:*kernel*
    #:broadcast-task
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
  (:import-from #:overlord/types
    #:error*)
  (:import-from #:uiop
    #:absolute-pathname-p
    #:wait-process
    #:terminate-process
    #:process-alive-p)
  (:export
   :with-build-env
   :*use-build-cache*
   :build-env-bound?
   :cached-stamp
   :target-exists?/cache
   :target-stamp/cache
   :ask-for-token*
   :return-token*
   :claim-file*
   :claim-files*
   :temp-prereqs
   :temp-prereqsne
   :target-locked-p
   :register-proc*))
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
    :reader build-env.table)
   (file-owners
    :initform (dict)
    :reader build-env.file-owners)
   (procs
    :initform nil
    :accessor build-env-procs
    :documentation "Processes being run asynchronously."))
  (:documentation "Metadata for the build run."))

(defmethod print-object ((self build-env) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (id) self
      (format stream "#~a" id))))

;;; Claiming files.

(defmethod claim-file ((self build-env) target (file pathname))
  (claim-files self target (list file)))

(defmethod claim-files ((self build-env) target (files sequence))
  (assert (every #'absolute-pathname-p files))
  (nest
   (with-slots (file-owners) self)
   #-ccl (synchronized (self))
   (do-each (file files)
     (let ((owner
             (ensure-gethash file file-owners target)))
       (unless (target= owner target)
         (error* "~
Target ~a wants to build ~a, but it has already been built by ~a."
                 target file owner))))))

(defun claim-file* (target file)
  (claim-file *build-env* target file))

(defun claim-files* (target files)
  (claim-files *build-env* target files))

;;; Tracking processes (so they can be shut down on abnormal exit).

(defun register-proc* (proc)
  (when (build-env-bound?)
    (register-proc *build-env* proc)))

(defmethod register-proc ((env build-env) proc)
  "Remember PROC in ENV. Return PROC."
  (synchronized (env)
    (push proc (build-env-procs env)))
  proc)

(defmethod kill-procs ((env build-env) &key urgent)
  "Kill all live processes tracked by ENV."
  (do-each (proc (build-env-procs env))
    (when (process-alive-p proc)
      (terminate-process proc :urgent urgent))))

(defmethod await-procs ((env build-env))
  "Wait for processes tracked by ENV to exit."
  (do-each (proc (build-env-procs env))
    (wait-process proc)))

(defmethod call-with-procs-tracked ((env build-env) (fn function))
  (let ((abnormal? t))
    (unwind-protect
         (multiple-value-prog1 (funcall fn)
           (setf abnormal? nil))
      (when (some #'process-alive-p (build-env-procs env))
        (message "Waiting for launched programs...")
        (force-output *message-stream*))
      (when abnormal?
        (kill-procs env))
      (await-procs env))))

(defmacro with-procs-tracked ((env) &body body)
  (with-thunk (body)
    `(call-with-procs-tracked ,env ,body)))

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
  (lock (bt:make-lock))
  (lockedp nil :type boolean)
  (temp-prereqs (fset:empty-map) :type fset:map)
  (temp-prereqsne (fset:empty-set) :type fset:set))

(defplace temp-prereqs (target)
  (target-meta.temp-prereqs (target-meta target)))

(defplace temp-prereqsne (target)
  (target-meta.temp-prereqsne (target-meta target)))

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
      (with-procs-tracked (env)
        ;; The DB cannot be loaded from within worker threads.
        (saving-database
          (funcall fn))))))

(defmethod make-env-kernel ((env threaded-build-env) thread-count)
  (with-slots (jobs id) env
    (message "Initializing ~a thread~:p for build ~a."
             thread-count
             id)
    (let ((kernel-name (fmt "Kernel for build ~a." id)))
      (make-kernel thread-count
                   :name kernel-name
                   :context (lambda (fn)
                              (nest
                               ;; Propagate the build env here.
                               (let ((*build-env* env)))
                               ;; Give each thread its own random state.
                               ;; (Clozure CL, at least, gives every
                               ;; thread the same initial random state.
                               ;; This can cause race conditions when
                               ;; generating temporary file names.)
                               (let ((*random-state* (make-random-state t))))
                               (funcall fn)))))))

(defmethod call-in-build-env ((env threaded-build-env) fn)
  (declare (ignore fn))
  (require-db)
  (with-slots (jobs id tokens jobs-used handler) env
    (let ((thread-count (max 1 (1- jobs))))
      (if (zerop thread-count) (call-next-method)
          (let ((*kernel* nil))
            (with-procs-tracked (env)
              (unwind-protect
                   ;; Initialize the kernel lazily.
                   (handler-bind ((no-kernel-error
                                    (lambda (e) (declare (ignore e))
                                      (synchronized (env)
                                        (unless *kernel*
                                          (invoke-restart
                                           'store-value
                                           (make-env-kernel env thread-count)))))))
                     (task-handler-bind ((error handler))
                       (multiple-value-prog1 (call-next-method)
                         (message "A maximum of ~a/~a jobs were used."
                                  jobs-used jobs))))
                (when *kernel*
                  (end-kernel :wait t)))))))))

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
          (setf (target-meta.lockedp meta) t)
          (unwind-protect
               (funcall fn)
            (setf (target-meta.lockedp meta) nil))))))

(defmethod call-with-target-locked (target fn)
  "Make call-with-target-meta-locked the default for call-with-target-locked."
  (with-target-meta-locked (target)
    (funcall fn)))

(defun target-locked-p (target)
  (target-meta.lockedp (target-meta target)))

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
;;; its scheduler. Instead, we use a fixed pool of tokens.

(deftype token ()
  '(integer 0 *))

(defun make-token-pool (n)
  (let ((n (max 1 n)))
    (make-queue :fixed-capacity n
                :initial-contents (range n))))

(-> ask-for-token (t) (or token null))
(defun ask-for-token (env)
  (lret* ((queue (build-env-tokens env))
          (token (try-pop-queue queue)))
    (track-jobs-used env)))

(defun ask-for-token* ()
  "Get a token from the current build environment."
  (ask-for-token *build-env*))

(-> return-token (t token) (values))
(defun return-token (env token)
  (push-queue token (build-env-tokens env))
  (values))

(defun return-token* (token)
  "Return TOKEN to the current build environment."
  (return-token *build-env* token))
