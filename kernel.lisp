(uiop:define-package :overlord/kernel
    (:use #:cl #:alexandria #:serapeum)
  (:import-from #:overlord/message #:message)
  (:import-from #:overlord/specials #:use-threads-p)
  (:import-from #:lparallel
    #:*kernel*
    #:make-kernel
    #:end-kernel
    #:pmap
    #:task-handler-bind
    #:invoke-transfer-error)
  (:import-from #:uiop
    #:register-image-dump-hook)
  (:export
   #:with-meta-kernel
   #:end-meta-kernel
   #:nproc))
(in-package :overlord/kernel)

(defconst thread-count-cap 20)

(defun nproc ()
  (assure (integer 1 *)
    (count-cpus :online nil)))

(def nproc
  (nproc))

(def meta-kernel-size
  (min thread-count-cap
       (* 2 nproc)))

(defvar-unbound *meta-kernel*
  "Lparallel kernel for fetching target metadata.")

(defun call/meta-kernel (thunk)
  (if (use-threads-p)
      (let ((*kernel* (ensure-meta-kernel)))
        (task-handler-bind ((error #'invoke-transfer-error))
          (funcall thunk)))
      (funcall thunk)))

(defmacro with-meta-kernel ((&key) &body body)
  (with-thunk (body)
    `(call/meta-kernel ,body)))

(defun ensure-meta-kernel ()
  (start-meta-kernel)
  *meta-kernel*)

(defun start-meta-kernel ()
  (unless (boundp '*meta-kernel*)
    (synchronized ('*meta-kernel*)
      (unless (boundp '*meta-kernel*)
        (message "Initializing metadata thread pool for session")
        (setf *meta-kernel*
              (make-kernel meta-kernel-size
                           :name "Overlord metadata fetcher"))))))

(defun end-meta-kernel ()
  "Terminate the Overlord kernel."
  (when (boundp '*meta-kernel*)
    (synchronized ('*meta-kernel*)
      (when-let (*kernel* (bound-value '*meta-kernel*))
        (makunbound '*meta-kernel*)
        (message "Terminating Overlord metadata thread pool")
        (end-kernel :wait t)))))

(register-image-dump-hook 'end-meta-kernel)
