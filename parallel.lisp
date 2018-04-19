(defpackage :overlord/parallel
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:overlord/message #:message)
  (:import-from #:overlord/specials #:use-threads-p)
  (:import-from #:lparallel
    #:*kernel*
    #:make-kernel
    #:end-kernel
    #:pmap)
  (:export
   #:with-our-kernel
   #:end-our-kernel
   #:make-resource
   #:with-resource-held))
(in-package :overlord/parallel)

(def nprocs
  (handler-case
      ;; TODO do something reasonable for Windows
      (parse-integer
       (uiop:run-program
        '("nproc" "--all")
        :output :string))
    (serious-condition ()
      2)))

(defvar-unbound *our-kernel*
  "Lparallel kernel for Overlord.")

(defun call/our-kernel (thunk)
  (if (use-threads-p)
      (let ((*kernel* (ensure-our-kernel)))
        (funcall thunk))
      (funcall thunk)))

(defmacro with-our-kernel ((&key) &body body)
  (with-thunk (body)
    `(call/our-kernel ,body)))

(defun ensure-our-kernel ()
  (start-our-kernel)
  *our-kernel*)

(defun start-our-kernel ()
  (unless (boundp '*our-kernel*)
    (synchronized ('*our-kernel*)
      (unless (boundp '*our-kernel*)
        (message "Initializing thread pool")
        (setf *our-kernel*
              (make-kernel (* nprocs 2)
                           :name "Overlord worker"))))))

(defun end-our-kernel ()
  "Terminate the Overlord kernel."
  (when (boundp '*our-kernel*)
    (synchronized ('*our-kernel*)
      (when-let (*kernel* (bound-value '*our-kernel*))
        (message "Terminating Overlord thread pool")
        (end-kernel :wait t)))))

(defconstructor resource-token
  (pool-name string)
  (id (integer 0 *)))

(defstruct-read-only (resource
                      (:constructor %make-resource)
                      (:conc-name resource.))
  (name :type string)
  (count :type (integer 1 *))
  queue)

(defmethod print-object ((self resource) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a (~a)"
            (resource.name self)
            (resource.count self))))

(defun make-resource (&key (name "Anonymous resource")
                           (count 1))
  (check-type count (integer 1 *))
  (check-type name string)
  (%make-resource
   :name name
   :count count
   :queue (lparallel.queue:make-queue
           :fixed-capacity count
           :initial-contents
           (loop for i below count
                 collect (resource-token name i)))))

(defun call/resource-held (fn resource)
  (if (not (use-threads-p)) (funcall fn)
      (let* ((queue (resource.queue resource))
             (token (lparallel.queue:pop-queue queue)))
        (multiple-value-prog1
            (funcall fn)
          (lparallel.queue:push-queue token queue)))))

(defmacro with-resource-held ((resource) &body body)
  (with-thunk (body)
    `(call/resource-held ,body ,resource)))
