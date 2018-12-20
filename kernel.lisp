(defpackage :overlord/kernel
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:overlord/message #:message)
  (:import-from #:overlord/specials #:use-threads-p)
  (:import-from #:lparallel
    #:*kernel*
    #:make-kernel
    #:end-kernel
    #:pmap)
  (:import-from #:uiop
    #:register-image-dump-hook)
  (:export
   #:with-our-kernel
   #:end-our-kernel
   #:make-resource
   #:with-resource-held))
(in-package :overlord/kernel)

(defconst threads 8)

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
              (make-kernel threads
                           :name "Overlord worker"))))))

(defun end-our-kernel ()
  "Terminate the Overlord kernel."
  (when (boundp '*our-kernel*)
    (synchronized ('*our-kernel*)
      (when-let (*kernel* (bound-value '*our-kernel*))
        (message "Terminating Overlord thread pool")
        (end-kernel :wait t)))))

(register-image-dump-hook 'end-our-kernel)
