(defpackage :overlord/parallel
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:overlord/message #:message)
  (:import-from #:overlord/specials #:use-threads?)
  (:import-from #:lparallel
    #:*kernel*
    #:make-kernel
    #:end-kernel
    #:pmap)
  (:export
   #:with-our-kernel))
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
  (if (use-threads?)
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
        (message "Initializing Overlord thread pool")
        (setf *our-kernel*
              (make-kernel (* nprocs 2)
                           :name "Overlord worker"
                           :use-caller t))))))

(defun end-our-kernel ()
  "Terminate the Overlord kernel."
  (when (boundp '*our-kernel*)
    (synchronized ('*our-kernel*)
      (when-let (*kernel* (bound-value '*our-kernel*))
        (message "Terminating Overlord thread pool")
        (end-kernel :wait t)))))

(uiop:register-image-dump-hook 'end-our-kernel)

