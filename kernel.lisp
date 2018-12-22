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
   #:with-meta-kernel
   #:end-meta-kernel
   #:make-resource
   #:with-resource-held))
(in-package :overlord/kernel)

(defconst meta-thread-count 8)

(defun nproc-string ()
  (handler-case
      (cond
        ((uiop:os-macosx-p)             ;NB macosx is also unix.
         (uiop:run-program
          '("sysctl" "-n" "hw.physicalcpu")
          :output :string))
        ((uiop:os-unix-p)
         (uiop:run-program
          '("nproc" "--all")
          :output :string))
        ;; TODO do something reasonable for Windows
        ((uiop:os-windows-p)
         (uiop:getenv "NUMBER_OF_PROCESSORS"))
        (t "2"))
    (serious-condition ()
      "2")))

(defun nproc ()
  (assure (integer 1 *)
    (parse-integer (nproc-string) :junk-allowed t)))

(def nproc
  (nproc))

(defvar-unbound *meta-kernel*
  "Lparallel kernel for fetching target metadata.")

(defun call/meta-kernel (thunk)
  (if (use-threads-p)
      (let ((*kernel* (ensure-meta-kernel)))
        (funcall thunk))
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
        (message "Initializing metadata thread pool")
        (setf *meta-kernel*
              (make-kernel meta-thread-count
                           :name "Overlord metadata fetcher"))))))

(defun end-meta-kernel ()
  "Terminate the Overlord kernel."
  (when (boundp '*meta-kernel*)
    (synchronized ('*meta-kernel*)
      (when-let (*kernel* (bound-value '*meta-kernel*))
        (message "Terminating Overlord metadata thread pool")
        (end-kernel :wait t)))))

(register-image-dump-hook 'end-meta-kernel)
