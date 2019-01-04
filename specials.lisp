(defpackage #:overlord/specials
  (:use #:cl :overlord/types :alexandria :serapeum)
  (:import-from :overlord/asdf
    :asdf-system-version)
  (:export #:*base*
           #:*cli*
           #:*db-version*
           #:db-version
           #:*suppress-phonies*
           #:use-threads-p
           #:*force*
           #:worker-specials
           #:register-worker-special
           #:unregister-worker-special
           #:register-worker-specials
           #:unregister-worker-specials
           #:wrap-worker-specials))
(in-package #:overlord/specials)

(defvar *worker-specials* '()
  "List of special variables that should be propagated into worker threads.")

(defun worker-specials ()
  *worker-specials*)

(defun (setf worker-specials) (value)
  (check-type value list)
  (assert (every #'symbolp value))
  (assert (setp value))
  (setf *worker-specials* value))

(defun register-worker-special (var)
  "Register VAR as a variable that should be propagated into worker threads."
  (check-type var symbol)
  (pushnew var (worker-specials)))

(defun unregister-worker-special (var)
  "Stop VAR from being propagated into worker threads."
  (check-type var symbol)
  (removef var (worker-specials)))

(defun register-worker-specials (vars)
  "Register each var in VARS, as with `register-worker-special'."
  (mapc #'register-worker-special vars))

(defun unregister-worker-specials (vars)
  "Unregister each var in VARS as with `unregister-worker-special'."
  (mapc #'unregister-worker-special vars))

(defun wrap-worker-specials (fn)
  "Return a function suitable for passing to a worker that, that
lexically closes over the current dynamic value of every special that has been registered for propagation to worker threads."
  (let* ((symbols (worker-specials))
         (symbols (filter #'boundp symbols))
         (values (mapcar #'symbol-value symbols)))
    (assert (length= symbols values))
    (lambda (&rest args)
      (declare (dynamic-extent args))
      (progv symbols values
        (apply fn args)))))

(register-worker-specials
 '(*package*
   *readtable*
   *read-base*
   *read-eval*
   *read-default-float-format*
   *default-pathname-defaults*

   *standard-output*
   ;; Propagating trace output makes debugging much easier.
   *trace-output*
   *error-output*

   ;; Guard against someone trying to alter the list of worker
   ;; specials from within a worker.
   *worker-specials*))

(defvar-unbound *base* "The current base.")
(register-worker-special '*base*)

(declaim (type (and directory-pathname absolute-pathname) *base*))

(defvar *cli* nil "Are we running on a CLI?")
(declaim (type boolean *cli*))

(defparameter *db-version*
  (parse-integer
   (asdf-system-version :overlord))
  "Versioning for fasls.
Incrementing this should be sufficient to invalidate old fasls.")
(declaim (type db-version *db-version*))
(register-worker-special '*db-version*)

(defun db-version ()
  (assure db-version *db-version*))

(defvar *use-threads* nil
  "Whether to allow parallelism.")
(declaim (type boolean *use-threads*))

(defun use-threads-p ()
  *use-threads*)

(defun (setf use-threads-p) (value)
  (when value
    (unless bt:*supports-threads-p*
      (error "This Lisp implementation does not support threads.")))
  (setf *use-threads* (true value)))

(defvar *suppress-phonies* nil)
(declaim (type boolean *suppress-phonies*))
(register-worker-special '*suppress-phonies*)

(defvar *force* nil
  "Whether to force rebuilding.")
(register-worker-special '*force*)
