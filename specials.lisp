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

(defvar *worker-specials* '())

(defun worker-specials ()
  *worker-specials*)

(defun (setf worker-specials) (value)
  (check-type value list)
  (assert (every #'symbolp value))
  (assert (setp value))
  (setf *worker-specials* value))

(defun register-worker-special (var)
  (check-type var symbol)
  (pushnew var (worker-specials)))

(defun unregister-worker-special (var)
  (check-type var symbol)
  (removef var (worker-specials)))

(defun register-worker-specials (vars)
  (mapc #'register-worker-special vars))

(defun unregister-worker-specials (vars)
  (mapc #'unregister-worker-special vars))

(defun wrap-worker-specials (fn)
  (dynamic-closure (worker-specials) fn))

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
   *error-output*))

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
