(in-package #:cl-user)

(defpackage #:overlord/specials
  (:use #:cl :overlord/types)
  (:import-from :serapeum :defvar-unbound :assure :~> :true)
  (:import-from :alexandria :read-file-into-string)
  (:import-from :overlord/asdf
    :asdf-system-version)
  (:export #:*base*
           #:*cli*
           #:*input*
           #:*output*
           #:*db-version*
           #:db-version
           #:*suppress-phonies*
           #:use-threads-p
           #:*force*))
(in-package #:overlord/specials)

(defvar-unbound *base* "The current base.")
(defvar-unbound *input* "Input of a pattern.")
(defvar-unbound *output* "Output of a pattern.")
(defvar-unbound *source* "Source file being compiled.")

(declaim (type (and directory-pathname absolute-pathname) *base*))

(declaim (type absolute-pathname *input* *output*))

(defvar *cli* nil "Are we running on a CLI?")
(declaim (type boolean *cli*))

(defparameter *db-version*
  (parse-integer
   (asdf-system-version :overlord))
  "Versioning for fasls.
Incrementing this should be sufficient to invalidate old fasls.")
(declaim (type db-version *db-version*))

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

(defvar *force* nil
  "Whether to force rebuilding.")
