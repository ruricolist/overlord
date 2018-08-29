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
           #:*module*
           #:*program*
           #:*program-preamble*
           #:*language*
           #:*source*
           #:*db-version*
           #:db-version
           #:*suppress-phonies*
           #:*save-pending*
           #:use-threads-p))
(in-package #:overlord/specials)

(defvar-unbound *base* "The current base.")
(defvar-unbound *input* "Input of a pattern.")
(defvar-unbound *output* "Output of a pattern.")
(defvar-unbound *source* "Source file being compiled.")

(declaim (type (and directory-pathname absolute-pathname) *base*))

(declaim (type absolute-pathname *input* *output* *source*))

(defvar *cli* nil "Are we running on a CLI?")
(declaim (type boolean *cli*))

(defvar-unbound *module* "The module being returned.")
(defvar-unbound *program* "The program to be compiled.")

(defvar-unbound *program-preamble*
  "A preamble to the program to be compiled.

This would be something like a package declaration, that the reader
has to see before the other forms.")

(defvar-unbound *language* "The name (symbol) of the current language.")
(declaim (type symbol *language*))

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
  (setf *use-threads* (true value)))

(defvar *suppress-phonies* nil)
(declaim (type boolean *suppress-phonies*))

(defvar *save-pending* nil)
