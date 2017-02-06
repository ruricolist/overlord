(uiop:define-package :overlord/lang/sweet-exp
    (:use :overlord/types)
  (:mix :overlord/shadows :alexandria :serapeum)
  (:import-from :overlord :with-meta-language :module-progn-in)
  (:import-from :readable :enable-sweet-real)
  (:import-from :named-readtables :find-readtable)
  (:export :read-module :module-progn))

(in-package :overlord/lang/sweet-exp)

(defconst eof "eof")

(defun make-sweet-exp-readtable ()
  (let ((*readtable* (copy-readtable)))
    (enable-sweet-real)
    *readtable*))

(defun read-module (path stream)
  (with-meta-language (path stream)
    (declare (ignore path))
    (let ((*read-eval* nil)
          (*readtable* (make-sweet-exp-readtable)))
      (loop for form = (cl:read stream nil eof)
            until (eq form eof)
            collect form))))

(defmacro module-progn (&body forms)
  `(progn ,@forms))
