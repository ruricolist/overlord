(uiop:define-package :overlord/lang/s-exp
    (:use)
  (:mix :overlord/shadows :alexandria :serapeum)
  (:import-from :overlord :with-meta-language :module-progn-in)
  (:export :read-module :module-progn))

(in-package :overlord/lang/s-exp)

(defconst eof "eof")

(defun read-module (path stream)
  (with-meta-language (path stream)
    (declare (ignore path))
    (let ((*read-eval* nil))
      (loop for form = (cl:read stream nil eof)
            until (eq form eof)
            collect form))))

(defmacro module-progn (&rest forms)
  `(progn ,@forms))
