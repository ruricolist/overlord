(defpackage :overlord/digest
  (:use :cl :alexandria :serapeum)
  ;; TODO Use something better?
  #+sbcl (:import-from :sb-md5 :md5sum-string)
  #-sbcl (:import-from :md5 :md5sum-string)
  (:export :digest-string))
(in-package :overlord/digest)

(-> digest-string (string) octet-vector)
(defun digest-string (string)
  (values
   (md5sum-string string :external-format :utf-8)))
