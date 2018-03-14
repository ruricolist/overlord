(defpackage :overlord/digest
  (:use :cl :alexandria :serapeum)
  ;; TODO Use something better?
  #+sbcl (:import-from :sb-md5 :md5sum-string :md5sum-file)
  #-sbcl (:import-from :md5 :md5sum-string :md5sum-file)
  (:export :digest-string :digest-file))
(in-package :overlord/digest)

(-> digest-string (string) octet-vector)
(defun digest-string (string)
  (values
   (md5sum-string string :external-format :utf-8)))

(-> digest-file (pathname) octet-vector)
(defun digest-file (pathname)
  (values (md5sum-file pathname)))
