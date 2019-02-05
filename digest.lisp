(defpackage :overlord/digest
  (:use :cl :alexandria :serapeum)
  ;; TODO Use something better?
  #+sbcl (:import-from :sb-md5 :md5sum-string :md5sum-file)
  #-sbcl (:import-from :md5 :md5sum-string :md5sum-file)
  (:import-from #:overlord/util
    #:byte-array-to-hex-string)
  (:export :digest-string :digest-file
           :string-digest-string :file-digest-string))
(in-package :overlord/digest)

(-> digest-string (string) octet-vector)
(defun digest-string (string)
  (values
   (md5sum-string string :external-format :utf-8)))

(-> digest-file (pathname) octet-vector)
(defun digest-file (pathname)
  (values (md5sum-file pathname)))

(-> string-digest-string (string) string)
(defun string-digest-string (string)
  (let* ((bytes (digest-string string))
         (hex-string (byte-array-to-hex-string bytes)))
    (fmt "md5:~a" hex-string)))

(-> file-digest-string (pathname) string)
(defun file-digest-string (pathname)
  (let* ((bytes (digest-file pathname))
         (hex-string (byte-array-to-hex-string bytes)))
    (fmt "md5:~a" hex-string)))
