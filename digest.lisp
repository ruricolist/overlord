(defpackage :overlord/digest
  (:use :cl :alexandria :serapeum)
  ;; TODO Use something better?
  #+sbcl (:import-from :sb-md5 :md5sum-string)
  #-sbcl (:import-from :md5 :md5sum-string)
  (:export :digest-string
   :byte-array-to-hex-string))
(in-package :overlord/digest)

(-> digest-string (string) octet-vector)
(defun digest-string (string)
  (values
   (md5sum-string string :external-format :utf-8)))

;;; From Ironclad.
(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'base-char))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector (unsigned-byte 8)) vector)
           (type fixnum start)
           (type (or null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (ecase element-type
                          ;; so that the compiler optimization can jump in
                          (base-char (make-string (* length 2)
                                                  :element-type 'base-char))
                          (character (make-string (* length 2)
                                                  :element-type 'character)))
          for i from start below end
          for j from 0 below (* length 2) by 2
          do (let ((byte (aref vector i)))
               (declare (optimize (safety 0)))
               (setf (aref string j)
                     (aref hexdigits (ldb (byte 4 4) byte))
                     (aref string (1+ j))
                     (aref hexdigits (ldb (byte 4 0) byte))))
          finally (return string))))
