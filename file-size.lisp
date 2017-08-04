(defpackage :overlord/file-size
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop :ensure-pathname)
  (:shadow :file-size)
  (:export :file-size))
(in-package :overlord/file-size)

(deftype file-size ()
  '(integer 0 *))

(defun file-size-from-stream (file)
  (with-input-from-file (in file :element-type '(unsigned-byte 8))
    (file-length in)))

(defun file-size (file)
  (setf file (ensure-pathname file :want-pathname t))
  (or #+sbcl (sb-posix:stat-size (sb-posix:stat file))
      #+ccl (ccl:file-data-size file)
      #+clisp (ext:file-stat-size (ext:file-stat file))
      (file-size-from-stream file)))
