(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum :iterate)
  (:shadowing-import-from :serapeum :collecting :summing :in)
  (:import-from :overlord/base :base)
  (:import-from :overlord/types :list-of :plist)
  (:export :cmd))
(cl:in-package :overlord/cmd)

(defun parse-cmd-args (args)
  (iterate (for arg in args)
    (typecase arg
      (string
       (appending (tokens arg) into tokens))
      (pathname
       (collect (uiop:native-namestring arg) into tokens))
      (plist
       (appending arg into plist))
      ((list-of string)
       (appending arg into tokens))
      (t (error "Can't use ~a as a cmd argument." arg)))
    (finally (return (values tokens plist)))))

(defun cmd (&rest args)
  (multiple-value-bind (tokens plist) (parse-cmd-args args)
    (multiple-value-call #'uiop:run-program
      tokens
      (values-list plist)
      :output t
      :error-output t)))
