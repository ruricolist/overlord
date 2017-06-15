(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum)
  (:import-from :overlord/specials :*base*)
  (:import-from :overlord/types :list-of :plist)
  (:export :cmd))
(cl:in-package :overlord/cmd)

(defun parse-cmd-args (args)
  (loop for arg in args
        if (stringp arg)
          nconc (tokens arg) into tokens
        else if (pathnamep arg)
               collect (uiop:native-namestring arg) into tokens
        else if (typep arg '(list-of string))
               append arg into tokens
        else if (typep arg 'plist)
               append arg into plist
        finally (return (values tokens plist))))

(defun cmd (&rest args)
  (multiple-value-bind (tokens plist) (parse-cmd-args args)
    (multiple-value-call #'uiop:run-program
      tokens
      (values-list plist)
      :output t
      :error-output t
      :directory *base*)))
