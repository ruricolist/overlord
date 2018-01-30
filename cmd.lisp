(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum)
  (:shadowing-import-from :serapeum :collecting :summing :in)
  (:import-from :overlord/base :base)
  (:import-from :overlord/types :list-of :plist)
  (:import-from :overlord/message :*message-stream*)
  (:export :cmd))
(cl:in-package :overlord/cmd)

(defun parse-cmd-args (args)
  (let ((tokens (queue))
        (plist (queue)))
    (dolist (arg args)
      (typecase arg
        (string
         (qconc tokens (tokens arg)))
        (pathname
         (let ((string (uiop:native-namestring arg)))
           (when (string^= "-" string)
             ;; Should we ignore the unsafe file names if `--' or
             ;; `---' is already present in the list of tokens?
             (cerror "Allow the unsafe file name"
                     "File name ~a begins with a dash"
                     string))
           (enq string tokens)))
        (plist
         (qappend plist arg))
        ((list-of string)
         (qappend tokens arg))
        (t (error "Can't use ~a as a cmd argument." arg))))
    (values (qlist tokens)
            (qlist plist))))

(defun cmd (&rest args)
  (multiple-value-bind (tokens plist) (parse-cmd-args args)
    (multiple-value-call #'uiop:run-program
      tokens
      (values-list plist)
      :output t
      :error-output *message-stream*)))
