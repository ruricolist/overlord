(defpackage :overlord/message
  (:use :cl :alexandria :serapeum
    :overlord/global-state)
  (:import-from :overlord/types :overlord-condition)
  (:export
   :overlord-message
   :message
   :*message-stream*))
(in-package :overlord/message)

(define-global-state *message-stream*
    (make-synonym-stream '*error-output*)
  "The stream printed to by the default message handler.")

(defun message (control &rest args)
  (let ((stream *message-stream*))
    (format stream "~&[Overlord] ~?~%" control args)))

(define-compiler-macro message (&whole call format-control &rest format-arguments)
  (eif (not (stringp format-control)) call
       `(message (formatter ,format-control) ,@format-arguments)))
