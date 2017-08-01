(defpackage :overlord/message
  (:use :cl :alexandria :serapeum
    :overlord/global-state)
  (:import-from :overlord/types :overlord-condition)
  (:export
   :overlord-message
   :message
   :*message-handler*
   :*message-stream*))
(in-package :overlord/message)

(define-global-state *message-stream*
  (make-synonym-stream '*trace-output*)
  "The stream printed to by the default message handler.")

(defun default-message-handler (msg)
  (let ((stream *message-stream*)
        (control (simple-condition-format-control msg))
        (args    (simple-condition-format-arguments msg)))
    (prog1 (format stream "~&[Overlord] ~?~%" control args)
      ;; Messages the user doesn't see in time aren't very useful.
      (force-output stream))))

(define-global-state *message-handler* 'default-message-handler
  "The current handler for message conditions.")
(declaim (type (or function symbol) *message-handler*))

(defcondition overlord-message (overlord-condition simple-condition)
  ())

(defun message (format-control &rest format-arguments)
  (signal 'overlord-message
          :format-control format-control
          :format-arguments format-arguments))

(define-compiler-macro message (&whole call format-control &rest format-arguments)
  (eif (not (stringp format-control)) call
       `(message (formatter ,format-control) ,@format-arguments)))
