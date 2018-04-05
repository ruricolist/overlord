(defpackage :overlord/safer-read
  (:use :cl)
  (:export :safer-read-from-string :safer-read)
  (:documentation "Safer variants of read and read-from-string."))

(in-package :overlord/safer-read)

;;; Adapted from LoL.

(defparameter *safer-read-from-string-blacklist*
  '(#\# #+(or) #\: #\|))

(let ((rt (copy-readtable nil)))
  (defun safer-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "safer-read-from-string failure"))

  (dolist (c *safer-read-from-string-blacklist*)
    (set-macro-character
     c #'safer-reader-error nil rt))

  (defun safer-read-from-string (s &optional fail)
    (if (stringp s)
        (with-input-from-string (s s)
          (safer-read s fail))
        fail))

  (defun safer-read (s &optional fail)
    (let ((*readtable* rt) *read-eval*)
      (handler-bind
          ((error (lambda (condition)
                    (declare (ignore condition))
                    (return-from
                     safer-read fail))))
        (read s)))))
