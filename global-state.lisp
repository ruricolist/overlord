(defpackage :overlord/global-state
  (:use :cl :serapeum)
  (:export
   #:define-global-state
   #:reset-global-state))
(in-package :overlord/global-state)

(defvar *global-state* ())

(defmacro define-global-state (name init &optional docstring)
  `(progn
     (pushnew (cons ',name
                    (lambda ()
                      ,init))
              *global-state*
              :key #'car)
     (defvar ,name ,init
       ,@(unsplice docstring))))

(defun reset-global-state ()
  (loop for (var . fn) in *global-state*
        collect var
        do (setf (symbol-value var) (funcall fn))))
