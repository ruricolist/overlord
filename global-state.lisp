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
  "Restore Overlord's global state to its value when first loaded.

This is intended to be the practical equivalent of quitting Lisp and
reloading: it completely resets Overlord's internal state."
  (loop for (var . init) in *global-state*
        collect var
        do (setf (symbol-value var) (funcall init))))
