(defpackage :overlord/global-state
  (:use :cl :serapeum)
  (:export
   #:define-global-state
   #:reset-global-state))
(in-package :overlord/global-state)

(defvar *initial-pathname-defaults*
  *default-pathname-defaults*)

(defvar *initial-working-dir*
  (uiop:getcwd))

(defvar *global-state*
  (list
   (cons '*default-pathname-defaults* (lambda () *initial-pathname-defaults*))
   (cons '*readtable* (lambda () (copy-readtable nil)))
   (cons '*read-base* (constantly 10))
   (cons '*read-default-float-format* (constantly 'double-float))))

(defmacro define-global-state (name &body (init &optional docstring))
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
reloading: it completely resets Overlord's internal state.

Note that this does not reset *just* Overlord's state. It also resets
a number of Lisp global variables to their default values."
  (uiop:chdir *initial-working-dir*)
  (loop for (var . init) in *global-state*
        collect var
        do (setf (symbol-value var) (funcall init))))
