#lang overlord/simple-module

(:export #'make #'rows #'cols #'ref #'each (#'put! :as set!))

(defun make (n m)
  (make-array (list n m) :initial-element nil))

(defun rows (grid)
  (first (array-dimensions grid)))

(defun cols (grid)
  (second (array-dimensions grid)))

(defun ref (grid n m)
  (and (< -1 n (rows grid))
       (< -1 m (cols grid))
       (aref grid n m)))

(defun put! (grid n m v)
  (setf (aref grid n m) v))

(defun each (grid proc)
  (loop for j below (rows grid) do
    (loop for k below (cols grid) do
      (funcall proc j k (ref grid j k)))))
