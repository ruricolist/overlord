#lang overlord/s-exp overlord/simple-module

(:export #'fact)

(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))
