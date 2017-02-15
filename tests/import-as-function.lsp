#lang cl

(lambda (n)
  (labels ((fact (n)
             (if (<= n 1)
                 1
                 (* n (fact (- n 1))))))
    (fact n)))
