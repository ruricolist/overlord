#lang overlord/lang/sweet-exp overlord/simple-module

(:export #'fact)

defun fact (n)
  if {n <= 1}
    1
    {n * factorial{n - 1}}
