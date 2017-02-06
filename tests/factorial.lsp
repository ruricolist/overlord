#lang overlord/lang/sweet-exp overlord/simple-module

(:export #'factorial)

defun factorial (n)
  if {n <= 1}
    1
    {n * factorial{n - 1}}
