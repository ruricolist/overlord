;;;; overlord.asd
(in-package :asdf)

(defsystem #:overlord
  :description "Experimental build/module system."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :version (:read-file-form "version.sexp")
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :in-order-to ((test-op (test-op "overlord-tests")))
  :depends-on (#:overlord/all))

(defsystem #:overlord-tests
  :depends-on (#:overlord
               #:overlord/simple-module
               #:core-lisp
               #:fiveam
               #:serapeum
               #:alexandria
               #:local-time
               #:overlord/demo/js
               #:overlord/lang/sweet-exp
               #:overlord/lang/s-exp
               #:core-lisp)
  :perform (test-op (o s)
                    (uiop:symbol-call :overlord-tests '#:run-overlord-tests))
  :components ((:file "tests")))
