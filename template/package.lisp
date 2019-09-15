;;;; package.lisp(#| TMPL_IF copyright |#)
;;
;;;; (#| TMPL_VAR copyright |#)(#| /TMPL_IF |#)

(defpackage #:(#| TMPL_VAR name |#)
  (:use #:cl #:alexandria #:serapeum)
  (:shadow #:(#| TMPL_VAR target |#))
  (:export #:(#| TMPL_VAR target |#)))
