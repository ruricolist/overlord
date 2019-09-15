;;;; (#| TMPL_VAR name |#).asd(#| TMPL_IF copyright |#)
;;
;;;; (#| TMPL_VAR copyright |#)(#| /TMPL_IF |#)

(defsystem "(#| TMPL_VAR name |#)"
  :defsystem-depends-on ("overlord")
  :class "overlord:overlord-project-system"
  :target-name #:(#| TMPL_VAR target |#)
  :serial t(#| TMPL_IF depends-on |#)
  :depends-on (#| TMPL_VAR dependencies-string |#)(#| /TMPL_IF |#)
  :components ((:file "package")
               (:file "(#| TMPL_VAR name |#)")))
