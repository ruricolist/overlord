(uiop:define-package :overlord/all
    (:nicknames :overlord)
  (:import-from :overlord/types
    :overlord-error :overlord-warning)
  (:export :overlord-error :overlord-warning)
  (:import-from :overlord/module
    :module-ref :module-ref* :module-exports :module-static-exports)
  (:import-from :overlord/simple-module
    :simple-module)
  (:import-from :overlord/hash-table-module
    :hash-table-module)
  (:import-from :overlord/file-package
    :ensure-file-package
    :reset-file-package)
  (:import-from :overlord/message
    :message
    :*message-handler*
    :*message-stream*)
  (:import-from :overlord/redo
    :redo :redo-ifchange :redo-ifcreate :redo-always :redo-stamp)
  (:import-from :overlord/util
    :write-file-if-changed
    :copy-file-if-changed)
  (:export
   :module-ref :module-ref* :module-exports :module-static-exports
   :simple-module :hash-table-module
   :ensure-file-package :reset-file-package
   :message :*message-handler*
   :write-file-if-changed :copy-file-if-changed
   :redo :redo-ifchange :redo-ifcreate :redo-always :redo-stamp)
  (:use-reexport
   :overlord/base
   :overlord/target
   :overlord/freeze
   :overlord/parsers
   :overlord/message))

(defpackage :overlord-user
  (:use :cl :alexandria :serapeum :overlord/target)
  (:shadowing-import-from :overlord/target :import :define-constant))
