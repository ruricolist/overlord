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
  (:export
   :module-ref :module-ref* :module-exports :module-static-exports
   :simple-module :hash-table-module
   :ensure-file-package :reset-file-package
   :message :*message-handler*)
  (:use-reexport
   :overlord/base
   :overlord/impl
   :overlord/parsers
   :overlord/message))

(defpackage :overlord-user
  (:use :cl :alexandria :serapeum :overlord/impl)
  (:shadowing-import-from :overlord/impl :import :define-constant))
