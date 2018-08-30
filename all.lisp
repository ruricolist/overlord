(uiop:define-package :overlord/all
    (:nicknames :overlord)
  (:import-from :overlord/types
    :overlord-error :overlord-warning)
  (:export :overlord-error :overlord-warning)
  (:import-from :overlord/message
    :message
    :*message-stream*)
  (:import-from :overlord/util
    :write-file-if-changed
    :copy-file-if-changed)
  (:import-from :overlord/specials
    :use-threads-p)
  (:import-from :overlord/redo
    :building?
    :redo-always)
  (:export
   :module-ref :module-ref* :module-exports :module-static-exports
   :simple-module :hash-table-module
   :ensure-file-package :reset-file-package
   :message :*message-stream*
   :write-file-if-changed :copy-file-if-changed
   :use-threads-p
   :building? :redo-always)
  (:use-reexport
   :overlord/base
   :overlord/target
   :overlord/freeze
   :overlord/message
   :overlord/target-protocol
   :overlord/oracle))

(defpackage :overlord-user
  (:use :cl :alexandria :serapeum :overlord/target)
  (:shadowing-import-from :overlord/target :import :define-constant))
