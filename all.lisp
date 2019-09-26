(uiop:define-package :overlord/all
  (:nicknames :overlord)
  (:import-from :overlord/types
    :overlord-error :overlord-warning
    :overlord-error-target)
  (:export :overlord-error :overlord-warning)
  (:import-from :overlord/message
    :message
    :*message-stream*)
  (:import-from :overlord/util
    :write-file-if-changed
    :copy-file-if-changed
    :strip-directory)
  (:import-from :overlord/specials
    :use-threads-p #:*force* #:*jobs*)
  (:import-from :overlord/redo
    :recursive-dependency
    :missing-script
    :building?
    :redo-always)
  (:import-from :overlord/build-env :*use-build-cache*)
  (:import-from :overlord/kernel :nproc)

  (:export
   :*use-build-cache*
   :recursive-dependency :missing-script
   :module-ref :module-ref* :module-exports :module-static-exports
   :simple-module :hash-table-module
   :ensure-file-package :reset-file-package
   :message :*message-stream*
   :write-file-if-changed :copy-file-if-changed
   :strip-directory
   :use-threads-p :*force*
   :building? :redo-always
   :overlord-error-target :overlord-error
   :nproc :*jobs*)
  (:use-reexport
   :overlord/base
   :overlord/target
   :overlord/freeze
   :overlord/message
   :overlord/target-protocol
   :overlord/oracle
   :overlord/cmd
   :overlord/project-system))

(defpackage :overlord-user
  (:use :cl :alexandria :serapeum :overlord))
