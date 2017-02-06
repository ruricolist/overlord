(defpackage :overlord/hash-table-module
  (:use :cl :alexandria :serapeum :overlord/types)
  (:import-from :overlord/module :module-ref :module-exports)
  (:export :hash-table-module))
(in-package :overlord/hash-table-module)

(defun hash-table-module (hash-table)
  (lret ((module (copy-hash-table hash-table :test #'eq)))
    (unless (= (hash-table-count module)
               (hash-table-count hash-table))
      (error* "Hash table ~a contains duplicate keys."))))

(defmethod module-ref ((m hash-table) name)
  (gethash name m))

(defmethod module-exports ((m hash-table))
  (hash-table-keys m))
