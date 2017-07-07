(defpackage :overlord/kv/api
  (:use :cl :serapeum)
  (:export :kv-getf :kv-snapshot :kv-key :kv-value))
(in-package :overlord/kv/api)

(deftype kv-key ()
  'keyword)

(deftype kv-value ()
  '(or string number null))

(-> kv-getf (t kv-key kv-key) (values kv-value boolean))
(defgeneric kv-getf (db key prop))

(-> (setf kv-getf) (kv-value t kv-key kv-key) kv-value)
(defgeneric (setf kv-getf) (value db key prop))

(defgeneric kv-snapshot (db))
