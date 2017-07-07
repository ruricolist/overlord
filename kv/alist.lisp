(defpackage :overlord/kv/alist
  (:use :cl :alexandria :serapeum :overlord/kv/api)
  (:import-from :uiop
    :with-temporary-file
    :rename-file-overwriting-target
    :pathname-directory-pathname)
  (:export :alist-db))
(in-package :overlord/kv/alist)

(defconst empty "empty")

(defclass alist-db ()
  ((lock :initform (bt:make-lock) :reader monitor)
   (data :initform nil :initarg :data :type list)))

(defmethods alist-db (db lock data)
  (:method kv-getf (db key prop)
    ;; Only Clozure lets you specialize on keywords.
    (check-type key keyword)
    (check-type prop keyword)
    (let ((record (assoc key data)))
      (if (null record)
          (values nil nil)
          (destructuring-bind (key . plist) record
            (declare (ignore key))
            (let ((value (getf plist prop empty)))
              (if (eq value empty)
                  (values nil nil)
                  (values value t)))))))

  (:method (setf kv-getf) (value db key prop)
    (check-type value (or string number))
    (prog1 value
      (synchronized (db)
        (let ((record (assoc key data)))
          (if (null record)
              (push `(,key . (,prop ,value)) data)
              (destructuring-bind (key . plist) record
                (let ((new-record
                        `(,key . (,prop ,value ,@(remove-from-plist plist prop)))))
                  (setf data (cons new-record (remove record data :count 1))))))))))

  (:method save-db (db output-file)
    (let (temp-file)
      (uiop:with-temporary-file (:pathname temp
                                 :stream output
                                 ;; Ensure we can rename.
                                 :directory (pathname-directory-pathname output-file)
                                 :direction :output
                                 :element-type 'character
                                 :keep t)
        (let ((*print-circle* nil)
              (*print-base* 10))
          (write data :stream output :readably t))
        (setf temp-file temp))

      (when (pathnamep temp-file)
        (rename-file-overwriting-target temp-file output-file))))

  (:method load-db! (db input-file)
    ;; NB Not synchronized. It is assumed the DB is new.
    (prog1 db
      (with-input-from-file (in input-file :element-type 'character)
        (let ((alist (read in)))
          (assert (every (of-type '(cons keyword list)) alist))
          (setf data alist))))))
