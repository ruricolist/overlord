(uiop:define-package :overlord/simple-module
    (:use)
  (:mix :serapeum :alexandria :overlord/shadows :overlord/types)
  (:import-from :alexandria :mappend)
  (:import-from :serapeum :op :car-safe :keep)
  (:import-from :overlord/module :module-ref :module-ref* :module-exports)
  (:import-from :overlord/parsers :slurp-stream :slurp-file)
  (:import-from :overlord/impl :with-imports)
  (:export
   :read-module :module-progn
   :simple-module
   :static-exports))

(defpackage :overlord/simple-module-user
  (:use :overlord/simple-module :overlord/shadows))

(in-package :overlord/simple-module)

(defun read-module (source stream)
  (declare (ignore source))
  `(module-progn
     ,@(slurp-stream stream)))

(defmacro module-progn (&body body)
  (let* ((export-forms (keep :export body :key #'car-safe))
         (exports (mappend #'rest export-forms))

         (import-forms (keep :import body :key #'car-safe))
         (import-specs (mapcar #'rest import-forms))

         (body (remove-if (lambda (form)
                            (or (member form export-forms)
                                (member form import-forms)))
                          body))

         (module-form
           `(simple-module ,exports
              ,@body)))
    (reduce (curry #'list 'with-imports)
            import-specs
            :initial-value module-form
            :from-end t)))

(defun static-exports (source)
  (let* ((forms (slurp-file source))
         (export-forms (keep :export forms :key #'car))
         (exports (mappend #'rest export-forms)))
    (loop for export in exports
          if (listp export)
            collect (second export)
          else collect export)))



(defun export-keyword (spec)
  (assure keyword
    (etypecase-of export-spec spec
      (non-keyword (make-keyword spec))
      (function-spec (export-keyword (second spec)))
      ((or (tuple non-keyword export-alias)
           (tuple function-spec export-alias))
       (make-keyword (second spec)))
      ((or macro-spec (tuple macro-spec export-alias))
       (error "Simple modules cannot export macros.")))))

(defun export-binding (spec)
  (assure (or non-keyword function-spec)
    (etypecase-of export-spec spec
      (non-keyword spec)
      (function-spec spec)
      ((tuple non-keyword export-alias) (first spec))
      ((tuple function-spec export-alias) (first spec))
      ((or macro-spec (tuple macro-spec export-alias))
       (error "Simple modules cannot export macros.")))))

(defstruct-read-only (simple-module (:conc-name sm.))
  (thunk :type function))

(defmethod print-object ((self simple-module) stream)
  (print-unreadable-object (self stream :type t)))

(def list-exports '%list-exports)

(defmethod module-ref ((sm simple-module) (key symbol))
  (funcall (sm.thunk sm) key))

(defmethod module-exports ((sm simple-module))
  (module-ref* sm list-exports))

(defmacro simple-module ((&rest exports) &body body)
  `(make-simple-module
    :thunk
    (mlet ,exports
      ,@body)))

(defmacro mlet (exports &body body)
  `(local*
     ,@body
     ;; The name for the lambda is just to make debugging easier.
     (named-lambda ,(string-gensym 'simple-module-lookup) (key)
       ,(mlet-get exports 'key))))

(defun mlet-get (exports key)
  (let* ((export-keys (mapcar #'export-keyword exports))
         (export-bindings (mapcar #'export-binding exports)))
    ;; No duplicate exports.
    (assert (length= export-keys (nub export-keys)))
    `(ecase ,key
       (,list-exports ',export-keys)
       ,@(mapcar #'list export-keys export-bindings))))
