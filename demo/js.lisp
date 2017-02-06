(defpackage :overlord/demo/js
  (:use :cl :alexandria :serapeum :cl-js)
  (:nicknames :demo/js)
  (:import-from :overlord
    :dynamic-require-as
    :ensure-absolute
    :module-ref
    :module-exports)
  (:import-from :overlord/util :coerce-case)
  (:import-from :local-time)
  (:import-from :parse-js :parse-js)
  (:export :read-module :module-progn))

;;; TODO Getting values from an env.

(in-package :overlord/demo/js)

;;; XXX The version in cl-js actually inlines the environment in the
;;; expansion, so without this we would get invalid memory access
;;; errors every time `require' was called.
(defun cl-js::expand-global-lookup (prop)
  `(cl-js::gcache-lookup (load-time-value (cons nil (cl-js::make-cache (cl-js::intern-prop ,prop)))) *env*))

(defparameter *overlord-lib*
  (lret ((lib (empty-lib "Overlord")))
    (add-to-lib lib
      (.func "require" (spec)
        (lret* ((canonical (ensure-absolute (uiop:parse-unix-namestring spec)))
                (module (dynamic-require-as :overlord/demo/js canonical))
                (obj (js-module-obj module))))))))

(defstruct-read-only
    (js-module
     (:constructor make-js-module
         (obj &aux (exports-table (object-exports-table obj))
                   (exports-list (hash-table-keys exports-table)))))
  (obj :type js-obj)
  (exports-table :type hash-table)
  (exports-list :type list))

(defun object-keys (object)
  (collecting
    (js-for-in object #'collect :shallow)))

(defun js-fun->lisp-fun (fun)
  (lambda (&rest args)
    (apply #'cl-js::js-funcall fun args)))

(defun object-exports-table (object)
  (alist-hash-table
   (mapcar (lambda (key)
             (let* ((value (js-prop object key))
                    (key-string (coerce-case key))
                    (key (make-keyword key-string))
                    (fun (js-fun->lisp-fun value)))
               (cons key fun)))
           (object-keys object))
   :test 'eq))

(defmethod module-exports ((m js-module))
  (js-module-exports-list m))

(defmethod module-ref ((m js-module) key)
  (gethash key (js-module-exports-table m)))

(defun wrap-as-module (ast)
  `(:function nil ("exports") (,@(second ast) (:return (:name "exports")))))

(defun read-module (source stream)
  (declare (ignore source))
  `(module-progn
     (cl-js::wrap-js
      ,(cl-js::translate-ast
        (wrap-as-module
         (parse-js:parse-js stream))))))

(defmacro module-progn (&body body)
  (with-gensyms (fun obj mod)
    `(with-js-env (*overlord-lib*)
       (lret* ((,fun ,`(progn ,@body))
               (,obj (js-call ,fun *env* (js-obj)))
               (,mod (make-js-module ,obj)))))))
