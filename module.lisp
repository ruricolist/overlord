(defpackage #:overlord/module
  (:use #:cl #:alexandria #:serapeum #:overlord/util)
  (:export
   #:package-exports
   #:module-ref
   #:module-ref*
   #:module-exports
   #:module-static-exports #:module-static-exports/cache
   #:with-static-exports-cache
   #:current-module
   #:current-module-cell
   #:current-module-source
   #:current-module-lang
   #:current-module-meta))

(in-package #:overlord/module)

;;; Actually defined in target.lisp.
(declaim (notinline module-static-exports))

;;; Generic functions.

(defgeneric module-ref (module name)
  (:documentation "Get the value of NAME in MODULE."))

(defgeneric module-exports (module)
  (:documentation "A list of names exported by MODULE."))

(defsubst module-ref* (module name)
  "Entry point for calling `module-ref'.
Inlinable, and skips generic dispatch for some common types."
  (declare (optimize
            (speed 3)
            (safety 1)
            (debug 0)
            (compilation-speed 0)
            (space 0)))
  (typecase module
    (function (funcall module name))
    (hash-table (gethash name module))
    (t (module-ref module name))))



(defvar-unbound *static-exports-cache*
  "You can bind this to a dictionary to prevent redundant computation
  of static exports.")
(declaim (type hash-table *static-exports-cache*))

(def static-exports-lock
  (bt:make-recursive-lock "Static exports lock"))

(defun module-static-exports/cache (lang source)
  (check-type lang symbol)
  (if (boundp '*static-exports-cache*)
      (let ((c *static-exports-cache*))
        (assert (hash-table-p c))
        (assert (eql (hash-table-test c) 'equal))
        (mvlet* ((key (cons lang source))
                 (cached cached?
                  (synchronized (static-exports-lock)
                    (gethash key c))))
          (if cached? cached
              (let ((exports (module-static-exports lang source)))
                (synchronized (static-exports-lock)
                  (setf (gethash key c) exports))))))
      (module-static-exports lang source)))

(defmacro with-static-exports-cache ((&key) &body body)
  `(let ((*static-exports-cache* (dict)))
     ,@body))
