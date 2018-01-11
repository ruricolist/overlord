(defpackage #:overlord/module
  (:use #:cl #:alexandria #:serapeum #:overlord/util)
  (:import-from #:overlord/types
    #:overlord-error)
  (:export
   #:package-exports
   #:validate-module
   #:make-module
   #:module-ref #:module-ref*
   #:module-exports #:module-exports*
   #:module-static-exports #:module-static-exports/cache
   #:no-such-export
   #:with-static-exports-cache
   #:current-module
   #:current-module-cell
   #:current-module-source
   #:current-module-lang
   #:current-module-meta
   #:default-export-module))

(in-package #:overlord/module)

;;; Generic functions.

(defgeneric module-ref (module name)
  (:documentation "Get the value of NAME in MODULE."))

(defgeneric module-exports (module)
  (:documentation "A list of names exported by MODULE."))

(defgeneric module-static-exports (lang source)
  (:documentation "Get static exports from LANG and SOURCE."))

(defun validate-module (module)
  "Validate that MODULE belongs to a class that implements the
protocol for modules."
  (if (null module)
      (error "~s cannot be used as a module." nil)
      (let ((class (class-of module))
            (class-t (class-of t)))
        (or (and (find-method #'module-ref nil (list class class-t)
                              nil)
                 (find-method #'module-exports nil (list class)
                              nil)
                 module)
            (error "Invalid module: ~a.
A module must have methods for both ~s and ~s."
                   module
                   'module-ref
                   'module-exports)))))



;;; Distinguished module objects.

(defcondition no-such-export (overlord-error)
  ((module :initarg :module)
   (key :initarg :key :type symbol))
  (:report (lambda (c s)
             (with-slots (module key) c
               (format s "Module ~a does not export ~a"
                       module key)))))

(defun empty-exports-table (module key)
  (error 'no-such-export
         :module module
         :key key))

(defstruct-read-only (module (:conc-name __module-)
                             (:constructor __make-module))
  (exports nil :type list)
  (exports-table #'empty-exports-table :type function))

(defmethod module-exports ((module module))
  (__module-exports module))

(defmethod module-ref ((module module) key)
  (funcall (__module-exports-table module) key))

(defun make-module (&key exports exports-table)
  (assert (every #'symbolp exports))
  (__make-module :exports exports
                 :exports-table exports-table))

(defun default-export-module (default)
  (make-module :exports-table (default-export-table default)))

(defun default-export-table (default)
  (lambda (module key)
    (if (eql key :default) default
        (error "Module ~a has no export named ~a" module key))))


;;; Actual entry points.

(defconst flank-speed
  '((speed 3)
    (safety 1)
    (debug 0)
    (compilation-speed 0)
    (space 0)))

(defsubst module-ref* (module name)
  "Entry point for calling `module-ref'.
Inlinable, and skips generic dispatch for some common types."
  (declare (optimize . #.flank-speed))
  (typecase module
    (function (funcall module name))
    (hash-table (gethash name module))
    (module (funcall (__module-exports-table module) name))
    (t (module-ref module name))))

(defsubst module-exports* (module)
  (declare (optimize . #.flank-speed))
  (typecase module
    (module (__module-exports module))
    (hash-table (hash-table-keys module))
    (t (module-exports module))))



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
