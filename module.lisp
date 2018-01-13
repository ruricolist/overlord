(defpackage #:overlord/module
  (:use #:cl #:alexandria #:serapeum #:overlord/util)
  (:import-from #:overlord/types
    #:overlord-error)
  (:export
   #:package-exports
   #:validate-module
   #:make-module
   #:module-ref #:module-ref* #:module-fn-ref
   #:module-exports #:module-exports*
   #:module-static-exports
   #:current-module
   #:current-module-cell
   #:current-module-source
   #:current-module-lang
   #:current-module-meta
   #:default-export-module
   #:module-error
   #:module-error-module
   #:no-such-export
   #:not-a-module))

(in-package #:overlord/module)

;;; Generic functions.

(defcondition module-error (overlord-error)
  ((module :initarg :module
           :reader module-error-module)))

(defcondition not-a-module (module-error)
  ()
  (:report (lambda (c s)
             (with-slots ((x module)) c
               (format s "Not a module: ~a" x)))))

(defcondition invalid-module (not-a-module)
  ()
  (:report (lambda (c s)
             (with-slots ((x module)) c
               (format s "~a cannot be a module." x)))))

(defcondition no-such-export (module-error)
  ((key :initarg :key :type symbol))
  (:report (lambda (c s)
             (with-slots (module key) c
               (format s "Module ~a does not export ~a."
                       module key)))))

(defgeneric module-ref (module name)
  (:documentation "Get the value of NAME in MODULE.")
  (:method (module name)
    (declare (ignore name))
    (error 'not-a-module
           :module module)))

(defgeneric module-exports (module)
  (:documentation "A list of names exported by MODULE.")
  (:method (module)
    (error 'not-a-module
           :module module)))

(defgeneric module-static-exports (lang source)
  (:documentation "Get static exports from LANG and SOURCE.
Returns two values: a list of static exports, and a second value that is T if the exports could be statically determined."))

(defun validate-module (module)
  "Validate that MODULE can be used as a module."
  (when (null module)
    (error 'invalid-module :module module))
  ;; `module-exports' signals `not-a-module' if there is no defined
  ;; method.
  (module-exports module)
  module)



;;; Distinguished module objects.

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
  (funcall (__module-exports-table module) module key))

(defun make-module (&key exports exports-table)
  (assert (every #'symbolp exports))
  (__make-module :exports exports
                 :exports-table exports-table))

(defun default-export-module (default)
  (make-module
   :exports '(:default)
   :exports-table (default-export-table default)))

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
    (module (funcall (__module-exports-table module) module name))
    (t (module-ref module name))))

(defsubst module-exports* (module)
  (declare (optimize . #.flank-speed))
  (typecase module
    (module (__module-exports module))
    (hash-table (hash-table-keys module))
    (t (module-exports module))))

;;; TODO Expand this differently depending on the lexical environment
;;; (speed vs. safety).

(-> module-fn-ref (t symbol) function)
(defsubst module-fn-ref (module name)
  "Exactly like `module-ref*', but has a signature that says it
returns a function."
  (assure function (module-ref* module name)))
