(defpackage :overlord/oracle
  (:use :cl :alexandria :serapeum :trivia
    :overlord/target-protocol)
  (:shadowing-import-from :trivia :@)
  (:import-from :named-readtables
    :readtable-name)
  (:import-from :overlord/util
    :class-name-of
    :byte-array-to-hex-string)
  (:import-from :overlord/redo
    :redo-ifchange
    :redo-ifcreate)
  (:import-from :overlord/types
    :delayed-symbol
    :delayed-symbol=
    :force-symbol
    :delay-symbol
    :overlord-error)
  (:import-from :overlord/digest
    :digest-string)
  (:export
   :oracle
   :use-var
   :use-env
   :oracle-name))
(in-package :overlord/oracle)

;;; Oracles. Oracles let you depend on the environment: either the
;;; Lisp environment (bound variables) or the system environment
;;; (environment variables). When you depend on an oracle, Overlord
;;; remembers the value of the oracle at the time of the dependency
;;; (or the lack of a value, if the oracle was unbound). If that value
;;; changes, then any targets that depend on the oracle are considered
;;; out of date.

;;; At the moment, there are two kinds of oracles: oracles for Lisp
;;; variables, and oracles for environment variables. Oracles for Lisp
;;; variables are intended to allow a target to record the fact that
;;; it depends on some aspect of the compile time or read time
;;; environment (e.g. `*read-base*') and should be considered out of
;;; date if that changes.

;;; Note that, because Overlord remembers the value of the oracle,
;;; that value should be small -- a flag, a symbol, a file name. Note
;;; also that the value per se is not remember, but a string
;;; representation with `princ', so the value should be one with a
;;; meaningful literal representation.

(deftype oracle-value ()
  '(or
    number
    ;; NB boolean is a subtype of symbol.
    symbol
    string))

(defgeneric oracle-value (oracle))

(defclass oracle ()
  ((key :initarg :key
        :accessor oracle.key
        :reader oracle-name)))

(defun depends-on-oracle (&rest oracles)
  (do-each (oracle (reshuffle oracles))
    (if (target-exists? oracle)
        (redo-ifchange oracle)
        (redo-ifcreate oracle)))
  (values))

(defmethods oracle (self key)
  (:method make-load-form (self &optional env)
    (make-load-form-saving-slots self
                                 :slot-names '(key)
                                 :environment env))
  (:method print-object (self stream)
    (format stream "~a~s"
            (read-eval-prefix self stream)
            `(make ',(class-name-of self) :key ,key)))
  (:method hash-target (self)
    (sxhash (make-load-form self)))
  (:method oracle-value :around (self)
    (assure oracle-value
      (call-next-method)))
  (:method target-stamp (self)
    (let ((value (oracle-value self)))
      (if (stringp value)
          (byte-array-to-hex-string (digest-string value))
          (prin1-to-string (oracle-value self)))))
  (:method target= (self (other oracle))
    nil)
  (:method target-being-built-string (self)
    (fmt "oracle for ~a" key)))

(defclass var-oracle (oracle)
  ((key :reader var-oracle.var
        :type delayed-symbol)
   (sym :type symbol))
  (:default-initargs
   :var (required-argument :var))
  (:documentation "Oracle that wraps a special variable."))

(defmethods var-oracle (self (var key) sym)
  (:method initialize-instance :after (self &key var)
    (setf (oracle.key self) (delay-symbol var)))
  ;; Cache the symbol.
  (:method slot-unbound ((class t) self (slot-name (eql 'sym)))
    (setf sym (force-symbol var)))
  (:method oracle-value (self)
    (symbol-value sym))
  (:method target-exists? (self)
    (boundp sym))
  (:method target= (self (other var-oracle))
    (handler-case
        (eql sym (slot-value other 'sym))
      (overlord-error ()
        ;; The symbols haven't been, or can't be, resolved.
        (delayed-symbol= var (var-oracle.var other))))))

(defclass cl-var-oracle (oracle)
  ((key :type symbol
        :initarg :var))
  (:default-initargs
   :var (required-argument :var))
  (:documentation
   "Oracle that wraps a special variable in the CL package."))

(defun cl-sym? (sym)
  (eql (symbol-package sym)
       (find-package :cl)))

(defmethods cl-var-oracle (self (var key))
  (:method initialize-instance :after (self &key)
    (assert (cl-sym? var)))
  (:method oracle-value (self)
    (symbol-value var))
  (:method target-exists? (self)
    (boundp var))
  (:method target= (self (other cl-var-oracle))
    (eql var (slot-value other 'var))))

(defclass name-oracle (oracle)
  ((key :reader name-oracle.name))
  (:documentation
   "Oracle that extracts a name from a value instead of recording the value directly."))

(defmethods name-oracle (self (name key))
  (:method oracle-value (self)
    name)
  (:method target-exists? (self)
    t)
  (:method target= (self (other name-oracle))
    (equal name (name-oracle.name other))))

(defclass package-oracle (name-oracle)
  ((key :type string
        :initform (package-name *package*)))
  (:documentation "Oracle that wraps the current package."))

(defclass readtable-oracle (oracle)
  ((key :type delayed-symbol
        :initform (delay-symbol (readtable-name *readtable*))))
  (:documentation "Oracle that wraps the current readtable.
A name is extracted using `named-readtable:readtable-name'."))

(defmethods readtable-oracle (self (name key))
  (:method target= (self (other readtable-oracle))
    (delayed-symbol= name (name-oracle.name other))))

(defun use-var (var)
  (check-type var symbol)
  (depends-on-oracle
   (cond ((eql var '*readtable*)
          (make 'readtable-oracle))
         ((eql var '*package*)
          (make 'package-oracle))
         ((cl-sym? var)
          (make 'cl-var-oracle :var var))
         (t (make 'var-oracle :var var)))))

(defclass env-oracle (oracle)
  ((key :initarg :name
        :type string
        :reader env-oracle.name))
  (:default-initargs
   :name (required-argument :name))
  (:documentation "Oracle that wraps an environment variable."))

(defmethods env-oracle (self (name key))
  (:method oracle-value (self)
    (uiop:getenv name))
  (:method target-stamp (self)
    (~> self
        oracle-value
        digest-string
        byte-array-to-hex-string))
  (:method target-exists? (self)
    (uiop:getenvp name))
  (:method target= (self (other env-oracle))
    (equal name (env-oracle.name other))))

(defun use-env (&rest names)
  (let ((oracles
          (mapcar (op (make 'env-oracle :name _))
                  names)))
    (apply #'depends-on-oracle oracles)))
