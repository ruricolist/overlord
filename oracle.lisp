(defpackage :overlord/oracle
  (:use :cl :alexandria :serapeum :trivia)
  (:shadowing-import-from :trivia :@)
  (:import-from :overlord/util
    :class-name-of)
  (:import-from :overlord/redo
    :redo-ifchange
    :redo-ifcreate)
  (:import-from :overlord/types
    :delayed-symbol
    :force-symbol
    :delay-symbol)
  (:export
   :oracle
   :oracle-exists?
   :oracle-timestamp
   :oracle=
   :hash-oracle
   :use-var
   :use-env))
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

(defgeneric oracle-timestamp (oracle))

(defgeneric oracle-exists? (oracle))

(defgeneric oracle-value (oracle))

(defgeneric oracle= (oracle1 oracle2))

(defgeneric hash-oracle (oracle))

(defclass oracle ()
  ((key :initarg :key
        :accessor oracle.key)))

(defun depends-on-oracle (oracle)
  (check-type oracle oracle)
  (if (oracle-exists? oracle)
      (redo-ifchange oracle)
      (redo-ifcreate oracle))
  (oracle-value oracle))

(defmethods oracle (self key)
  (:method make-load-form (self &optional env)
    (make-load-form-saving-slots self
                                 :slot-names '(key)
                                 :environment env))
  (:method print-object (self stream)
    (format stream "~a~s"
            (read-eval-prefix self stream)
            `(make ',(class-name-of self) :key ,key)))
  (:method hash-oracle (self)
    (sxhash (make-load-form self)))
  (:method oracle-value :around (self)
    (assure oracle-timestamp
      (call-next-method)))
  (:method oracle-timestamp (self)
    (prin1-to-string (oracle-value self)))
  (:method oracle= (self (other oracle))
    nil))

(defclass var-oracle (oracle)
  ((key :reader var-oracle.var
        :type delayed-symbol)
   (sym :type symbol))
  (:default-initargs
   :var (required-argument :var)))

(defmethods var-oracle (self (var key) sym)
  (:method initialize-instance :after (self &key var)
    (setf (oracle.key self) (delay-symbol var)))
  ;; Cache the symbol.
  (:method slot-unbound ((class t) self (slot-name (eql 'sym)))
    (setf sym (force-symbol var)))
  (:method oracle-value (self)
    (symbol-value sym))
  (:method oracle-exists? (self)
    (boundp sym))
  (:method oracle= (self (other var-oracle))
    (handler-case
        (eql sym (slot-value other 'sym))
      (overlord-error ()
        (multiple-value-ematch
            (values var (var-oracle.var other))
          (((delayed-symbol p1 s1)
            (delayed-symbol p2 s2))
           (and (equal p1 p2)
                (equal s1 s2))))))))

(defclass cl-var-oracle (oracle)
  ((key :type symbol
        :initarg :var))
  (:default-initargs
   :var (required-argument :var)))

(defun cl-sym? (sym)
  (eql (symbol-package sym)
       (find-package :cl)))

(defmethods cl-var-oracle (self (var key))
  (:method initialize-instance :after (self &key)
    (assert (cl-sym? var)))
  (:method oracle-value (self)
    (symbol-value var))
  (:method oracle-exists? (self)
    (boundp var))
  (:method oracle= (self (other cl-var-oracle))
    (eql var (slot-value other 'var))))

(defun use-var (var)
  (depends-on-oracle
   (if (cl-sym? var)
       (make 'cl-oracle :var var)
       (make 'var-oracle :var var))))

(defclass env-oracle (oracle)
  ((key :initarg :name
        :type string
        :reader env-oracle.name))
  (:default-initargs
   :name (required-argument :name)))

(defmethods env-oracle (self (name key))
  (:method oracle-value (self)
    (uiop:getenv name))
  (:method oracle-timestamp (self)
    (oracle-value self))
  (:method oracle-exists? (self)
    (uiop:getenvp name))
  (:method oracle= (self (other env-oracle))
    (equal name (env-oracle.name other))))

(defun use-env (name)
  (depends-on-oracle (make 'env-oracle :name name)))
