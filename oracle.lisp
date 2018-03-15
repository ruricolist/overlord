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
  (:import-from :overlord/asdf
    :asdf-system-version)
  (:import-from :fset)
  (:export
   :oracle :oracle-name
   :var-oracle
   :env-oracle
   :system-version-oracle
   :dist-version-oracle))
(in-package :overlord/oracle)

;;; Oracles. Oracles let you depend on the environment: either the
;;; Lisp environment (bound variables, features) or the OS environment
;;; (environment variables). When you depend on an oracle, Overlord
;;; remembers the value of the oracle at the time of the dependency
;;; (or the lack of a value, if the oracle was unbound). If that value
;;; changes, then any targets that depend on the oracle are considered
;;; out of date.

;;; Note that, because Overlord remembers the value of the oracle,
;;; that value should be small -- a flag, a symbol, a file name. Note
;;; also that the value per se is not remembered, but a string
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

(defmethods oracle (self key)
  (:method make-load-form (self &optional env)
    (make-load-form-saving-slots self
                                 :slot-names '(key)
                                 :environment env))
  (:method print-object (self stream)
    (format stream "~a~s"
            (read-eval-prefix self stream)
            `(make ',(class-name-of self) :key ,key)))

  ;; Yes, these are circular; every subclass must define one or the
  ;; other.
  (:method fset:compare (self (other oracle))
    (if (target= self other)
        :equal
        (fset:compare-slots self other
                            #'class-name-of
                            #'oracle.key)))
  (:method target= (self (other oracle))
    (eql :equal (fset:compare self other)))


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
  (:method target-being-built-string (self)
    (fmt "oracle for ~a" key)))


;;; Var oracles.

;;; Oracles for Lisp variables are intended to allow a target to
;;; record the fact that it depends on some aspect of the compile time
;;; or read time environment (e.g. `*read-base*') and should be
;;; considered out of date if that changes.

(defclass var-oracle (oracle)
  ((key :reader var-oracle.var
        :initarg :var
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
    t))

(defclass package-oracle (name-oracle)
  ((key :type string
        :reader package-oracle.name
        :initform (package-name *package*)))
  (:documentation "Oracle that wraps the current package."))

(defmethods package-oracle (self (name key))
  (:method target= (self (other package-oracle))
    (string= name (package-oracle.name other))))

(defclass readtable-oracle (name-oracle)
  ((key :type delayed-symbol
        :initform (delay-symbol (readtable-name *readtable*))))
  (:documentation "Oracle that wraps the current readtable.
A name is extracted using `named-readtable:readtable-name'."))

(defmethods readtable-oracle (self (name key))
  (:method target= (self (other readtable-oracle))
    (delayed-symbol= name (name-oracle.name other))))

(defun var-oracle (var)
  (check-type var symbol)
  (cond ((eql var '*readtable*)
         (make 'readtable-oracle))
        ((eql var '*package*)
         (make 'package-oracle))
        ((cl-sym? var)
         (make 'cl-var-oracle :var var))
        (t (make 'var-oracle :var var))))


;;; Environment oracles.

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

(defun env-oracle (name)
  (make 'env-oracle
        :name (assure string name)))


;;; System version oracles.

;;; Using a system version oracle, you can depend on the version of an
;;; ASDF system. Note that if the system is not known to ASDF, then
;;; the version recorded is simply nil.

(defclass system-version-oracle (oracle)
  ((key :initarg :name
        :reader system-version-oracle.system-name
        :type string)))

(defmethods system-version-oracle (self (name key))
  (:method target-exists? (self)
    t)
  (:method oracle-value (self)
    (asdf-system-version name))
  (:method target= (self (other system-version-oracle))
    (string-equal name (system-version-oracle.system-name other))))

(defun system-version-oracle (name)
  (make 'system-version-oracle :name (assure string name)))


;;; Dist version oracles.

;;; Depend on the current version of a Quicklisp dist.

(defconst quicklisp "quicklisp")

(defun dist-exists? (dist)
  (and (find-package :ql-dist)
       (uiop:symbol-call :ql-dist :find-dist
                         (assure string dist))))

(defun ql-dist-version (&optional (dist-name quicklisp))
  (when (dist-exists? dist-name)
    (uiop:symbol-call :ql-dist :version dist-name)))

(defclass dist-version-oracle (oracle)
  ((key :initarg :name
        :reader dist-version-oracle.name
        :type string)))

(defmethods dist-version-oracle (self (name key))
  (:method target-exists? (self)
    (dist-exists? name))
  (:method oracle-value (self)
    (ql-dist-version name))
  (:method target= (self (other dist-version-oracle))
    (equal name (dist-version-oracle.name other))))

(defun dist-version-oracle (&optional (dist-name quicklisp))
  (make 'dist-version-oracle
        :name (assure string dist-name)))


;;;; Feature oracles.

;;; TODO We could do automatic translation (along the lines of
;;; trivial-features) to ensure that feature dependencies are
;;; portable?

(defclass feature-oracle (oracle)
  ((key :initarg :feature
        :type keyword
        :reader feature-oracle.feature)))

(defun feature-oracle (feature)
  (make 'feature-oracle
        :feature (assure keyword feature)))

(defmethods feature-oracle (self (feature key))
  (:method target-exists? (self)
    (featurep feature))
  (:method oracle-value (self)
    t)
  (:method target= (self (other feature-oracle))
    (eql feature (feature-oracle.feature other))))
