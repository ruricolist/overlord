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
    :absolute-pathname
    :delayed-symbol
    :delayed-symbol=
    :force-symbol
    :delay-symbol
    :overlord-error
    :wild-pathname)
  (:import-from :overlord/digest
    :string-digest-string)
  (:import-from :overlord/asdf
    :asdf-system-version)
  (:import-from :overlord/util
    :version-major-version)
  (:import-from :cmd :$cmd)
  (:import-from :fset)
  (:import-from #:cl-strftime
    #:format-time)
  (:import-from #:local-time
    #:now)
  (:import-from #:cl-murmurhash
    #:murmurhash)
  (:export
   :oracle
   :oracle-question
   :oracle-answer
   :var-oracle
   :env-oracle
   :system-version-oracle
   :feature-oracle
   :dist-version-oracle
   :function-oracle
   :daily-oracle
   :--version
   :glob-target))
(in-package :overlord/oracle)

;;; TODO Would it be worthwhile to provide oracles for optimization
;;; qualities (speed, debug, safety)? You could get them through
;;; introspect-environment.

(deftype oracle-answer ()
  '(or
    number
    ;; NB boolean is a subtype of symbol.
    symbol
    string))

(defgeneric oracle-answer (oracle))

(defclass oracle ()
  ((question :initarg :question
             :reader oracle-question))
  (:documentation "Oracles let you depend on aspects of the Lisp or OS
environment.

When you depend on an oracle, Overlord remembers the answer the oracle
gave at the time of the dependency \(or the lack of an answer, if the
oracle was unbound). If that answer changes, then any targets that
depend on the oracle are considered out of date."))

(defmethods oracle (self question)
  (:method make-load-form (self &optional env)
    (make-load-form-saving-slots self
                                 :slot-names '(question)
                                 :environment env))
  (:method print-object (self stream)
    (format stream "~a~s"
            (read-eval-prefix self stream)
            `(make-instance ',(class-name-of self) :question ,question)))

  (:method fset:compare (self (other oracle))
    (fset:compare-slots self other
                        #'class-name-of
                        #'oracle-question))
  (:method target= (self (other oracle))
    (eql :equal (fset:compare self other)))


  (:method hash-target (self)
    (sxhash (make-load-form self)))
  (:method oracle-answer :around (self)
    (assure oracle-answer
      (call-next-method)))
  (:method target-stamp (self)
    (let ((answer (oracle-answer self)))
      (if (stringp answer)
          (string-digest-string answer)
          (prin1-to-string answer))))
  (:method target-node-label (self)
    (fmt "oracle for ~a" question)))


;;; Var oracles.

(defclass var-oracle (oracle)
  ((question :reader var-oracle.var
             :initarg :var
             :type delayed-symbol)
   (sym :type symbol
        :documentation "Cache for the resolved symbol."))
  (:default-initargs
   :var (required-argument :var))
  (:documentation "Oracle that wraps a special variable.

Oracles for Lisp variables are intended to allow a target to
record the fact that it depends on some aspect of the compile time
or read time environment (e.g. `*read-base*') and should be
considered out of date if that changes."))

(defmethods var-oracle (self (var question) sym)
  (:method initialize-instance :after (self &key var)
    (setf var (delay-symbol var)))
  ;; Cache the symbol.
  (:method slot-unbound ((class t) self (slot-name (eql 'sym)))
    (setf sym (force-symbol var)))
  (:method oracle-answer (self)
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
  ((question :type symbol
             :initarg :var))
  (:default-initargs
   :var (required-argument :var))
  (:documentation
   "Oracle that wraps a special variable in the CL package."))

(defun cl-sym? (sym)
  (eql (symbol-package sym)
       (find-package :cl)))

(defmethods cl-var-oracle (self (var question))
  (:method initialize-instance :after (self &key)
    (assert (cl-sym? var)))
  (:method oracle-answer (self)
    (symbol-value var))
  (:method target-exists? (self)
    (boundp var))
  (:method target= (self (other cl-var-oracle))
    (eql var (oracle-question other))))

(defclass fixed-question-oracle (oracle)
  ((question :reader fixed-question-oracle.name))
  (:documentation
   "Oracle that extracts a name from a value instead of recording the
value directly. The question is also the answer."))

(defmethods fixed-question-oracle (self (name question))
  (:method oracle-answer (self)
    name)
  (:method target-exists? (self)
    t)
  (:method target= (self (other fixed-question-oracle))
    (eql name (oracle-question other))))

(defclass package-oracle (fixed-question-oracle)
  ((question :type string
             :reader package-oracle.name
             :initform (package-name *package*)))
  (:documentation "Oracle that wraps the current package."))

(defmethods package-oracle (self (name question))
  (:method target= (self (other package-oracle))
    (string= name (package-oracle.name other))))

(defclass readtable-oracle (fixed-question-oracle)
  ((question :type delayed-symbol
             :initform (delay-symbol (readtable-name *readtable*))))
  (:documentation "Oracle that wraps the current readtable.

A name is extracted using `named-readtable:readtable-name'."))

(defmethods readtable-oracle (self (name question))
  (:method target= (self (other readtable-oracle))
    (delayed-symbol= name (fixed-question-oracle.name other))))

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
  ((question :initarg :name
             :type string
             :reader env-oracle.name))
  (:default-initargs
   :name (required-argument :name))
  (:documentation "Oracle that wraps an environment variable."))

(defmethods env-oracle (self (name question))
  (:method oracle-answer (self)
    (uiop:getenv name))
  (:method target-stamp (self)
    (string-digest-string (oracle-answer self)))
  (:method target-exists? (self)
    (uiop:getenvp name))
  (:method target= (self (other env-oracle))
    (equal name (env-oracle.name other))))

(defun env-oracle (name)
  (make 'env-oracle
        :name (assure string name)))


;;; System version oracles.

(defclass system-version-oracle (oracle)
  ((question :initarg :name
             :reader system-version-oracle.system-name
             :type string))
  (:documentation "Using a system version oracle, you can depend on
the major version of an ASDF system.

Note that if the system is not known to ASDF, then the version
recorded is simply nil."))

(defmethods system-version-oracle (self (name question))
  (:method target= (self (other system-version-oracle))
    (let ((other-name
            (system-version-oracle.system-name other)))
      (string-equal name other-name)))
  (:method target-exists? (self)
    t)
  (:method oracle-answer (self)
    (version-major-version (asdf-system-version name)))
  (:method target-node-label (self)
    (fmt "system major version ~a" name)))

(defun system-version-oracle (name)
  (make 'system-version-oracle :name (string-downcase name)))


;;; Dist version oracles.

;;; Depend on the current version of a Quicklisp dist.

(defconst quicklisp "quicklisp")

(defun dist-exists? (dist)
  (and (find-package :ql-dist)
       (uiop:symbol-call :ql-dist :find-dist
                         (assure string dist))))

(defun ql-dist-version (&optional (dist-name quicklisp))
  (when-let (dist (dist-exists? dist-name))
    (uiop:symbol-call :ql-dist :version dist)))

(defclass dist-version-oracle (oracle)
  ((question :initarg :name
             :reader dist-version-oracle.name
             :type string))
  (:documentation "An oracle that reports the current version of a
  Quicklisp dist.

By default this is the Quicklisp dist itself.")
  (:default-initargs
   :name quicklisp))

(defmethods dist-version-oracle (self (name question))
  (:method target-exists? (self)
    (dist-exists? name))
  (:method oracle-answer (self)
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
  ((question :initarg :feature
             :type keyword
             :reader feature-oracle.feature))
  (:documentation "An oracle that wraps whether a particular keyword
  is present in `*features*'."))

(defun feature-oracle (feature)
  (make 'feature-oracle
        :feature (assure keyword feature)))

(defmethods feature-oracle (self (feature question))
  (:method target-exists? (self)
    (featurep feature))
  (:method oracle-answer (self)
    t)
  (:method target= (self (other feature-oracle))
    (eql feature (feature-oracle.feature other))))


;;; Date oracles.

;;; For targets that should only be built once a day.

(defun todays-date-string ()
  (format-time nil "%F" (now)))

(defun daily-oracle ()
  "Depend on today's date.
This is for targets that should be no more than one a day."
  (function-oracle 'todays-date-string))


;;; Version oracles.

(defun get-version (command)
  ($cmd command "--version"))

(defun --version (command)
  "An oracle that monitors the version of COMMAND (by calling it with
an argument of `--version'."
  (function-oracle 'get-version command))


;;; Glob oracles.

(defun wildcard-hash (wildcard)
  (declare ((and absolute-pathname wild-pathname) wildcard))
  (let* ((files (directory wildcard))
         (files (map 'vector #'namestring files))
         (files (sort files #'string<))
         (hash (murmurhash files)))
    (print (integer-length hash))
    (fmt "murmurhash3:~(~x~)" hash)))

(defun glob-target (wildcard)
  (check-type wildcard (and absolute-pathname wild-pathname))
  (function-oracle 'wildcard-hash wildcard))


;;; Function oracles.

;;; For easy extension.
(defclass function-oracle (oracle)
  ((question :initarg :function
             :type delayed-symbol
             :reader function-oracle.delayed-symbol)
   (args
    :initarg :args
    :type list
    :reader function-oracle.args))
  (:documentation "An oracle for a user-supplied function.

The function must be supplied as a symbol.")
  (:default-initargs
   :args nil))

(defmethods function-oracle (self (fn question) args)
  (:method target-exists? (self)
    (fboundp
     (ignoring overlord-error
       (force-symbol fn))))
  (:method oracle-answer (self)
    (apply (force-symbol fn) args))
  (:method print-object (self stream)
    (format stream "~a~s"
            (read-eval-prefix self stream)
            `(make-instance 'function-oracle
                            :function ,fn
                            :args '(,@args)))))

(defun function-oracle (function-name &rest args)
  (check-type function-name symbol)
  (make 'function-oracle
        :function (delay-symbol function-name)
        :args args))
