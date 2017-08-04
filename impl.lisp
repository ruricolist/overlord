(defpackage :overlord/impl
  (:use
    :cl
    :alexandria
    :serapeum
    :local-time

    :uiop/filesystem
    :uiop/pathname

    ;; Resettable global state.
    :overlord/global-state
    ;; Types common to the project.
    :overlord/types
    ;; Special variables.
    :overlord/specials
    ;; Module protocol.
    :overlord/module
    ;; Utilities.
    :overlord/util
    ;; How to infer the base for the current package.
    :overlord/base
    ;; The #lang syntax.
    :overlord/hash-lang
    ;; Import sets.
    :overlord/import-set
    ;; Logging.
    :overlord/message
    ;; Time tuples.
    :overlord/time-tuple
    ;; Running shell commands.
    :overlord/cmd
    ;; The database.
    :overlord/kv)
  (:import-from :fset)
  (:import-from :trivia
    :match :ematch)
  ;; Portability shim for "global" or "static" vars. They have global
  ;; scope, but cannot be rebound.
  (:import-from :global-vars
    :define-global-var)
  ;; How to compile a program to a fasl.
  (:import-from :overlord/compile-to-file
    :compile-to-file :load-as-module :fasl-ext)
  ;; How to test similarity (CLHS 3.2.4.2.2) of externalizable objects.
  (:import-from :overlord/similarity
    :similar?)
  (:import-from :uiop
    :implementation-identifier
    :with-temporary-file
    :rename-file-overwriting-target
    :xdg-cache-home)
  ;; How to escape names for use in pathnames.
  (:import-from :quri :url-encode)
  (:import-from :cl-custom-hash-table
    :define-custom-hash-table-constructor
    :with-custom-hash-table)
  (:shadow :defconfig :import :now :file-size)
  ;; Shadow for style.
  (:shadow
   :defmacro                            ;Hygienic pathnames.
   :if                                  ;Always ternary.
   :if-let                              ;Ditto.
   :cond                                ;Require exhaustive.
   :set                                 ;Use symbol-value.
   :defclass                            ;Force checking slot types.
   :typecase                            ;Use typecase-of instead.
   :etypecase                           ;Use etypecase-of instead.
   :ctypecase                           ;Use ctypecase-of instead.
   :file-write-date                     ;Use file-mtime instead.
   :pathname                            ;Use ensure-pathname.
   :multiple-value-bind                 ;Use receive.
   )
  (:export
   ;; Defining and building targets.
   :defconfig
   :defconfig/deps
   :var-target
   :defvar/deps
   :deftask
   :file-target
   :undefine-target
   :build :unbuild :forget-target
   :run

   :*target*
   :ensure-absolute
   :extension
   :defpattern

   ;; Languages.
   :lang :lang-name :hash-lang-name
   :load-module
   :expand-module
   :depends-on :depends-on*
   :depends-on-all :depends-on-all*
   :package-expander :package-reader :module-progn-in
   :with-meta-language
   :load-same-name-system
   :define-loader-language

   ;; Loading.
   :*language* :*source*
   :read-lang-name
   :require-as
   :dynamic-require-as

   ;; Module protocol.
   :module-meta

   :import :import/local
   :import-as-package
   :import-as-subpackage
   :with-imports
   :reintern :reinterning
   :*file-local-variables*
   :find-module

   ;; Emacs integration.
   :require-for-emacs
   :expand-module-for-emacs

   ;; Freezing the state of the Lisp image.
   :freeze :freeze-policy
   :unfreeze
   :file))

(in-package :overlord/impl)


;;; Shadows and preferred alternatives.

(deftype pathname ()
  'cl:pathname)

;;; Resolve literal relative pathnames in macro bodies at compile
;;; time.

(cl:defmacro defmacro (name args &body body)
  (receive (body decls docstring)
      (parse-body body :documentation t)
    `(cl:defmacro ,name ,args
       ,@(unsplice docstring)
       ,@decls
       ;; Expand all literal relative pathnames.
       (hygienic-pathnames
        (local ,@body)))))

;;; Conditionals should always be exhaustive.

(defmacro if (test then else)
  "Like `cl:if', but require two branches."
  `(cl:if ,test ,then ,else))

(defmacro if-let (bindings then else)
  "Like `alexandria:if-let', but require two branches."
  `(alexandria:if-let ,bindings ,then ,else))

(defmacro cond (&whole whole &body clauses)
  "Like `cl:cond', but require a default clause."
  (unless (and clauses (eql (car (lastcar clauses)) 't))
    (warn "Non-exhaustive COND: ~s" whole))
  `(cl:cond ,@clauses))

(defmacro defclass (name supers &body (slots . options))
  "Like `cl:defclass', but try to force slot types to be checked.
Works for SBCL, at least."
  `(locally (declare (optimize (safety 3)))
     (cl:defclass ,name ,supers
       ,slots
       ,@options)))

;;; Make sure that we treat package names consistently, whether or not
;;; the Lisp implementation uses package-relative nicknames.

(defmacro with-absolute-package-names ((&key) &body body)
  `(let ((*package* (find-package :keyword)))
     ,@body))

;; Maybe this should shadow `find-package'; I'm not sure.
(defun resolve-package (package-designator)
  "Like `find-package', but make sure the package is resolved in
absolute terms even if the Lisp implementation supports local package
nicknames."
  (with-absolute-package-names ()
    (find-package package-designator)))

(defun file-mtime (pathname)
  "Same as `file-write-date'.
This is provided in case we ever want to offer more precise timestamps
on Lisp/OS/filesystem combinations that support it."
  (cl:file-write-date pathname))


;;; Types.

;;; Timestamps can be exact timestamps (from local-time), universal
;;; times, the singleton `never' (which means the target
;;; unconditionally needs building) and the singleton `far-future'
;;; (which means the target unconditionally does not need building).

;;; Alternatively, a timestamp can be a `time-tuple', which consists
;;; of a universal time and a count of internal time units. A time
;;; tuple does not establish a specific time but does establish an
;;; ordering, and is used instead of a local-time timestamp on
;;; implementation/platform combinations (e.g. Clozure on Windows)
;;; where local-time timestamps are too fuzzy to be useful.

(define-singleton-type never)
(define-singleton-type far-future)

(declaim (type function *now-function*))
(defvar *now-function*
  (let ((local-time-resolution-bad?
          (loop repeat 1000
                for timestamp = (local-time:now)
                always (zerop (local-time:timestamp-microsecond
                               timestamp)))))
    (if local-time-resolution-bad?
        #'time-tuple
        #'local-time:now)))

(defun now ()
  (funcall *now-function*))

(deftype target-timestamp ()
  '(or timestamp
    time-tuple
    universal-time
    never
    far-future))

(deftype file-size ()
  '(integer 0 *))

(defsubst file-size (file)
  (serapeum:file-size file))

(declaim (inline file-meta))
(defstruct-read-only (file-meta
                      (:conc-name file-meta.)
                      ;; Define the constructor here so it can be
                      ;; inlined.
                      (:constructor file-meta
                          (file &aux (size (file-size file))
                                     (timestamp (target-timestamp file)))))
  "Metadata to track whether a file has changed."
  ;; TODO hash?
  (size :type (integer 0 *))
  (timestamp :type target-timestamp))

(defun file-meta= (x y)
  (and (typep x 'file-meta)
       (typep y 'file-meta)
       (compare #'= #'file-meta.size x y)
       (compare #'target-timestamp= #'file-meta.timestamp x y)))

(defconst deleted :deleted)

(deftype stamp ()
  `(or target-timestamp
       (eql ,deleted)
       string
       file-meta))

(deftype lang-name ()
  ;; Keywords can be language names.
  '(and symbol (not (member t nil))))

(deftype hash-lang-name ()
  '(and lang-name (satisfies hash-lang-name?)))

(defun hash-lang-name? (x)
  (and (typep x 'lang-name)
       (valid-lang-name? (string x))))

(deftype lang ()
  '(or package lang-name))

(defclass ref ()
  ((name
    :reader ref.name
    :type t
    :initarg :name))
  (:documentation "Base class for different kinds of by-name references."))

(defmethods ref (self name)
  (:method initialize-instance :after (self &key &allow-other-keys)
    (unless (slot-boundp self 'name)
      (error* "No name")))

  (:method make-load-form (self &optional env)
    (declare (ignore env))
    `(make-instance ,(class-of self)
                    :name ,name))

  (:method fset:compare (self (other ref))
    (fset:compare-slots self other #'class-of #'ref.name)))

(fset:define-cross-type-compare-methods ref)

(defclass directory-ref (ref)
  ((name
    :initarg :path
    :type (and absolute-pathname directory-pathname)))
  (:documentation "A reference to a directory."))

(defun directory-ref (name)
  "Wrap NAME as a directory reference."
  (etypecase-of (or string directory-pathname) name
    (string (directory-ref (ensure-pathname name :want-pathname t)))
    (directory-pathname (make 'directory-ref :name name))))

(defclass package-ref (ref)
  ((name :type string)
   (nicknames
    :type list
    :initarg :nicknames
    :reader package-ref.nicknames)
   (use-list
    :type list
    :initarg :use
    :reader package-ref.use-list))
  (:documentation "A reference to a package.")
  (:default-initargs
   :nicknames nil
   :use-list nil))

(defmethods package-ref (self name nicknames use-list)
  (:method make-load-form (self &optional env)
    (make-load-form-saving-slots self
                                 :slot-names '(name nicknames use-list)
                                 :environment env)))

(defun package-ref (name &key nicknames use-list)
  (make 'package-ref :name (string name)
                     :nicknames nicknames
                     :use-list use-list))

(defclass pattern-ref (ref)
  ;; Note that the pattern slot has a silly type: a pattern ref can be
  ;; either a symbol or an instance of `pattern', which is not yet
  ;; defined. Being able to directly pass in patterns will be useful
  ;; later when we bootstrap support for languages.
  ((pattern
    :initarg :pattern
    :type (or symbol standard-object)
    :accessor pattern-ref.pattern)
   (name
    :type pathname
    :initarg :input
    :accessor pattern-ref.input)
   (output
    :type pathname
    :accessor pattern-ref.output)))

(defgeneric merge-pattern-defaults (pattern input)
  (:method (pattern input)
    (values (merge-input-defaults pattern input)
            (merge-output-defaults pattern input))))

(defgeneric merge-input-defaults (pattern input)
  (:method (pattern input)
    ;; Note that we're merging the *provided* inputs and
    ;; outputs into the defaults, rather than vice-versa.
    (merge-pathnames* (pattern.input-defaults pattern)
                      input)))

(defgeneric merge-output-defaults (pattern input)
  (:method (pattern input)
    (merge-pathnames (pattern.output-defaults pattern)
                     input)))

(defmethods pattern-ref (self (input name) output pattern)
  (:method initialize-instance :after (self &key)
    ;; Merge in the defaults for inputs and outputs.
    (let ((pattern (find-pattern pattern)))
      (setf (values input output)
            (merge-pattern-defaults pattern input))))

  (:method print-object (self stream)
    (if *print-escape*
        (format stream "#.~s"
                (make-load-form self))
        (print-unreadable-object (self stream :type t)
          (format stream "~a -> ~a" input output))))

  (:method make-load-form (self &optional env)
    (make-load-form-saving-slots self
                                 :slot-names '(pattern name output)
                                 :environment env))

  (:method fset:compare (self (other pattern-ref))
    (fset:compare-slots self other
                        #'pattern-ref.input
                        #'pattern-ref.pattern
                        #'pattern-ref.output)))

(defun pattern-ref (pattern file)
  "Make a pattern reference, or a list of pattern references."
  ;; TODO Should this be absolute?
  ;; Shouldn't this complain about relative vs. absolute?
  (ensure-pathnamef file)
  (assure (or pattern-ref (list-of pattern-ref))
    (if (wild-pathname-p file)
        (mapcar (op (pattern-ref pattern _))
                (directory* file))
        (make 'pattern-ref
              :pattern pattern
              :input file))))

(defclass module-cell ()
  ((timestamp
    :type target-timestamp
    :initform never
    :accessor module-cell.timestamp)
   (lang
    :initarg :lang
    :type lang-name
    :reader module-cell.lang)
   (source
    :initarg :source
    :type (and file-pathname tame-pathname)
    :accessor module-cell.source)
   (meta
    :initform nil
    :type plist
    :accessor module-cell.meta
    :documentation "Metadata about the module. This persists even when
the module is reloaded.")
   (module
    :initform nil
    :accessor module-cell.module)
   (lock
    :reader monitor))
  (:documentation "Storage for a module.

Each lang+source combination gets its own module cell with its own
unique identity.

The module itself may be reloaded, but the module cell is interned
forever."))

;;; Compiler macro needs to appear as soon as possible to satisfy
;;; SBCL.
(define-compiler-macro module-cell (&whole call lang path)
  (cond ((packagep lang)
         `(module-cell ,(lang-name lang) ,path))
        ((or (quoted-symbol? lang) (keywordp lang))
         (typecase-of (or string pathname) path
           (string `(module-cell ,lang ,(ensure-pathname path :want-pathname t)))
           (pathname
            (let ((path (resolve-target path (base)))) ;Resolve now, while `*base*' is bound.
              `(load-time-value
                (%module-cell ,lang ,path))))
           (otherwise call)))
        ((constantp lang)
         (let ((val (eval lang)))
           (if (eql val lang) call
               `(module-cell ,val ,path))))
        (t call)))

(defun module-cell-meta (cell key)
  (synchronized (cell)
    (getf (module-cell.meta cell) key)))

(defun (setf module-cell-meta) (value cell key)
  (synchronized (cell)
    (setf (getf (module-cell.meta cell) key)
          value)))

(defplace module-meta (lang path key)
  (module-cell-meta (module-cell lang path) key))

(define-compiler-macro module-meta (lang path key)
  "Expand the call to module-cell at compile time so it can be
resolved at load time."
  `(module-cell-meta (module-cell ,lang ,path) ,key))

(define-compiler-macro (setf module-meta) (value lang path key)
  `(setf (module-cell-meta (module-cell ,lang ,path) ,key) ,value))

(defmethods module-cell (self lock source lang module)
  (:method initialize-instance :after (self &key)
    ;; Give the lock a name.
    (setf lock (bt:make-lock (fmt "Lock for module ~a" self))))

  (:method print-object (self stream)
    (if *print-escape*
        (format stream "#.~s"
                (make-load-form self))
        (print-unreadable-object (self stream :type t)
          (format stream "~a (~a) (~:[not loaded~;loaded~])"
                  source
                  lang
                  module))))

  (:method module-ref (self name)
    (module-ref* module name))

  (:method module-exports (self)
    (module-exports module))

  (:method make-load-form (self &optional env)
    (declare (ignore env))
    `(module-cell ,lang
                  ,(assure absolute-pathname
                     source))))

(defun load-module-into-cell (cell)
  (lret ((module
          (assure (not null)
            (load-module (module-cell.lang cell)
                         (module-cell.source cell)))))
    (setf
     (module-cell.module cell) module
     (module-cell.timestamp cell) (now))))

(defvar *building-root* nil)
(declaim (type boolean *building-root*))

(defstruct root-target
  "The root target; it depends on everything but nothing depends on
it."
  (timestamp never :type target-timestamp))

(defmethods root-target (self)
  (:method print-object (self stream)
    (if *print-escape*
        (format stream "#.~s" '*root-target*)
        (print-unreadable-object (self stream :type t))))
  (:method fset:compare (self (obj t))
    (if (eq self obj) :equal :unequal))
  (:method fset:compare ((obj t) self)
    (if (eq self obj) :equal :unequal)))

(fset:define-cross-type-compare-methods root-target)

(defvar *root-target*
  (prog1 (make-root-target)
    (fmakunbound 'make-root-target))
  "The one and only root target.")
(declaim (type root-target *root-target*))

(define-symbol-macro root-target *root-target*)

(deftype target ()
  ;; NB Not allowing lists of targets as targets is a conscious
  ;; decision. It would make things much more complicated. In
  ;; particular, there would no longer be a single timestamp for a
  ;; target, because the proper timestamp to use for a list of targets
  ;; would depend on whether it was being depended on (in which case
  ;; we want the /newest/ timestamp) or doing the depending (in which
  ;; case we want the /oldest/ timestamp).
  '(or
    root-target
    bindable-symbol
    pathname
    package-ref
    directory-ref
    pattern-ref
    module-cell))

(defvar-unbound *target*
  "The target being built.")
(declaim (type target *target*))


;;; Targets.

(defconst reset-key 'reset)

(define-global-state *symbol-timestamps* (make-hash-table :size 1024))
(declaim (type hash-table *symbol-timestamps*))

(defun pathname-exists? (path)
  (etypecase-of (or string pathname) path
    (string (pathname-exists? (ensure-pathname path :want-pathname t)))
    (pathname
     (or (file-exists-p path)
         (directory-exists-p path)))))

(defun target-exists? (target)
  ;; (not (eql never (target-timestamp target)))
  (etypecase-of target target
    (root-target t)
    (bindable-symbol (boundp target))
    (pathname (pathname-exists? target))
    (package-ref
     (~> target
         ref.name
         string
         find-package))
    (directory-ref
     (~> target
         ref.name
         (resolve-target (base))
         directory-exists-p))
    (pattern-ref
     (~> target
         pattern-ref.output
         pathname-exists?))
    (module-cell (module-cell.module target))))

(defun target-timestamp (target)
  (etypecase-of target target
    (root-target (root-target-timestamp root-target))
    (bindable-symbol
     (if (boundp target)
         (let ((now (now)))
           (ensure2 (gethash target *symbol-timestamps*)
             now))
         never))
    (pathname
     (if (pathname-exists? target)
         (file-mtime target)
         never))
    (package-ref
     (let* ((name (ref.name target))
            (package (find-package (string name))))
       (if package far-future never)))
    (directory-ref
     (let ((dir (resolve-target (ref.name target) (base))))
       (if (directory-exists-p dir)
           far-future
           never)))
    (pattern-ref
     (with-accessors ((output pattern-ref.output)) target
       (if (pathname-exists? output)
           (file-mtime output)
           never)))
    (module-cell
     (with-slots (module timestamp) target
       (if (null module) never timestamp)))))

(defun (setf target-timestamp) (timestamp target)
  (check-type timestamp (or timestamp time-tuple))
  (check-not-frozen)
  (etypecase-of target target
    (root-target (setf (root-target-timestamp root-target) timestamp))
    (bindable-symbol
     ;; Configurations need to set the timestamp while unbound.
     #+ () (unless (boundp target)
             (error* "Trying to set timestamp for unbound symbol ~s"
                     target))
     (setf (gethash target *symbol-timestamps*) timestamp))
    (pathname
     (if (pathname-exists? target)
         ;; TODO There must be some portable way to do this.
         (error* "Cannot set pathname timestamps (yet).")
         (open target :direction :probe :if-does-not-exist :create)))
    (directory-ref
     (let ((dir (ref.name target)))
       (if (directory-exists-p dir)
           ;; TODO Ditto.
           (error* "Cannot set directory timestamps (yet).")
           (ensure-directories-exist dir))))
    ((or pattern-ref package-ref)
     ;; TODO Or does it?
     (error* "Setting the timestamp of ~s does not make sense."))
    (module-cell
     (setf (module-cell.timestamp target) timestamp))))

(defun touch-target (target &optional (date (now)))
  (setf (target-timestamp target) date))

(defun unbuild (target)
  "Destroy (\"unbuild\") TARGET.
E.g. delete a file, unbind a variable."
  (check-not-frozen)
  (etypecase-of target target
    (root-target)
    (bindable-symbol
     (makunbound target))
    (package-ref
     (delete-package (ref.name target)))
    (pathname
     (delete-file-or-directory (ref.name target)))
    (pattern-ref
     (delete-file-or-directory (pattern-ref.output target)))
    (directory-ref
     (delete-directory-tree (ref.name target)))
    (module-cell
     (with-slots (lang source) target
       (clear-module-cell lang source)
       (unbuild-lang-deps lang source)))))

(defun delete-file-or-directory (p)
  (if (directory-pathname-p p)
      (delete-directory-tree p)
      (delete-file-if-exists p)))

(defun resolve-target (target base)
  (setf base (pathname-directory-pathname base))
  (when (typep base 'temporary-file)
    (simple-style-warning "Base looks like a temporary file: ~a" base))
  (etypecase-of target target
    ((or root-target bindable-symbol package-ref) target)
    (pathname
     (let ((path (merge-pathnames* target base)))
       (if (wild-pathname-p path)
           (directory* path)
           path)))
    (pattern-ref
     (pattern-ref (pattern-ref.pattern target)
                  (merge-pathnames* (pattern-ref.input target) base)))
    (directory-ref
     (directory-ref
      ;; Could this be wild?
      (assure tame-pathname
        (merge-pathnames* (ref.name target) base))))
    (module-cell
     (with-slots (lang source) target
       (module-cell lang
                    (assure tame-pathname
                      (merge-pathnames* source base)))))))

(defparameter *preserve-fractional-seconds* nil
  "When comparing precise timestamps (local-time timestamps) and
  imprecise timestamps (universal times), should we conserve
  precision (convert the imprecise timestamp to a precise timestamp)
  or lose precision (convert the precise timestamp to an imprecise
  timestamp)?

  This matters at the REPL. It's perfectly possible to edit a file,
  build a target that depends on it, edit the file again, try to build
  it, and have nothing happen, because the file timestamp is not
  precise enough.

  Still, I'm not sure what the right thing to do here is -- which is
  why it is configurable. If I come to a definite decision, this
  parameter will be removed.")

(defun timestamp-newer? (ts1 ts2 &key (precise *preserve-fractional-seconds*))
  "Is TS1 greater than TS2?"
  ;; NB Note that conversion from timestamp to universal rounds down
  ;; (loses nsecs), so when comparing one of each, whether you convert
  ;; the universal time to a timestamp, or the timestamp to a
  ;; universal time, actually matters.
  (etypecase-of target-timestamp ts1
    (timestamp
     (etypecase-of target-timestamp ts2
       (timestamp (timestamp> ts1 ts2))
       (universal-time
        (if precise
            (timestamp> ts1 (universal-to-timestamp ts2))
            (> (timestamp-to-universal ts1) ts2)))
       (time-tuple
        ;; TODO Should we consider precision here?
        (timestamp> ts1 (time-tuple->timestamp ts2)))
       (never t)
       (far-future nil)))
    (universal-time
     (etypecase-of target-timestamp ts2
       (universal-time
        (> ts1
           ts2))
       (time-tuple
        (> ts1
           (time-tuple-universal-time ts2)))
       (timestamp
        (if precise
            (timestamp> (universal-to-timestamp ts1)
                        ts2)
            (> ts1
               (timestamp-to-universal ts2))))
       (never t)
       (far-future nil)))
    (time-tuple
     (etypecase-of target-timestamp ts2
       (universal-time
        (let ((u1 (time-tuple-universal-time ts1)))
          (or (> u1 ts2)
              (and (= u1 ts2)
                   (> (time-tuple-real-time ts1) 0)))))
       (time-tuple
        (let ((u1 (time-tuple-universal-time ts1))
              (u2 (time-tuple-universal-time ts2)))
          (or (> u1 u2)
              (and (= u1 u2)
                   (> (time-tuple-real-time ts1)
                      (time-tuple-real-time ts2))))))
       (timestamp
        (timestamp> (time-tuple->timestamp ts1)
                    ts2))
       (never t)
       (far-future nil)))
    (never nil)
    (far-future t)))

(defun target-timestamp= (ts1 ts2 &key (precise *preserve-fractional-seconds*))
  "Is TS1 greater than TS2?"
  ;; NB Note that conversion from timestamp to universal rounds down
  ;; (loses nsecs), so when comparing one of each, whether you convert
  ;; the universal time to a timestamp, or the timestamp to a
  ;; universal time, actually matters.
  (etypecase-of target-timestamp ts1
    (timestamp
     (etypecase-of target-timestamp ts2
       (timestamp (timestamp= ts1 ts2))
       (universal-time
        (if precise
            (timestamp= ts1 (universal-to-timestamp ts2))
            (= (timestamp-to-universal ts1) ts2)))
       (time-tuple
        ;; TODO Should we consider precision here?
        (timestamp= ts1 (time-tuple->timestamp ts2)))
       (never nil)
       (far-future nil)))
    (universal-time
     (etypecase-of target-timestamp ts2
       (universal-time
        (= ts1 ts2))
       (time-tuple
        (= ts1
           (time-tuple-universal-time ts2)))
       (timestamp
        (if precise
            (timestamp= (universal-to-timestamp ts1)
                        ts2)
            (= ts1
               (timestamp-to-universal ts2))))
       (never nil)
       (far-future nil)))
    (time-tuple
     (etypecase-of target-timestamp ts2
       (universal-time
        (let ((u1 (time-tuple-universal-time ts1)))
          (and (= u1 ts2)
               (if precise
                   (= (time-tuple-real-time ts1) 0)
                   t))))
       (time-tuple
        (let ((u1 (time-tuple-universal-time ts1))
              (u2 (time-tuple-universal-time ts2)))
          (and (= u1 u2)
               (if precise
                   (= (time-tuple-real-time ts1)
                      (time-tuple-real-time ts2))
                   t))))
       (timestamp
        (timestamp= (time-tuple->timestamp ts1) ts2))
       (never nil)
       (far-future nil)))
    (never
     (etypecase-of target-timestamp ts2
       (never t)
       (target-timestamp nil)))
    (far-future
     (etypecase-of target-timestamp ts2
       (far-future t)
       (target-timestamp nil)))))

(defun target-newer? (t1 t2)
  (timestamp-newer?
   (target-timestamp t1)
   (target-timestamp t2)))


;;; Target table abstract data type.

(deftype target-table ()
  'hash-table)

(defun target-type-of (x)
  (typecase-of target x
    (root-target 'root-target)
    (bindable-symbol 'bindable-symbol)
    (pathname 'pathname)
    (module-cell 'module-cell)
    (package-ref 'package-ref)
    (directory-ref 'directory-ref)
    (pattern-ref 'pattern-ref)
    ;; Bottom.
    (otherwise nil)))

(defun target= (x y)
  "Are two targets the same?"
  (or (eql x y)
      (and (compare #'type= #'target-type-of x y)
           (etypecase-of target x
             (root-target t)            ;There's only one.
             (bindable-symbol (eql x y))
             (pathname (pathname-equal x y))
             (module-cell (eql x y))
             (package-ref
              (compare #'string= #'ref.name x y))
             (directory-ref
              (compare #'pathname-equal #'ref.name x y))
             (pattern-ref
              (and (compare #'equal #'pattern-ref.input    x y)
                   (compare #'eql   #'pattern-ref.pattern  x y)
                   (compare #'equal #'pattern-ref.output   x y)))))))

(defun hash-target (target)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (compilation-speed 0)))
  (etypecase-of target target
    (root-target
     (load-time-value (sxhash *root-target*)))
    (bindable-symbol (sxhash target))
    (pathname (sxhash target))
    (module-cell
     (dx-sxhash
      (list 'module-cell
            (module-cell.lang target)
            (module-cell.source target))))
    (directory-ref
     (dx-sxhash
      (list 'directory-ref
            (ref.name target))))
    (package-ref
     (dx-sxhash
      (list 'package-ref
            (ref.name target))))
    (pattern-ref
     (dx-sxhash
      (list 'package-ref
            (ref.name target))))))

(define-custom-hash-table-constructor %make-target-table
  :test target=
  :hash-function hash-target)

(defun make-target-table (&rest args &key &allow-other-keys)
  (apply #'%make-target-table args))

(defun target-table-ref (table key)
  (with-custom-hash-table
    (gethash key table)))

(defun (setf target-table-ref) (value table key)
  (with-custom-hash-table
    (setf (gethash key table) value)))

(defun target-table-member (table key)
  (nth-value 1
    (target-table-ref table key)))

(defun (setf target-table-member) (value table key)
  (prog1 value
    (if value
        (setf (target-table-ref table key) nil)
        (with-custom-hash-table
          (remhash key table)))))

(defun target-table-keys (table)
  (collecting
    (with-custom-hash-table
      (maphash (lambda (k v) (declare (ignore v))
                 (collect k))
               table))))

(defun clear-target-table (table)
  (with-custom-hash-table
    (clrhash table)))


;;; Building.

(deftype freeze-policy ()
  '(member t nil :hard))

(defparameter *freeze-policy* t)
(declaim (type freeze-policy *freeze-policy*))

(defun freeze-policy ()
  "Get or set the current freeze policy.

The freeze policy determines what Overlord does when saving an image.

A freeze policy of `t' (the default) disables module loading, but can
be reversed with `overlord:unfreeze'.

A freeze policy of `nil` does nothing. This should only be used for
local development.

A freeze policy of `:hard' does the same thing as `t', but cannot be
reversed. This should be used when the image is intended to be
distributed."
  *freeze-policy*)

(defun (setf freeze-policy) (value)
  (setf *freeze-policy* (assure freeze-policy value)))

(defvar *frozen* nil
  "Is the build system frozen?")

(defun frozen? ()
  *frozen*)

(defparameter *freeze-fmakunbound-hit-list*
  '(unfreeze
    build
    unbuild
    run
    dynamic-require-as
    saved-stamp
    '(setf saved-stamp)))

(defun freeze ()
  ;; NB. You should be able to load an image and save it again.
  (unless (frozen?)
    (labels ((freeze ()
               (format t "~&Overlord: freezing image...~%")
               (build root-target)
               ;; The DB can still be reloaded, but is not in memory.
               (unload-db)
               (setf *frozen* t))
             (hard-freeze ()
               (freeze)
               (format t "~&Overlord: hard freeze...~%")
               (fmakunbound 'unfreeze)
               ;; Variables aren't defined yet.
               (clear-target-table (symbol-value '*all-targets*))
               (clrhash (symbol-value '*symbol-timestamps*))
               (clrhash (symbol-value '*tasks*))
               ;; The table of module cells needs special handling.
               (clear-module-cells)
               (clrhash (symbol-value '*claimed-module-names*))
               ;; The DB will not be reloaded.
               (deactivate-db)
               (dolist (fn *freeze-fmakunbound-hit-list*)
                 (fmakunbound fn))))
      (ecase-of freeze-policy *freeze-policy*
        ((nil))
        ((t) (freeze))
        (:hard (hard-freeze))))))

(uiop:register-image-dump-hook 'freeze)

(defun unfreeze ()
  (setf *frozen* nil))

(defun check-not-frozen ()
  (when *frozen*
    (restart-case
        (error* "The build system is frozen.")
      (unfreeze ()
        :report "Unfreeze the build system."
        (setf *frozen* nil)))))

(define-global-state *tasks* (dict))
(declaim (type hash-table *tasks*))

(define-global-state *all-targets* (make-target-table :size 8192))
(declaim (type target-table *all-targets*))

(defun list-all-targets ()
  (append (hash-table-keys *tasks*)
          (list-module-cells)
          (target-table-keys *all-targets*)))

(defstruct-read-only (task
                      (:constructor task (target init deps))
                      (:conc-name task.))
  (target :type target)
  (init :type function)
  (deps :type function))

(defun save-task (target thunk deps)
  (check-not-frozen)
  (etypecase-of target target
    (root-target)
    ((or bindable-symbol pathname)
     (let ((task (task target thunk deps)))
       (setf (gethash target *tasks*) task)))
    (module-cell)
    ((or directory-ref package-ref pattern-ref)
     (setf (target-table-member *all-targets* target) t)
     (values))))

(defun forget-target (target)
  (etypecase-of target target
    (root-target (error* "Cannot forget root target."))
    ((or bindable-symbol pathname)
     (unsave-stamp target)
     (remhash target *tasks*))
    ((or directory-ref package-ref pattern-ref module-cell)
     (unsave-stamp target)
     (setf (target-table-member *all-targets* target) nil))))

(defun find-saved-task (target)
  (check-not-frozen)
  (etypecase-of target target
    (root-target
     (task target
           (constantly nil)
           (lambda ()
             (let ((*building-root* t)
                   ;; `depends-on' needs `*base*' to be bound to something.
                   (*base*
                     (or (bound-value '*base*)
                         (user-homedir-pathname))))
               (depends-on-all (list-all-targets))))))
    ((or bindable-symbol pathname)
     (gethash target *tasks*))
    (directory-ref
     (let ((dir (ref.name target)))
       (task target
             (lambda ()
               (let ((dir (resolve-target dir (base))))
                 (ensure-directories-exist dir)))
             (constantly nil))))
    (package-ref
     (with-slots (name use-list nicknames) target
       (task target
             (lambda ()
               (or (find-package name)
                   (make-package name
                                 :use use-list
                                 :nicknames nicknames)))
             (constantly nil))))
    (pattern-ref
     (let* ((input (pattern-ref.input target))
            (output (pattern-ref.output target))
            (pattern (find-pattern (pattern-ref.pattern target))))
       (task output
             (lambda ()
               (let ((*input* input)
                     (*output* output))
                 (pattern-init pattern)))
             (lambda ()
               (let ((*input* input)
                     (*output* output))
                 (let ((*base* (pathname-directory-pathname input)))
                   (depends-on input))
                 (pattern-depend pattern))))))
    (module-cell
     (with-slots (lang source) target
       (task target
             (lambda ()
               (let ((*language* lang))
                 (load-module-into-cell target)))
             (lambda ()
               (let ((*base* (pathname-directory-pathname source))
                     (*language* lang))
                 ;; Depend on the source file.
                 (depends-on source)
                 ;; The stashed recursive dependencies of the module.
                 (depends-on-all (module-deps target))
                 ;; Let the language tell you what else to depend on.
                 (lang-deps lang source))))))))

(defcondition no-such-task (overlord-error)
  ((target :type target :initarg :target :reader overlord-error-target))
  (:report (lambda (c s)
             (format s "No such task: ~a"
                     (overlord-error-target c)))))

(defun no-such-task (target)
  (error 'no-such-task :target target))

(defcondition missing-file (no-such-task)
  ((target :type pathname))
  (:report (lambda (c s)
             (format s "~
Don't know how to build missing prerequisite ~s."
                     (overlord-error-target c)))))

(defun missing-file (target)
  (error 'missing-file :target target))

(defun target-task (target &optional (errorp t))
  (check-not-frozen)
  (or (find-saved-task target)
      (cond ((symbolp target)
             (if (boundp target)
                 (task target (constantly nil) (constantly nil))
                 (and errorp (no-such-task target))))
            ((pathnamep target)
             (if (pathname-exists? target)
                 (task target (constantly nil) (constantly nil))
                 (and errorp (missing-file target))))
            (errorp (no-such-task target))
            (t nil))))

(defun target-task-values (target &optional (errorp t))
  (let ((task (target-task target errorp)))
    (values (task.target task)
            (task.init task)
            (task.deps task))))

(define-global-state *always-rebuild* nil
  "Flag to always rebuild.
When this flag is set, targets are always rebuilt.

Possibly useful for testing.")

(defun build (&optional (target nil target-supplied?)
              &key (errorp t)
                   (force *always-rebuild*)
                   (message-handler *message-handler*))
  (check-not-frozen)
  (let ((*always-rebuild* force))
    (handler-bind ((overlord-message message-handler))
      (if target-supplied?
          (receive (target thunk deps)
              (target-task-values target errorp)
            (build-task target thunk deps))
          (build root-target)))))

(defun system-loaded? (system)
  (let ((system (asdf:find-system system nil)))
    (and system
         (asdf:component-loaded-p system)
         system)))

(defun run (target &optional system-name)
  "Entry point for scripts."
  (mvlet* ((target package-name
            (ematch target
              ((and target (type symbol)) target)
              ((list (and package-name (type string-designator))
                     (and symbol-name (type string-designator)))
               (values symbol-name package-name))))
           (system-name
            (string-downcase            ;What ASDF wants.
             (or system-name package-name))))
    (unless (system-loaded? system-name)
      (restart-case
          (asdf:load-system system-name)
        (quickload ()
          :test (lambda () (find-package :ql))
          :report (lambda (s)
                    (format s "Load ~a with Quicklisp instead."
                            system-name))
          (uiop:symbol-call :ql :quickload system-name))))
    (let ((package
            (or (find-package package-name)
                (error* "No such package: ~s" package-name))))
      (build (intern (string target) package))
      (values target system-name package))))

(defun build-task (target thunk deps)
  (check-type thunk function)
  (check-type deps function)
  (check-not-frozen)
  (save-task target thunk deps)
  (build-recursively target))

(defvar-unbound *already-built*
  "List of targets that have already been built.")
(declaim (type target-table *already-built*))

(defun already-built? (target)
  (target-table-member *already-built* target))

(defun print-target-being-built (target)
  "Print the target being built.
The idea here (borrowed from Apenwarr redo) is that the user should be
able to simply evaluate the form for a given target to pick up
building from there."
  (message "~Vt~s"
           (1- *depth*)
           `(build ,(dump-target/pretty target))))

(defun dump-target/pretty (target)
  "Return a form which, when evaluated, returns a target identical to
TARGET."
  (etypecase-of target target
    (pathname target)
    (bindable-symbol `(quote ,target))
    (root-target 'root-target)
    (module-cell
     (with-slots (lang source) target
       `(find-module ,lang ,source)))
    (directory-ref
     `(directory-ref ,(ref.name target)))
    (package-ref
     `(package-ref ,(ref.name target)
                   :nicknames ,(package-ref.nicknames target)
                   :use-list ,(package-ref.use-list target)))
    (pattern-ref
     `(pattern-ref ,(pattern-ref.pattern target)
                   ,(pattern-ref.input target)))))

(defun target-changed? (target)
  (let ((saved-stamp (saved-stamp target)))
    (cond ((null saved-stamp)            t)
          ((eql saved-stamp far-future)  t)
          ((eql saved-stamp never)       t)
          (t (not
              (stamp= (target-stamp target)
                      saved-stamp))))))

(defun stamp= (s1 s2)
  (etypecase-of stamp s1
    ((eql #.deleted) nil)
    (target-timestamp
     (etypecase-of stamp s2
       (target-timestamp (target-timestamp= s1 s2))
       (stamp nil)))
    (string
     (etypecase-of stamp s2
       (string (string= s1 s2))
       (stamp nil)))
    (file-meta
     (etypecase-of stamp s2
       (file-meta (file-meta= s1 s2))
       (stamp nil)))))

(defvar *custom-stamps*)

(defun custom-stamp (target)
  (if-let (table (bound-value '*custom-stamps*))
    (target-table-ref table target)
    nil))

(defun (setf custom-stamp) (stamp target)
  (assert (boundp '*custom-stamps*))
  (setf (target-table-ref *custom-stamps*
                          target)
        (assure string stamp)))

(defun target-stamp (target)
  (assure stamp
    (or (custom-stamp target)
        (etypecase-of target target
          ((or root-target
               bindable-symbol
               package-ref
               pattern-ref
               ;; TODO?
               directory-ref
               module-cell)
           (target-timestamp target))
          (pathname
           ;; TODO use stat instead of serapeum:file-size. (Or, use stat
           ;; in Serapeum?)
           (cond ((file-exists-p target)
                  (file-meta target))
                 ((directory-pathname-p target)
                  (target-timestamp target))
                 (t deleted)))))))

(defconst stamp-prop :stamp)

(defplace saved-stamp (target)
  (prop target stamp-prop))

(defun unsave-stamp (target)
  (delete-prop target stamp-prop))

(defun update-saved-stamp (target)
  (setf (saved-stamp target)
        (target-stamp target)))

(defun ensure-saved-stamp (target)
  (ensure2 (saved-stamp target)
    (target-stamp target)))

(defun build-recursively (target &aux (force *always-rebuild*))
  (check-not-frozen)
  (labels
      ((build-deps (deps)
         (let ((*deps*))
           (funcall deps)
           (reverse *deps*)))

       (never-been-built? (target)
         (not (target-exists? target)))

       (needs-building? (target deps)
         (let ((timestamp (target-timestamp target)))
           ;; Wait to check FORCE until after DEPS has been evaluated, in
           ;; case it has side effects.
           (or force
               ;; Checking if the target has *ever* been built also
               ;; has to wait until after DEPS has been evaluated.
               ;; Just because it's never been built doesn't mean it
               ;; has no dependencies.
               (or (eql timestamp never)
                   (some #'target-changed? deps)))))

       (rec (target)
         (assure stamp
           (if (already-built? target)
               (target-timestamp target)
               (progn
                 (setf (target-table-member *already-built* target) t)
                 (let ((*target* target))
                   (receive (target thunk deps-thunk)
                       (target-task-values target)
                     (let ((deps (build-deps deps-thunk)))
                       (flet ((rebuild ()
                                (let ((*depth* (1+ *depth*)))
                                  (print-target-being-built target)
                                  (funcall thunk)
                                  (update-saved-stamp target))))
                         (declare (dynamic-extent #'rebuild))
                         (cond ((never-been-built? target)
                                (rebuild))
                               ((needs-building? target deps)
                                (rebuild))
                               (t nil))
                         (ensure-saved-stamp target))))))))))
    (saving-database
      (handler-bind ((dependency #'redo))
        (let ((*already-built*
                (or (bound-value '*already-built*)
                    (make-target-table)))
              (*custom-stamps*
                (or (bound-value '*custom-stamps*)
                    (make-target-table))))
          (rec target))))))

(defun redo (&optional c)
  (when-let (r (find-restart 'redo c))
    (invoke-restart r)))

(defun rebuild-symbol (symbol thunk)
  (lambda ()
    (let ((*target* symbol))
      (setf (symbol-value symbol)     (funcall thunk)
            (target-timestamp symbol) (now)))))

(defcondition dependency ()
  ((target :initarg :target
           ;; Nothing can depend on the root target.
           :type (and target (not root-target))
           :reader overlord-error-target)))

(defun depends-on/1 (target)
  (let ((target (resolve-target target (base))))
    (restart-case
        (signal 'dependency :target target)
      (continue ()
        :report "Ignore the dependency and move on.")
      (save ()
        :report "Save the dependency, but don't build it."
        (push target *deps*))
      (redo ()
        :report "Build the dependency."
        (push target *deps*)
        (build target)))))

(defun depends-on (&rest deps)
  "Build DEPS in no particular order.
If any of DEPS is a list, its elements will also be added in no
particular order."
  (depends-on-all deps))

(defun depends-on-all (deps)
  ;; NB This is where you would add parallelism.
  (map nil #'depends-on/1 (reshuffle deps))
  (values))

(defun depends-on* (&rest deps)
  "Build DEPS in the order they are supplied.
If any of DEPS is a list, it will be descended into."
  (depends-on-all* deps))

(defun depends-on-all* (deps)
  (map nil #'depends-on/1 deps)
  (values))

(defun call/temp-file (dest fn)
  "Call FN on a freshly allocated temporary pathname; if it completes
safely, overwrite DEST with the contents of the temporary file."
  (let* ((ok nil)
         (tmp (with-temporary-file (:pathname p :keep t)
                (funcall fn p)
                (setq ok t)
                p)))
    (if ok
        ;; Cross-device?
        #+ () (rename-file-overwriting-target tmp dest)
        (copy-file tmp dest :if-to-exists :rename-and-delete)
        (delete-file tmp))))

(defun rebuild-file (file thunk &optional (base (base)))
  (lambda ()
    (let* ((file (resolve-target file base))
           (*target* file)
           (old (target-timestamp file)))
      (funcall thunk)
      ;; Since we depend on the granularity of the timestamps, all we can
      ;; be sure of is that is not older than the old timestamp.
      (assert (not (timestamp-newer? old (target-timestamp file)))))))

(defun save-file-task (file thunk deps)
  (check-type file pathname)
  (check-type thunk function)
  (check-type deps function)
  (save-task file
             (rebuild-file file thunk (base))
             deps))

(defun build-conf (name new test)
  "Initialize NAME, if it is not set, or reinitialize it, if the old
value and NEW do not match under TEST."
  (let ((old (symbol-value name)))
    (if (funcall test old new)
        old
        (progn
          (simple-style-warning "Redefining configuration ~s" name)
          (funcall (rebuild-symbol name (lambda () new)))))))

;
;;; Keyword macros

(defun path (path)
  (~> path
      (ensure-pathname :want-pathname t)
      (merge-pathnames (base))))

(defun file (file)
  (assure file-pathname (path file)))

(defmacro with-keyword-macros (&body body)
  `(macrolet ((:depends-on (x &rest xs)
                `(depends-on ,x ,@xs))
              (:depends-on* (x &rest xs)
                `(depends-on* ,x ,@xs))
              (:depends-on-all (xs)
                `(depends-on-all ,xs))
              (:depends-on-all* (xs)
                `(depends-on-all* ,xs))
              (:path (path)
                (assure pathname
                  (path path)))
              (:file (file)
                `(:path ,file))
              (:package-exists (name &key use nicknames)
                `(package-ref ,name :use ,use :nicknames ,nicknames))
              (:directory-exists (name)
                `(directory-ref ,name))
              (:pattern (name input)
                `(pattern-ref ,name ,input))
              (:module (lang source)
                `(module-cell ',lang ,source))
              (:extension (ext)
                `(extension ,ext))
              (:run (cmd &rest args)
                `(run-cmd ,cmd ,@args))
              (:cmd (&rest args)
                `(cmd ,@args))
              (:message (control-string &rest args)
                `(message ,control-string ,@args))
              (:basename (file)
                `(basename ,file))
              (:stamp (stamp)
                `(setf (custom-stamp *target*) ,stamp)))
     ,@body))

(defun basename (file)
  (enough-pathname file (pathname-directory-pathname file)))

(defun run-cmd (cmd &rest args)
  "Like `uiop:run-program, but defaulting the `:directory' argument to
the current base."
  (multiple-value-call #'uiop:run-program
    cmd
    (values-list args)
    :directory (base)))


;;; Bindings.

(defun save-base (form)
  `(let ((*base* ,(base)))
     ,form))

(defmacro defconfig (name init &key (test '#'equal)
                                    documentation)
  (let ((init
          (save-base
            `(with-defaults-from-base
               (with-keyword-macros
                 ,init)))))
    `(progn
       (eval-always
         (define-global-var ,name
             (prog1 ,init
               (touch-target ',name
                             ,(if (boundp name)
                                  (assure target-timestamp
                                    (target-timestamp name))
                                  (now))))
           ,@(unsplice documentation)))
       (eval-always
         (save-task ',name (constantly ,init) (constantly nil)))
       (eval-always
         (build-conf ',name ,init ,test))
       ',name)))

(defmacro deps-thunk (&body body)
  `(lambda ()
     ,(save-base
        `(with-defaults-from-base
           (with-keyword-macros
             ,@body)))))

(defmacro init-thunk (&body body)
  `(lambda ()
     ,(save-base
        `(with-defaults-from-base
           (with-keyword-macros
             ,@body)))))

(defmacro define-script (name expr)
  `(defconfig ,name ',expr
     :test #'source=))

(defmacro with-script-dependency ((name expr deps) &body body)
  (with-gensyms (sn)
    `(let* ((,sn (script-name ,name))
            (,deps (cons (list :depends-on (list 'quote ,sn)) ,deps)))
       (list
        'progn
        (list 'define-script ,sn ,expr)
        ,@body))))

(defun script-name (name)
  (intern (coerce-case (fmt "~a.do" name))
          (symbol-package name)))

(defun source= (x y)
  ;; TODO How to test equality in the presence of macros?
  ;; Maybe expand with a code walker?
  (similar? x y))

(defmacro undefine-target (name &body body)
  (declare (ignore body))
  `(forget-target ',name))

(defmacro var-target (name expr &body deps)
  (with-script-dependency (name expr deps)
    `(progn
       (defvar ,name)
       (save-task ',name
                  (rebuild-symbol ',name
                                  (init-thunk ,expr))
                  (deps-thunk ,@deps))
       ',name)))

(defmacro defvar/deps (name expr &body deps)
  "Define a variable with dependencies.
A dependency can be a file or another variable.

If any of those files or variables change, then the variable is
rebuilt."
  `(progn
     (var-target ,name ,expr
       ,@deps)
     (build ',name)
     ',name))

(defmacro defconfig/deps (name expr &body deps)
  "Define a conf with dependencies.
A dependency can be a file or another variable.

If any of those files or variables change, then the variable is
rebuilt."
  `(progn
     ;; The script must be available at compile time to be depended
     ;; on.
     (define-script ,(script-name name) ,expr)
     (defconfig/deps-aux ,name ,expr
       ,@deps)))

(defmacro defconfig/deps-aux (name expr &body deps)
  (mvlet* ((base (base))
           (*base* base)
           (deps
            `((:depends-on ',(script-name name))
              ,@deps))
           (init timestamp
            (progn
              (if (boundp name)
                  (build name)
                  (let ((deps-thunk (eval* `(deps-thunk ,@deps)))
                        (init-thunk (rebuild-symbol name (eval* `(init-thunk ,expr)))))
                    (build-task name init-thunk deps-thunk)))
              (values (symbol-value name)
                      (target-timestamp name)))))
    `(progn
       (eval-always
         (define-global-var ,name
             (prog1 ',init
               (setf (target-timestamp ',name) ,timestamp))))
       (eval-always
         (build-task ',name
                     (rebuild-symbol ',name (init-thunk ,expr))
                     (deps-thunk ,@deps)))
       ',name)))

(defmacro file-target (name pathname (&optional tmp) &body (init . deps))
  "If TMP is null, no temp file is used."
  (ensure-pathnamef pathname)
  (check-type pathname tame-pathname)
  (let* ((base (base))
         (pathname (resolve-target pathname base))
         (dir (pathname-directory-pathname pathname))
         (init
           `(progn
              (setf (current-dir!) ,dir)
              ,init)))
    (with-script-dependency (name init deps)
      `(progn
         ;; Make the task accessible by name.
         (def ,name ,pathname)
         (with-defaults-from-base
           (save-file-task ,pathname
                           (init-thunk
                             ,(if (null tmp)
                                  ;; No temp file needed.
                                  init
                                  ;; Write to a temp file and rename.
                                  `(call/temp-file ,pathname
                                                   (lambda (,tmp)
                                                     ,init)))
                             (assert (file-exists-p ,pathname)))
                           (deps-thunk
                             (setf (current-dir!) ,dir)
                             ,@deps)))
         ',pathname))))


;;;; Phony targets.

(defmacro deftask (name &body deps)
  "Define a task -- a target that only has dependencies.
This is essentially a convenience to let you use keyword macros to
specify the dependencies you want on build."
  `(progn
     (save-task ',name
                (constantly nil)
                (lambda ()
                  ;; Phony targets don't *need* to be built.
                  (unless *building-root*
                    (funcall (deps-thunk ,@deps)))))
     ',name))


;;;; File patterns.

;;; A pattern is an abstract relationship between two files.

(defclass pattern ()
  ((input-defaults
    :initarg :input-defaults
    :type pathname
    :reader pattern.input-defaults)
   (output-defaults
    :initarg :output-defaults
    :type pathname
    :reader pattern.output-defaults)
   (init-fn
    :initarg :init
    :type function
    :reader pattern.init-fn)
   (deps-fn
    :initarg :deps
    :type function
    :reader pattern.deps-fn))
  (:default-initargs
   :input-defaults *nil-pathname*
   :output-defaults *nil-pathname*
   :deps (constantly nil)
   :init (constantly nil)))

(defmethods pattern (self init-fn deps-fn)
  (:method pattern-init (self)
    (funcall init-fn))
  (:method pattern-depend (self)
    (funcall deps-fn)))

(defun extension (ext)
  (assure pathname
    (etypecase-of (or null string) ext
      (null *nil-pathname*)
      (string (make-pathname :type ext)))))

(defun find-pattern (pattern &optional (errorp t))
  (assure pattern
    (etypecase-of (or symbol pattern) pattern
      (pattern pattern)
      (symbol
       (cond ((subtypep pattern 'pattern)
              (make pattern))
             (errorp (error* "No such pattern: ~s" pattern))
             (t nil))))))

(defmacro defpattern (class-name (in out)
                      (&rest options &key &allow-other-keys)
                      &body (init . deps))
  "Define a file pattern named NAME.

Some build systems let you define file patterns based on extensions or
regular expressions. That won't work for Overlord, because there is no
special namespace for targets, so such a rule would apply everywhere.
It has to have a name.

A file pattern in Overlord must have a name. You use it like this:

    (:depend-on (:pattern 'my-pattern \"file\"))

If you set the input pathname defaults, you don't have to give an
extension to the file.

Based on the pattern, the output file is calculated, and the result
depends on that."
  (let ((script-name (script-name class-name)))
    `(progn
       (define-script ,script-name ,(list* in out init))
       (eval-always
         (with-keyword-macros
           (defclass ,class-name (pattern)
             ()
             (:default-initargs ,@options))))
       (defmethod pattern-init ((self ,class-name))
         (let ((,in *input*)
               (,out *output*))
           (declare (ignorable ,in ,out))
           ,init))
       (defmethod pattern-depends ((self ,class-name))
         (let ((,in *input*)
               (,out *output*))
           (declare (ignorable ,out))
           (funcall
            (deps-thunk
              (:depends-on ',script-name)
              (:depends-on ,in)
              ,@deps)))))))


;;; Languages

;;; Note that support for languages follows support for file patterns.
;;; A pattern is an abstract relationship between two files; a
;;; language is an abstract relationship between a file and Lisp
;;; binding.

(defun delete-versioned-fasls (&optional (version *fasl-version*))
  (uiop:delete-directory-tree
   (fasl-version-dir version)
   :validate (op (subpathp _ (xdg-cache-home)))))

(defun fasl-version-dir (&optional (version *fasl-version*))
  (check-type version fasl-version)
  (ensure-directory-pathname
   (xdg-cache-home "overlord" (fmt "v~a" version))))

;;; Module dependencies: when compiling module X requires module Y,
;;; save the information that X depends on Y so Y can be checked when
;;; determining if X needs rebuilding.

(defvar-unbound *module-chain*
  "The chain of modules being loaded.")

(defcondition module-dependency ()
  ((module-cell
    :initarg :module-cell
    :reader module-dependency.module-cell)))

(defconst module-deps-prop :module-deps)

(defun module-deps (m)
  (check-type m module-cell)
  (mapply #'module-cell (prop m module-deps-prop)))

(defun (setf module-deps) (deps m)
  (check-type m module-cell)
  (check-type deps list)
  (let ((deps-table (mapcar (juxt #'module-cell.lang #'module-cell.source) deps)))
    (setf (prop m module-deps-prop) deps-table)))

(defun flatten-module-deps (m)
  (collecting
    (labels ((rec (m)
               (collect m)
               (dolist (d (module-deps m))
                 (rec d))))
      (rec m))))

(defmacro with-module-dependency-tracking ((&key) &body body)
  "Sugar for `call/module-dependency-tracking'."
  (with-thunk (body)
    `(call/module-dependency-tracking ,body)))

(defun call/module-dependency-tracking (thunk)
  "Ensure that `*module-chain*' is bound around THUNK."
  (fbind (thunk)
    (if (boundp '*module-chain*)
        (thunk)
        (let ((*module-chain* '()))
          (handler-bind ((module-dependency
                           (lambda (c)
                             (let ((mc (module-dependency.module-cell c)))
                               (when-let (prev (first *module-chain*))
                                 (pushnew mc (module-deps prev)))
                               (push mc *module-chain*)))))
            (thunk))))))

(defun save-module-dependency (mc)
  (check-type mc module-cell)
  (signal 'module-dependency :module-cell mc))

(defun %require-as (lang source *base* &rest args)
  (ensure-pathnamef source)
  (with-defaults-from-base
    (apply #'dynamic-require-as
           lang
           (merge-pathnames* source *base*)
           args)))

(defun dynamic-require-as (lang source &key force)
  (check-type source (and absolute-pathname file-pathname))
  (setf lang (or lang (guess-lang+pos source)))
  (when force
    (dynamic-unrequire-as lang source))
  (with-module-dependency-tracking ()
    (let ((mc (module-cell lang source)))
      (save-module-dependency mc)
      (build mc)
      (module-cell.module mc))))

(defun %unrequire-as (lang source *base*)
  (with-defaults-from-base
    (dynamic-unrequire-as lang
                          (merge-pathnames* source *base*))))

(defun dynamic-unrequire-as (lang source)
  (check-type source (and absolute-pathname file-pathname))
  (clear-module-cell lang source)
  (values))

(defmacro require-as (lang source)
  `(%require-as ,lang ,source ,(base)))

(defun require-for-emacs (lang source)
  "Like `dynamic-require-as', but with looser restrictions for easy
interoperation with Emacs."
  (let ((lang
          (resolve-lang lang))
        (source
          (assure absolute-pathname
            (ensure-pathname source))))
    (dynamic-require-as lang source)
    (values)))

(defmacro unrequire-as (lang source)
  `(%unrequire-as ,lang ,source ,(base)))

(defun escape-lang-name (lang-name)
  (check-type lang-name lang-name)
  (url-encode (string lang-name) :encoding :utf-8))

(defun lang-fasl-dir (lang current-dir)
  (let ((version-string (fmt "v~a" *fasl-version*))
        (lang-string (escape-lang-name lang))
        (suffix
          (make-pathname :directory
                         (cons :relative
                               (drop-while #'keywordp
                                           (pathname-directory current-dir))))))
    (xdg-cache-home "overlord"
                    version-string
                    :implementation
                    "fasls"
                    lang-string
                    suffix)))

(defun faslize (lang pathname)
  (etypecase-of lang lang
    (package (faslize (package-name-keyword lang) pathname))
    (lang-name
     (merge-pathnames*
      (make-pathname :name (pathname-name pathname)
                     :type fasl-ext)
      (lang-fasl-dir lang pathname)))))

(defun fasl? (pathname)
  (equal (pathname-type pathname)
         fasl-ext))

(defun load-module (lang source)
  (ensure-pathnamef source)
  (let ((*base* (pathname-directory-pathname source)))
    (with-defaults-from-base
      (load-fasl-lang lang source))))

(defun module-static-exports (lang source)
  (ensure-static-exports lang source)
  (assure list
    (snarf-static-exports lang source)))

(defun ensure-static-exports (lang source)
  (let ((*base* (pathname-directory-pathname source)))
    (~> lang
        (static-exports-pattern source)
        (pattern-ref source)
        build)))

(defun extract-static-exports (lang source)
  (check-type source absolute-pathname)
  (let ((lang (resolve-lang-package lang)))
    (if-let (sym (find-external-symbol 'static-exports lang))
      (funcall sym source)
      (module-exports (dynamic-require-as lang source)))))

(defclass static-exports-pattern (pattern)
  ((lang :initarg :lang)
   (source :initarg :source)))

(defmethods static-exports-pattern (self lang source)
  (:method pattern.output-defaults (self)
    ;; Bear in mind *input* may have been resolved.
    (let ((fasl (faslize lang source))
          (ext (extension "static-exports")))
      (merge-pathnames* ext fasl)))

  (:method pattern-init (self)
    (save-static-exports lang source))

  (:method pattern-depend (self)
    (depends-on (fasl-lang-pattern-ref lang source))))

(defun static-exports-pattern (lang source)
  ;; The static export file depends on the fasl.
  (make 'static-exports-pattern :lang lang :source source))

(defun snarf-static-exports (lang source)
  (let ((file (static-exports-file lang source)))
    (assert (file-exists-p file))
    (read-file-form file)))

(defun save-static-exports (lang source)
  (let ((exports (extract-static-exports lang source))
        (file (static-exports-file lang source)))
    (write-form-as-file exports file)))

(def static-exports-extension (extension "static-exports"))

(defun static-exports-file (lang source)
  (merge-pathnames*
   static-exports-extension
   (faslize lang source)))

(defun module-dynamic-exports (lang source)
  (module-exports (dynamic-require-as lang source)))

;;; Module cells.

;;; The idea here is to avoid runtime lookups of modules by interning
;;; mutable cells instead.

(define-global-state *module-cells* (dict))

(defun list-module-cells ()
  (hash-table-values *module-cells*))

(defun clear-module-cells ()
  "Delete information not needed at runtime from module cells."
  ;; We don't actually clear the table because there may be cases
  ;; where expansion of compiler macros has been suppressed by
  ;; optimization settings and there is no reference to the module
  ;; cell.
  (maphash (lambda (k mc) (declare (ignore k))
             (with-slots (source timestamp) mc
               (setf source *nil-pathname*
                     timestamp never)))
           *module-cells*))

(defun %ensure-module-cell (lang path)
  "Get the module cell for LANG and PATH, creating and interning one
if it does not exist."
  (check-type path absolute-pathname)
  (setf path
        (assure pathname
          (or (truename* path)
              (or (progn
                    (build path)
                    (truename* path))
                  (error "Cannot resolve pathname ~a" path)))))
  (mvlet* ((key (cons lang path))
           (cell cell?
            (gethash key *module-cells*)))
    (if cell? (assure module-cell cell)
        (let ((cell (make 'module-cell :lang lang :source path)))
          (setf (gethash key *module-cells*) cell)))))

(defun module-cell (lang path)
  (let ((path
          (assure absolute-pathname
            (merge-input-defaults lang (ensure-pathname path :want-pathname t))))
        (lang (lang-name lang)))
    (%ensure-module-cell lang path)))

(defun find-module (lang source)
  (module-cell.module (module-cell lang source)))

(define-compiler-macro find-module (lang source)
  `(module-cell.module (module-cell ,lang ,source)))

;;; Hack to stop recursion. We can't use %ensure-module-cell directly,
;;; because it doesn't apply the defaults for the language.
(declaim (notinline %module-cell))
(defun %module-cell (lang path)
  (declare (notinline module-cell))
  (module-cell lang path))

(defun clear-module-cell (lang source)
  (declare (notinline module-cell))
  (lret ((m (module-cell lang source)))
    (with-slots (timestamp module) m
      (setf timestamp never)
      (nix module))))

;;; Lazy-loading modules.

(defun load-module/lazy (lang source)
  (load-module-into-cell/lazy
   (module-cell lang source)))

(define-compiler-macro load-module/lazy (lang path)
  `(load-module-into-cell/lazy (module-cell ,lang ,path)))

(defun load-module-into-cell/lazy (cell)
  (with-slots (module lang source) cell
    ;; "Double-checked locking."
    (or module
        (synchronized (cell)
          (or module
              (progn
                (build cell)
                module))))))

;;; Languages.

;;; This is a generic function so individual langs can define their
;;; own dependencies in :after methods.

;;; TODO Should this use the progn method combination?
(defgeneric lang-deps (lang source)
  (:method ((lang t) (source t))
    nil)

  (:method ((lang symbol) (source t))
    (lang-deps (resolve-lang-package lang) source)))

(defgeneric unbuild-lang-deps (lang source)
  (:method ((lang t) (source t))
    nil)

  (:method ((lang symbol) (source t))
    (unbuild-lang-deps (resolve-lang lang) source)))

(defmethod pattern.input-defaults ((lang symbol))
  (let ((p (resolve-package lang)))
    (if p (pattern.input-defaults p) *nil-pathname*)))

(defmethod pattern.input-defaults ((p package))
  (let ((sym (find-symbol #.(string 'extension) p)))
    (or (and sym (symbol-value sym))
        *nil-pathname*)))

(defmacro define-loader-language (package-name (source) &body (reader &key extension))
  (let* ((pn (string package-name)))
    ;; Sanity check: are we overwriting an existing package?
    (when-let (package (find-package pn))
      (when (package-use-list package)
        (error* "Package already exists with a use list: ~a" package))
      (unless (set-equal (package-exports package)
                         (loader-language-exports)
                         :test #'string=)
        (error* "Package already exists with wrong exports: ~a" package)))
    `(progn
       (defpackage ,pn
         (:use)
         (:export ,@(loader-language-exports)))
       (define-loader-language-1 ,pn (,source)
         ,reader
         :extension ,extension))))

(defmacro define-loader-language-1 (package-name (source) &body (reader &key extension))
  "The part that gets expanded once PACKAGE-NAME exists."
  (let* ((reader (hygienic-pathnames reader))
         (p (find-package package-name))
         (syms (mapcar (op (find-symbol (string _) p))
                       (loader-language-exports))))
    (destructuring-bind (load read ext script) syms
      `(progn
         (declaim (notinline ,load ,read))
         (eval-always
           (define-script ,script ,reader)
           (defparameter ,ext (extension ,extension))
           (defun ,load (,source)
             ,reader)
           (defun ,read (,source _stream)
             (declare (ignore _stream))
             (list ',load ,source))
           (defmethod lang-deps :after ((self (eql ,(make-keyword package-name))) source)
             (declare (ignore source))
             (depends-on ',script)))))))

(defun load-fasl-lang (lang source)
  (let ((object-file (faslize lang source)))
    (restart-case
        (load-as-module object-file)
      (recompile-object-file ()
        :report "Recompile the object file."
        (delete-file-if-exists object-file)
        (build (module-cell lang source))
        (load-fasl-lang lang source)))))

(defmethod lang-deps ((lang package) (source cl:pathname))
  (let* ((pat (fasl-lang-pattern lang source))
         (ref (pattern-ref pat source)))
    (depends-on ref)))

(defmethod unbuild-lang-deps ((lang package) (source cl:pathname))
  (delete-file-if-exists (faslize lang source)))

(defun lang-name (lang)
  (assure lang-name
    (etypecase-of (or keyword lang-name package) lang
      (keyword lang)
      (lang-name (make-keyword lang))
      (package (package-name-keyword lang)))))

(defclass fasl-lang-pattern (pattern)
  ((lang :initarg :lang)
   (source :initarg :source)))

(defmethods fasl-lang-pattern (self lang source)
  (:method pattern.output-defaults (self)
    (faslize lang source))

  (:method pattern-init (self)
    ;; TODO Should we reset the deps first?
    (let* ((*source* *input*)
           (lang (lang-name lang))
           (*language* lang)
           ;; Must be bound here for macros that intern
           ;; symbols.
           (*package* (user-package (resolve-package lang))))
      (compile-to-file
       (wrap-current-module
        (expand-module lang *input*)
        lang *input*)
       (ensure-directories-exist *output*)
       :top-level (package-compile-top-level? lang)
       :source *source*)))

  (:method pattern-depend (self)
    (depends-on-all
     (module-static-dependencies lang *input*))))

(defun fasl-lang-pattern (lang source)
  (make 'fasl-lang-pattern :lang lang :source source))

(defun fasl-lang-pattern-ref (lang source)
  (pattern-ref (fasl-lang-pattern lang source) source))

(defun module-static-dependencies (lang source)
  (module-deps (module-cell lang source)))

(defmacro with-input-from-source ((stream source) &body body)
  "Read from SOURCE, skipping any #lang declaration."
  `(with-input-from-file (,stream ,source :element-type 'character)
     (skip-hash-lang ,stream)
     ,@body))

(def reader-string (string 'read-module))

(def module-string (string 'module-progn))

(def compile-top-level-string (string '*compile-top-level*))

(def loader-language-exports
  (list (string 'load)
        reader-string
        (string 'extension)
        (string 'script)))

;;; Make it a function so it can be used before defined.
(defun loader-language-exports ()
  loader-language-exports)

(defmacro module-progn-in (package &body body &environment env)
  "Resolve a package's expander at macro-expansion time.
Also, ensure that PACKAGE is the current package when BODY is
macro-expanded.

If PACKAGE does not export an expander, `progn' is used instead."
  ;; Is expanding the macro this way useful?
  (let* ((package-expander (package-expander package :errorp nil))
         (module-progn (or package-expander 'progn))
         (form `(,module-progn ,@body)))
    (expand-in-package form package env)))

(defun suffix-package (package suffix)
  "Like `resolve-package' but, if a package exists with the same name,
but ending in SUFFIX, and inheriting from that package, return that
instead."
  (assert (string^= "-" suffix))
  (assure package
    (with-absolute-package-names ()
      (when-let (base-package (resolve-package package))
        (let* ((user-package-name
                 (concatenate 'string
                              (package-name base-package)
                              suffix))
               (user-package (find-package user-package-name)))
          (or (and user-package
                   (find base-package (package-use-list user-package))
                   user-package)
              base-package))))))

(defun user-package (package)
  "Like `resolve-package' but, if a package exists with the same name,
but ending in `-user', and inheriting from that package, return that
instead."
  (suffix-package package "-USER"))

;;; TODO Is this useful?
(defun expand-in-package (form package env)
  (let ((*package* (user-package (resolve-package package))))
    (macroexpand-1 form env)))

(defun cl-read-module (source stream)
  (declare (ignore source))
  (let ((eof "eof"))
    `(progn
       ,@(loop for form = (read stream nil eof)
               until (eq form eof)
               collect form))))

(defun package-compile-top-level? (package)
  (and-let* ((sym (find-symbol compile-top-level-string package))
             ((boundp sym)))
    (symbol-value sym)))

(defun package-reader (package &key (errorp t))
  "Resolve the reader exported by PACKAGE."
  (flet ((error* (&rest args)
           (if errorp
               (apply #'error* args)
               (return-from package-reader nil))))
    (assure (or symbol null)
      (let ((p (resolve-package package)))
        (if (eql p (find-package :cl))
            'cl-read-module
            (receive (sym status) (find-symbol reader-string p)
              (cond ((no sym)
                     ;; There is no symbol.
                     (error* "No reader defined in package ~a" p))
                    ((not (eql status :external))
                     ;; There is a symbol, but it's not external.
                     (error* "Package ~a does not export a reader" p))
                    ((not (fboundp sym))
                     ;; There is an external symbol, but it's not
                     ;; fbound.
                     (error* "No binding for reader in package ~a" p))
                    (t
                     (unless (eql (symbol-package sym) p)
                       (simple-style-warning "Package reader ~a in ~a is inherited from ~a."
                                             sym p (symbol-package sym)))
                     sym))))))))

(defun reintern (s &aux (p *package*))
  (let ((s (string s)))
    (or (find-symbol s p)
        (error "No symbol named ~a in ~s" s p))))

(defmacro reinterning ((&rest names) &body body)
  `(let ,(loop for name in names
               collect `(,name (reintern ',name)))
     ,@body))

(defun package-expander (package &key (errorp t))
  "Resolve the expander exported by PACKAGE."
  (flet ((error* (&rest args)
           (if errorp
               (apply #'error* args)
               (return-from package-expander nil))))
    (assure (or symbol null)
      (let ((p (resolve-package package)))
        (receive (sym status) (find-symbol module-string p)
          (cond ((no sym)
                 (error* "No expander defined in package ~a" p))
                ((not (eql status :external))
                 (error* "Package ~a does not export an expander" p))
                ((not (fboundp sym))
                 (error* "Expander in package ~a is exported but unbound" p))
                ((not (macro-function sym))
                 (error* "Package ~a exports an expander that is not a macro" p))
                (t
                 (unless (eql (symbol-package sym) p)
                   (simple-style-warning "Package expander ~a in ~a is inherited from ~a."
                                         sym p (symbol-package sym)))
                 sym)))))))

(defparameter *file-local-variables*
  '(*package* *readtable*
    ;; These seem like a good idea to me.
    *read-base* *read-default-float-format*
    *file-local-variables*)
  "Variables that should be given fresh rebindings while reading in a
  module.

This should be a superset of the variables bound by CL during calls to
`cl:load'.")

(defun expand-module (package source
                      &aux (file-locals *file-local-variables*))
  (let* ((package (resolve-package package))
         (*language* (lang-name package))
         (source (ensure-pathname source :want-pathname t))
         (*source* source))
    (with-input-from-source (in source)
      (progv file-locals (mapcar #'symbol-value file-locals)
        (let* ((reader (package-reader package))
               (module-form
                 (let ((*package* (user-package package)))
                   (funcall reader source in))))
          module-form)))))

(defun expand-module-for-emacs (lang source)
  (setf lang (resolve-lang lang))
  (values (expand-module lang source)))

(defmacro with-current-module ((lang source) &body body)
  `(macrolet ((current-module-lang () ',lang)
              (current-module-source () ',source)
              (current-module-cell ()
                `(module-cell ',',lang ,',source))
              (current-module ()
                `(find-module ',',lang ,',source))
              (current-module-meta (key)
                `(module-meta ',',lang ,',source ,key)))
     ,@body))

(defun wrap-current-module (form package source)
  (let ((lang (lang-name package)))
    `(with-current-module (,lang ,source)
       ,form)))


;;; #lang syntax.

(defcondition no-such-lang (overlord-error)
  ((lang :initarg :lang :type string-designator
         :reader no-such-lang.lang))
  (:report (lambda (c s)
             (with-slots (lang) c
               (format s "No such language as ~a" lang)))))

(defun load-same-name-system (c)
  (declare (ignore c))
  (invoke-restart 'load-same-name-system))

(defgeneric find-asdf-system (lang)
  (:method ((lang no-such-lang))
    (find-asdf-system (no-such-lang.lang lang)))
  (:method ((lang t))
    (and (not (frozen?))
         (let ((lang (string-downcase lang)))
           (asdf:find-system lang nil)))))

(defun ensure-lang-exists (lang &optional (cont #'identity))
  (check-type lang package-designator)
  (check-type cont function)
  (if (packagep lang) lang
      (let ((pkg (resolve-package lang)))
        (or (and pkg (package-name-keyword pkg))
            (restart-case
                (error 'no-such-lang :lang lang)
              (load-same-name-system ()
                :test find-asdf-system
                :report (lambda (s)
                          (format s "Load the system named ~a and try again" lang))
                (asdf:load-system lang)
                (funcall cont lang)))))))

(defun lookup-hash-lang (name)
  (assure (or null lang-name)
    (let* ((pkg-name (assure (satisfies valid-lang-name?)
                       ;; Set the case as if the string were being
                       ;; read, without using `read`.
                       (coerce-case name))))
      (ensure-lang-exists pkg-name #'lookup-hash-lang))))

(defun guess-lang+pos (file)
  "If FILE has a #lang line, return the lang and the position at which
the #lang declaration ends."
  (receive (lang pos)
      (file-hash-lang file)
    (if (stringp lang)
        (values (lookup-hash-lang lang) pos)
        (values nil 0))))

(defun resolve-lang (lang)
  (assure lang-name
    (etypecase-of (or lang-name string) lang
      (string (lookup-hash-lang lang))
      (lang-name lang))))

(defun resolve-lang-package (lang)
  (assure package
    (resolve-package (resolve-lang lang))))

(defmacro with-meta-language ((path stream) &body body)
  (with-thunk (body path stream)
    `(call/meta-language ,body ,path ,stream)))

(defun call/meta-language (fn path stream)
  (let* ((next-lang (read-lang-name stream))
         (package (resolve-lang-package next-lang))
         (user-package (user-package package))
         (*package* user-package)
         (forms (funcall fn path stream)))
    `(module-progn-in ,(package-name-keyword package)
                      ,@forms)))


;;; Imports.

;;; It might seem like this could be moved into a separate file, but
;;; that would requiring exposing too much of the above.

;;; Note that the import macros defined here expand into definition
;;; forms from overlord/shadows rather than from cl proper. (E.g.
;;; `overlord/shadows:defun' rather than `cl:defun'.) This is so
;;; languages that need to handle imports specially (e.g. Core Lisp)
;;; can do so simply by shadowing the relevant definition forms with
;;; `macrolet', instead of having to re-implement everything.

;;; Hopefully most, if not all, of this code will be replaced once we
;;; have a full implementation of import sets.

(define-global-state *always-import-values* nil
  "Flag to control importing behavior.
When this is T, imports should always be values, never bindings.

This is intended to be used when saving an image, where you don't care
about ease of development or debugging, only speed.")
(declaim (type boolean *always-import-values*))

(defcondition bad-macro-import (overlord-error)
  ((name :initarg :name :type symbol
         :documentation "The name of the macro."))
  (:documentation "Invalid attempt to import something as a macro."))

(defcondition module-as-macro (bad-macro-import)
  ()
  (:documentation "Attempt to import a module as a macro.")
  (:report (lambda (c s)
             (with-slots (name) c
               (format s "Cannot import a module as a macro: ~a"
                       name)))))

(defcondition macro-as-value (bad-macro-import)
  ()
  (:documentation "Attempt to import a macro as a value.")
  (:report (lambda (c s)
             (with-slots (name) c
               (format s "Cannot import a macro as a value: ~a."
                       name)))))

(defun expand-binding-spec (spec lang source)
  (setf source (merge-pathnames source (base))
        lang (lang-name lang))
  (flet ((get-static-exports ()
           ;; This doesn't save any work. The static bindings are
           ;; always computed every time we import from a module. But
           ;; we still only want to compute them here if we absolutely
           ;; have to. Why? For friendlier debugging. Doing the check
           ;; here would prevent us from macroexpanding `import' at
           ;; all if there was a problem with the imports, which is
           ;; frustrating. Instead, we push the check down into the
           ;; `check-static-bindings-now' macro.
           (module-static-exports/cache lang source)))
    (etypecase-of binding-spec spec
      ((eql :all)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(,export :as ,sym)))
      ((eql :all-as-functions)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(,export :as #',sym)))
      ((tuple :import-set list)
       (let ((import-set (second spec)))
         (expand-import-set import-set #'get-static-exports)))
      (list spec))))

(defun guess-source (lang alias)
  (~>> (etypecase-of import-alias alias
         (var-alias alias)
         ((or function-alias macro-alias)
          (second alias)))
       string-downcase
       (make-pathname :name)
       (merge-input-defaults lang)))

(defmacro function-wrapper (fn)
  "Global definition for possible shadowing."
  fn)

(define-global-state *claimed-module-names* (make-hash-table :size 1024)
  "Table to track claimed modules, so we can warn if they are
  redefined.")

(def module-name-lock (bt:make-lock "Module name lock"))

(defun claim-module-name (module lang source)
  "Warn if MODULE is already in use with a different LANG and SOURCE."
  (synchronized (module-name-lock)
    (let* ((table *claimed-module-names*)
           (old-value (gethash module table))
           (new-value (list lang source)))
      (when old-value
        (unless (equal old-value new-value)
          (warn "~s was claimed for ~a in ~a" module source lang)))
      (setf (gethash module table) new-value))))

(defun lang+source (lang source module base &optional env)
  (setf source (macroexpand source env)) ;Allow a symbol macro as the source.
  (flet ((resolve-source (source)
           (merge-pathnames* (ensure-pathname source :want-pathname t)
                             base)))
    (cond
      ;; We have the source and the language.
      ((and source lang)
       (values (resolve-lang lang)
               (resolve-source source)))
      ;; We have the source, but not the language.
      (source
       (let ((source (resolve-source source)))
         (values (resolve-lang
                  (or (guess-lang+pos source)
                      (required-argument :as)))
                 source)))
      ;; We have the language, but not the source.
      (lang
       (values (resolve-lang lang)
               (resolve-source
                (or (guess-source lang module)
                    (required-argument :from)))))
      ;; We have neither the language nor the source.
      (t (whichever
          (required-argument :as)
          (required-argument :from))))))

(defun imports-with-module-as-function-not-supported (mod bindings values)
  (when (and (typep mod 'function-alias)
             (or bindings values))
    (error* "~
Binding imports (~a) from a module imported as a function (~a) is not currently supported."
            (append bindings values) mod)))

(defun resolve-import-spec
    (&key lang source bindings values module (base (base)) env prefix)
  (check-type base absolute-pathname)
  (check-type prefix string-designator)
  (mvlet* ((lang source (lang+source lang source module base env))
           (bindings values (bindings+values bindings values
                                             :lang lang
                                             :source source
                                             :prefix prefix)))
    (imports-with-module-as-function-not-supported module bindings values)
    (values lang source bindings values)))

(defmacro import (module &body (&key
                                  ((:as lang))
                                  ((:from source))
                                  ((:binding bindings))
                                  values
                                  prefix
                                  function-wrapper)
                  &environment env)
  "Syntax for importing from modules.

Note you can do (import #'foo ...), and the module will be bound as a function."
  ;; Ensure we have both the lang and the source.
  (receive (lang source bindings values)
      (resolve-import-spec :lang lang
                           :source source
                           :module module
                           :bindings bindings
                           :values values
                           :prefix prefix
                           :env env)
    ;; Warn if MODULE is already in use with an incompatible language
    ;; and source.
    (claim-module-name module lang source)
    (let ((lazy? (null values)))
      `(progn
         ;; Importing modules non-lazily has a speed advantage when
         ;; there are no bindings, but it also makes maintenance more
         ;; complex to have two import forms. For now, just use lazy
         ;; imports; maybe re-enable eager loading later.

         ;; Also: while it happens to be the case that Serapeum's `def'
         ;; expands into a symbol macro definition, so switching between
         ;; lazy and eager imports works, it is *conceptually* weird
         ;; that we can just go ahead and redefine a global lexical as a
         ;; symbol macro.
         (import-module/lazy ,module :as ,lang :from ,source)
         #+ () (,(if lazy? 'import-module/lazy 'import-module)
                ,module :as ,lang :from ,(merge-pathnames* source (base)))
         ;; We push the check down into a separate macro so we can
         ;; inspect overall macroexpansion without side effects.
         (check-static-bindings-now ,lang ,source ,(append bindings values))
         (macrolet ((function-wrapper (fn)
                      ,(if function-wrapper
                           `(list ',function-wrapper fn)
                           'fn)))
           (import-bindings ,module ,@bindings)
           (import-values ,module ,@values))
         (import-task ,module :as ,lang :from ,source :values ,values :lazy ,lazy?)
         ;; Strictly for debuggability.
         (values ',module ',(append bindings values))))))

(defun bindings+values (bindings values &key lang source prefix)
  ;; Avoid redundant calls to module-static-bindings.
  (with-static-exports-cache ()
    (flet ((expand (spec)
             (~> (expand-binding-spec spec lang source)
                 canonicalize-bindings
                 (apply-prefix prefix))))
      (let ((bindings (expand bindings))
            (values (expand values)))
        (if *always-import-values*
            (values nil (append bindings values))
            (values bindings values))))))

(defmacro check-static-bindings-now (lang source bindings)
  "Wrapper around check-static-bindings to force evaluation at compile time.
Can't use eval-when because it has to work for local bindings."
  (check-static-bindings lang source bindings))

(defcondition binding-export-mismatch (overlord-error)
  ((bindings :initarg :bindings :type list)
   (exports :initarg :exports :type list))
  (:report (lambda (c s)
             (with-slots (bindings exports) c
               (format s "Requested bindings do not match exports.~%Bindings: ~s~%Exports: ~s"
                       bindings exports)))))

(defun check-static-bindings (lang source bindings)
  "Check that BINDINGS is free of duplicates. Also, using
`module-static-exports', check that all of the symbols being bound are
actually exported by the module specified by LANG and SOURCE."
  (ensure-lang-exists lang)
  (when bindings
    (when (relative-pathname-p source)
      (setf source (merge-pathnames* source (base))))
    (restart-case
        (progn
          (build (module-cell lang source))
          (let ((exports
                  (module-static-exports lang source))
                (bindings
                  (mapcar (op (import-keyword (first _)))
                          (canonicalize-bindings bindings))))
            ;; Check for duplicated bindings.
            (unless (set-equal bindings (nub bindings))
              (error* "Duplicated bindings."))
            ;; Make sure the bindings match the exports.
            (unless (subsetp bindings exports :test #'string=)
              (error 'binding-export-mismatch
                     :bindings bindings
                     :exports exports))))
      (recompile-object-file ()
        :report "Recompile the object file."
        (let ((object-file (faslize lang source))
              (target (module-cell lang source)))
          (delete-file-if-exists object-file)
          (build target)
          (check-static-bindings lang source bindings))))))

(defmacro import-module (module &key as from)
  (let ((req-form `(require-as ',as ,from)))
    (etypecase-of import-alias module
      (var-alias
       `(overlord/shadows:def ,module ,req-form))
      (function-alias
       `(overlord/shadows:defalias ,(second module) ,req-form))
      (macro-alias
       (error 'module-as-macro :name (second module))))))

(defmacro import-module/lazy (module &key as from)
  (save-module-dependency (module-cell as from))
  (let ((lazy-load `(load-module/lazy ',as ,from)))
    (etypecase-of import-alias module
      (var-alias
       `(overlord/shadows:define-symbol-macro ,module ,lazy-load))
      (function-alias
       (let ((fn (second module)))
         `(progn
            (declaim (notinline ,fn))
            (overlord/shadows:defun ,fn (&rest args)
              (apply ,lazy-load args)))))
      (macro-alias
       (error 'module-as-macro :name (second module))))))

(defmacro import-task (module &key as from values lazy)
  (declare (ignorable lazy))
  (let ((task-name
          (etypecase-of import-alias module
            (var-alias module)
            ((or function-alias macro-alias)
             (second module)))))
    `(deftask ,task-name
       (progn
         (require-as ',as ,from)
         ;; Put this back if we ever allow non-lazy loaded modules again.
         #+ () ,(let ((req-form `(require-as ',as ,from)))
                  (if lazy
                      req-form
                      `(setf ,module ,req-form)))
         (update-value-bindings ,module ,@values)))))

(defmacro update-value-bindings (module &body values)
  `(progn
     ,@(collecting
         (dolist (clause values)
           (receive (import alias ref) (import+alias+ref clause module)
             (declare (ignore import))
             (collect
                 (etypecase-of import-alias alias
                   (var-alias `(setf ,alias ,ref))
                   (function-alias
                    `(setf (symbol-function ',(second alias)) ,ref))
                   (macro-alias
                    ;; TODO Why not? It's just setf of macro-function.
                    (error 'macro-as-value :name (second alias))))))))))

(defmacro import-bindings (module &body bindings &environment env)
  `(progn
     ,@(mapcar (op (import-binding _ module env))
               bindings)))

(defmacro import-values (module &body values)
  `(progn
     ,@(mapcar (op (import-value _ module)) values)))

(defun canonicalize-binding (clause)
  (assure canonical-binding
    (if (typep clause 'canonical-binding)
        clause
        (etypecase-of binding-designator clause
          (var-spec
           (list (make-keyword clause) clause))
          (function-alias
           (list (make-keyword (second clause)) clause))
          (macro-alias
           (list (make-keyword (second clause)) clause))
          ((tuple symbol :as import-alias)
           (destructuring-bind (import &key ((:as alias))) clause
             (list (make-keyword import) alias)))))))

(defun canonicalize-bindings (clauses)
  (mapcar #'canonicalize-binding clauses))

(defun apply-prefix (clauses prefix)
  (if (null prefix) clauses
      (flet ((prefix (suffix) (symbolicate prefix suffix)))
        (loop for (import alias) in clauses
              collect (list import
                            (etypecase-of import-alias alias
                              (var-alias (prefix alias))
                              (function-alias `(function ,(prefix (second alias))))
                              (macro-alias `(macro-function ,(prefix (second alias))))))))))

(defun import-binding (clause module &optional env)
  (receive (import alias ref) (import+alias+ref clause module)
    (declare (ignore import))
    (etypecase-of import-alias alias
      (var-alias
       `(overlord/shadows:define-symbol-macro ,alias ,ref))
      (function-alias
       (let ((alias (second alias))
             (exp (macroexpand-1 `(function-wrapper ,ref) env)))
         ;; We used to use dynamic-extent declarations here, but Core
         ;; Lisp binds `args' as a symbol macro, and SBCL, stickler
         ;; that it is, objects to dynamic-extent declarations for
         ;; symbol macros.
         (if (equal exp ref)
             `(progn
                (declaim (notinline ,alias))
                (overlord/shadows:defun ,alias (&rest args)
                  (apply ,ref args)))
             `(progn
                (overlord/shadows:defalias ,alias
                  (function-wrapper
                   (lambda (&rest args)
                    (apply ,ref args))))))))
      (macro-alias
       (let ((alias (second alias)))
         (with-gensyms (whole body env)
           `(overlord/shadows:defmacro ,alias (&whole ,whole &body ,body &environment ,env)
              (declare (ignore ,body))
              (funcall ,ref ,whole ,env))))))))

(defun import-value (clause module)
  (receive (import alias ref) (import+alias+ref clause module)
    (declare (ignore import))
    (etypecase-of import-alias alias
      (var-alias
       `(overlord/shadows:def ,alias ,ref))
      (function-alias
       (let ((alias (second alias)))
         `(overlord/shadows:defalias ,alias
            (function-wrapper ,ref))))
      (macro-alias
       (error 'macro-as-value :name (second alias))))))

(defun import+alias+ref (clause module)
  (destructuring-bind (import alias) (canonicalize-binding clause)
    (let* ((key (import-keyword import))
           (ref `(module-ref* ,module ',key)))
      (values import alias ref))))

(defun import-keyword (import)
  (if (symbolp import)
      (make-keyword import)
      (make-keyword (second import))))

(defmacro import/local (mod &body (&key from as binding values prefix)
                        &environment env)
  (receive (lang source bindings values)
      (resolve-import-spec :lang as
                           :source from
                           :prefix prefix
                           :module mod
                           :bindings binding
                           :values values
                           :env env)
    ;; TODO If we knew that no macros were being imported, we could
    ;; give the module a local binding and not have to look it up
    ;; every time.
    `(progn
       (import-module/lazy ,mod :as ,lang :from ,source)
       (check-static-bindings-now ,lang ,source ,(append bindings values))
       (import-bindings ,mod ,@bindings)
       (import-values ,mod ,@values))))

(defmacro with-imports ((mod &key from as binding values prefix) &body body)
  "A version of `import' with local scope."
  `(local*
     (import/local ,mod
       :from ,from
       :as ,as
       :binding ,binding
       :values ,values
       :prefix ,prefix)
     (progn ,@body)))

(defmacro import-as-package (package-name
                             &body body
                             &key ((:as lang))
                                  ((:from source) (guess-source lang package-name))
                                  ((:binding bindings))
                                  values
                                  prefix
                             &allow-other-keys
                             &environment env)
  "Like `import', but instead of creating bindings in the current
package, create a new package named PACKAGE-NAME which exports all of
the symbols bound in the body of the import form."
  (receive (lang source bindings values)
      (resolve-import-spec :lang lang
                           :source source
                           :bindings bindings
                           :values values
                           :module 'package-module
                           :prefix prefix
                           :env env)
    (declare (ignore source lang))
    (let ((body (list* :binding bindings
                       :values values
                       (remove-from-plist body :prefix :binding :values))))
      `(progn
         (import->defpackage ,package-name ,@body)
         ;; The helper macro must be expanded after package-name has
         ;; been defined.
         (import-as-package-aux ,package-name ,@body)))))

(defmacro import->defpackage (package-name
                              &body (&rest body
                                     &key
                                       ((:binding bindings))
                                       values
                                     &allow-other-keys))
  (declare (ignore body))
  `(defpackage ,package-name
     (:use)
     (:export ,@(nub (loop for (nil alias) in (append bindings values)
                           collect (make-keyword
                                    (etypecase-of import-alias alias
                                      (var-alias alias)
                                      (function-alias (second alias))
                                      (macro-alias (second alias)))))))))

(defmacro import-as-package-aux (package-name &body
                                                (&rest body
                                                 &key ((:binding bindings))
                                                      values
                                                 &allow-other-keys))
  (let ((p (assure package (find-package package-name))))
    (labels ((intern* (sym)
               (intern (string sym) p))
             (intern-spec (spec)
               (loop for (key alias) in spec
                     collect `(,key :as ,(etypecase-of import-alias alias
                                           (var-alias (intern* alias))
                                           (function-alias
                                            (let ((alias (second alias)))
                                              `(function ,(intern* alias))))
                                           (macro-alias
                                            (let ((alias (second alias)))
                                              `(macro-function ,(intern* alias)))))))))
      (let ((module-binding (symbolicate '%module-for-package- (package-name p))))
        `(import ,module-binding
           :binding ,(intern-spec bindings)
           :values  ,(intern-spec values)
           ,@body)))))

(defun subpackage-full-name (child-package-name)
  (let* ((parent-package *package*)
         (parent-package-name (package-name parent-package))
         (child-package-name (string child-package-name))
         (full-package-name
           (fmt "~a.~a" parent-package-name child-package-name)))
    (make-keyword full-package-name)))

(defmacro import-as-subpackage (child-package-name
                                &body body
                                &key
                                &allow-other-keys)
  `(import-as-package ,(subpackage-full-name child-package-name)
     ,@body))
