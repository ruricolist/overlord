(defpackage :overlord/target
  (:use
    :cl
    :alexandria
    :serapeum
    :local-time

    :uiop/filesystem
    :uiop/pathname

    ;; What we need to implement.
    :overlord/redo
    ;; Timestamps.
    :overlord/stamp
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
    :overlord/hash-lang-syntax
    ;; Import sets.
    :overlord/import-set
    ;; Logging.
    :overlord/message
    ;; Running shell commands.
    :overlord/cmd
    ;; The database.
    :overlord/db
    ;; Freezing the state of the Lisp image.
    :overlord/freeze
    ;; Oracles.
    :overlord/oracle)
  (:import-from :named-readtables :in-readtable)
  (:import-from :fset)
  (:import-from :trivia
    :match :ematch :let-match1 :multiple-value-ematch
    :multiple-value-match)
  (:import-from :trivial-file-size
    :file-size-in-octets)
  ;; Portability shim for "global" or "static" vars. They  global
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
    :rename-file-overwriting-target)
  ;; How to escape names for use in pathnames.
  (:shadow :defconfig :import)
  ;; Shadow for style.
  (:shadow
   :if                                  ;Always ternary.
   :if-let                              ;Ditto.
   :cond                                ;Require exhaustive.
   :set                                 ;Use symbol-value.
   :defclass                            ;Force checking slot types.
   :typecase                            ;Use typecase-of instead.
   :etypecase                           ;Use etypecase-of instead.
   :ctypecase                           ;Use ctypecase-of instead.
   :case :ecase :ccase
   :file-write-date                     ;Use file-mtime instead.
   :pathname                            ;Use ensure-pathname.
   :multiple-value-bind                 ;Use receive.
   )
  (:export
   ;; Defining and building targets.
   :deftask
   :defconfig
   :define-target-config
   :var-target
   :define-target-var
   :file-target

   :ensure-absolute
   :extension
   :defpattern

   ;; Languages.
   :lang :lang-name :hash-lang-name
   :load-module
   :expand-module
   :package-expander :package-reader :module-progn-in
   :with-meta-language
   :load-same-name-system
   :define-loader-language

   ;; Loading.
   :*language* :*source*
   :read-lang-name
   :require-as :require-default
   :dynamic-require-as :dynamic-require-default

   ;; Module protocol.
   :module-meta

   :import :import/local
   :import-default
   :import-as-package
   :import-as-subpackage
   :with-imports
   :with-import-default
   :reintern :reinterning
   :*file-local-variables*
   :find-module

   ;; Emacs integration.
   :require-for-emacs
   :expand-module-for-emacs

   :find-pattern
   :build
   :run
   :depends-on
   :depends-on*
   :pdepends-on
   :depends-on-all
   :depends-on-all*
   :pdepends-on-all
   :depends-not
   :depends-not-all
   :with-script))

(in-package :overlord/target)
(in-readtable :standard)


;;; Shadows and preferred alternatives.

(deftype pathname ()
  'cl:pathname)

;;; This confuses CCL: it thinks our `pathname' is a built-in type and
;;; won't compile the above `deftype' form.

;; (setf (find-class 'pathname)
;;       (find-class 'cl:pathname))

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


;;; Auxiliary functions for Redo.

(defconst nonexist       :nonexist)
(defconst prereqs        :prereqs)
(defconst prereqs-temp   :prereqs-temp)
(defconst prereqsne      :prereqsne)
(defconst prereqsne-temp :prereqsne-temp)
(defconst stamp          :stamp)
(defconst uptodate       :uptodate)

(defun saved-prereq (x &optional (stamp (target-stamp x)))
  (cons x (assure stamp stamp)))

(defmethod saved-prereq-target (p) (car p))
(defmethod saved-prereq-stamp (p) (cdr p))

(defplace temp-prereqs (target)
  (prop target prereqs-temp (fset:empty-map)))

(defplace temp-prereqsne (target)
  (prop target prereqsne-temp (fset:empty-set)))

(defmethod record-prereq (target &aux (parent (current-parent)))
  (record-parent-prereq parent target))

(defun record-parent-prereq (parent target)
  (check-type target target)
  (unless (root-target? parent)
    (withf (temp-prereqs parent)
           target
           (target-stamp target))))

(defmethod record-prereqne (target &aux (parent (current-parent)))
  (record-parent-prereqne parent target))

(defun record-parent-prereqne (parent target)
  (check-type target target)
  (withf (temp-prereqsne parent) target))

(defmethod target-in-db? (target)
  (has-prop? target
             uptodate
             prereqs
             prereqs-temp
             prereqsne
             prereqsne-temp))

(defmethod clear-temp-prereqs (target)
  (delete-prop target prereqs-temp))

(defmethod clear-temp-prereqsne (target)
  (delete-prop target prereqsne-temp))

(defmethod save-temp-prereqs (target)
  (let ((map (temp-prereqs target)))
    (if (fset:empty? map)
        (delete-prop target prereqs)
        (setf (prop target prereqs) map))
    (clear-temp-prereqs target)))

(defmethod save-temp-prereqsne (target)
  (let ((set (temp-prereqsne target)))
    (if (fset:empty? set)
        (delete-prop target prereqsne)
        (setf (prop target prereqsne) set))
    (clear-temp-prereqsne target)))

(defmethod target-up-to-date? (target)
  (prop target uptodate))

(defmethod (setf target-up-to-date?) (value target)
  (check-type value boolean)
  (if value
      (setf (prop target uptodate) t)
      (delete-prop target uptodate)))

(defmethod target-saved-prereqs (target)
  (let ((map (prop target prereqs)))
    (and (typep map 'fset:map)
         (collecting
           (fset:do-map (k v map)
             (collect (saved-prereq k v)))))))

(defun (setf target-saved-prereqs) (value target)
  (setf (prop target prereqs)
        (assure fset:map value)))

(defmethod target-saved-prereqsne (target)
  (let ((set (prop target prereqsne)))
    (and (typep set 'fset:map)
         (fset:convert 'list set))))

(defun (setf target-saved-prereqsne) (value target)
  (setf (prop target prereqsne)
        (assure fset:set value)))


;;; Types.

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

(defgeneric load-form-slot-names (self)
  (:method-combination append))

(defclass externalizable ()
  ())

(defmethod make-load-form ((self externalizable) &optional env)
  (make-load-form-saving-slots self
                               :slot-names (load-form-slot-names self)
                               :environment env))

(defmethod load-form-slot-names append ((self externalizable))
  nil)

(defclass ref (externalizable)
  ((name
    :reader ref.name
    :type t
    :initarg :name))
  (:documentation "Base class for different kinds of by-name references."))

(defmethods ref (self name)
  (:method initialize-instance :after (self &key &allow-other-keys)
    (unless (slot-boundp self 'name)
      (error* "No name")))

  (:method load-form-slots append (self)
    '(name))

  (:method fset:compare (self (other ref))
    (fset:compare-slots self other #'class-name-of #'ref.name)))

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
  (:method load-form-slot-names append (self)
    '(nicknames use-list)))

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

(defgeneric print-pattern-ref (pattern ref stream)
  (:method ((pattern symbol) ref stream)
    (print-pattern-ref (find-pattern pattern) ref stream)))

(defmethods pattern-ref (self (input name) output pattern)
  (:method initialize-instance :after (self &key)
    ;; Merge in the defaults for inputs and outputs.
    (let ((pattern (find-pattern pattern)))
      (setf (values input output)
            (merge-pattern-defaults pattern input))))

  (:method print-object (self stream)
    (print-pattern-ref pattern self stream))

  (:method load-form-slot-names append (self)
    '(pattern output))

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

(defconstructor module-spec
  (lang lang-name)
  (path absolute-pathname))

(fset:define-cross-type-compare-methods module-spec)

(defmethod fset:compare ((spec1 module-spec) (spec2 module-spec))
  (nest
   (let-match1 (module-spec lang1 path1) spec1)
   (let-match1 (module-spec lang2 path2) spec2)
   (if (and (eql lang1 lang2)
            (equal path1 path2))
       :equal
       (let ((list1 (list lang1 path1))
             (list2 (list lang2 path2)))
         (declare (dynamic-extent list1 list2))
         (fset:compare list1 list2)))))

(defunit root-target)

(defmethod root-target ()
  root-target)

(defun root-target? (x)
  (eql x root-target))

(defun current-parent ()
  (or (first *parents*)
      root-target))

(defmethods root-target (self)
  (:method fset:compare (self (obj t))
    (if (eq self obj) :equal :unequal))
  (:method fset:compare ((obj t) self)
    (if (eq self obj) :equal :unequal)))

(fset:define-cross-type-compare-methods root-target)

(defunit impossible-target)
(defunit trivial-target)

(defmethod fset:compare ((x impossible-target) (y impossible-target))
  :equal)

(defmethod fset:compare ((x trivial-target) (y trivial-target))
  :equal)

(fset:define-cross-type-compare-methods impossible-target)
(fset:define-cross-type-compare-methods trivial-target)

(defmethod generate-impossible-target ()
  impossible-target)

(defconstructor phony-target
  (name symbol))

(defmethod fset:compare ((x phony-target) (y phony-target))
  (fset:compare-slots x y #'phony-target-name))

(fset:define-cross-type-compare-methods phony-target)

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
    impossible-target
    trivial-target
    phony-target
    bindable-symbol
    pathname
    package-ref
    directory-ref
    pattern-ref
    module-spec
    oracle))

(defconstructor task
  "A task."
  (target target)
  (thunk function)
  (script target))


;;; Manipulating targets.

(define-global-state *symbol-timestamps* (make-hash-table :size 1024))
(declaim (type hash-table *symbol-timestamps*))

(defun pathname-exists? (path)
  (etypecase-of (or string pathname) path
    (string (pathname-exists? (ensure-pathname path :want-pathname t)))
    (pathname
     (or (file-exists-p path)
         (directory-exists-p path)))))

(defmethod target-exists? (target)
  (true
   (etypecase-of target target
     ((or root-target trivial-target) t)
     ((or impossible-target phony-target) nil)
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
     (module-spec
      (~> target
          module-spec-cell
          module-cell.module))
     (oracle (oracle-exists? target)))))

(defun target-timestamp (target)
  (etypecase-of target target
    ((or root-target impossible-target phony-target)
     never)
    (trivial-target far-future)
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
    (module-spec
     (let* ((cell (module-spec-cell target)))
       (with-slots (module timestamp) cell
         (if (null module) never timestamp))))
    (oracle (oracle-timestamp target))))

(defun (setf target-timestamp) (timestamp target)
  (check-type timestamp target-timestamp)
  (check-not-frozen)
  (etypecase-of target target
    (root-target (error* "Cannot set timestamp of root target."))
    ((or impossible-target trivial-target phony-target)
     (error* "Cannot set timestamp for ~a" target))
    (bindable-symbol
     ;; Configurations need to set the timestamp while unbound
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
    ((or pattern-ref package-ref oracle)
     ;; TODO Or does it?
     (error* "Setting the timestamp of ~s does not make sense."))
    (module-spec
     (let ((cell (module-spec-cell target)))
       (setf (module-cell.timestamp cell) timestamp)))))

(defun touch-target (target &optional (date (now)))
  (setf (target-timestamp target) date))

(defun delete-file-or-directory (p)
  (if (directory-pathname-p p)
      (delete-directory-tree p)
      (delete-file-if-exists p)))

(defun resolve-target (target base)
  (setf base (pathname-directory-pathname base))
  (when (typep base 'temporary-file)
    (simple-style-warning "Base looks like a temporary file: ~a" base))
  (etypecase-of target target
    ((or root-target impossible-target trivial-target
         bindable-symbol package-ref
         phony-target
         oracle)
     target)
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
    (module-spec
     (let-match1 (module-spec lang source) target
       (module-spec lang
                    (assure tame-pathname
                      (merge-pathnames* source base)))))))


;;; Target table abstract data type.

(defun target-type-of (x)
  (let ((type (target-type-of x)))
    ;; Remember that `nil' is a subtype of everything.
    (assert (subtypep type 'target))
    type))

(defun target-type-of-1 (x)
  (typecase-of target x
    (root-target 'root-target)
    (trivial-target 'trivial-target)
    (impossible-target 'impossible-target)
    (bindable-symbol 'bindable-symbol)
    (pathname 'pathname)
    (module-spec 'module-spec)
    (package-ref 'package-ref)
    (directory-ref 'directory-ref)
    (pattern-ref 'pattern-ref)
    (phony-target 'phony-target)
    (oracle 'oracle)
    ;; Bottom.
    (otherwise nil)))

(defmethod target= (x y)
  "Are two targets the same?"
  (etypecase-of target x
    (root-target
     (typep y 'root-target))
    (trivial-target
     (typep y 'trivial-target))
    (impossible-target
     (typep y 'impossible-target))
    (phony-target
     (and (typep y 'phony-target)
          (eql (phony-target-name x)
               (phony-target-name y))))
    (bindable-symbol
     (eql x y))
    (pathname
     (and (pathnamep y)
          (pathname-equal x y)))
    (module-spec
     (multiple-value-match (values x y)
       (((module-spec lang1 path1)
         (module-spec lang2 path2))
        (and (eql lang1 lang2)
             (pathname-equal path1 path2)))))
    (package-ref
     (and (typep y 'package-ref)
          (compare #'string= #'ref.name x y)))
    (directory-ref
     (and (typep y 'directory-ref)
          (compare #'pathname-equal #'ref.name x y)))
    (oracle
     (oracle= x y))
    (pattern-ref
     (and (typep y 'pattern-ref)
          (and (compare #'equal #'pattern-ref.input    x y)
               (compare #'eql   #'pattern-ref.pattern  x y)
               (compare #'equal #'pattern-ref.output   x y))))))

(defun hash-target (target)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (compilation-speed 0)))
  (etypecase-of target target
    (root-target
     (load-time-value (sxhash root-target)))
    (trivial-target
     (load-time-value (sxhash trivial-target)))
    (impossible-target
     (load-time-value (sxhash impossible-target)))
    (bindable-symbol (sxhash target))
    (pathname (sxhash target))
    (module-spec
     (let-match1 (module-spec lang path) target
       (dx-sxhash (list 'module-spec lang path))))
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
            (ref.name target))))
    (oracle
     (hash-oracle target))
    (phony-target
     (let ((name (phony-target-name target)))
       (dx-sxhash `(phony ,name))))))

(deftype hash-friendly-target ()
  '(or root-target
    impossible-target
    trivial-target
    bindable-symbol
    pathname))

(defstruct (target-table (:conc-name target-table.)
                         (:constructor %make-target-table))
  (map (fset:empty-map) :type fset:map)
  (hash-table (make-hash-table :test 'equal :size 1024)
   :type hash-table :read-only t)
  (lock (bt:make-recursive-lock) :read-only t)
  (synchronized nil :type boolean :read-only t))

;;; Ensure target tables can be written.

(defmethod print-object ((self target-table) stream)
  (when (or (null *print-readably*)
            (not *read-eval*))
    (return-from print-object
      (call-next-method)))
  (write-string (read-eval-prefix self stream) stream)
  (format stream "~s"
          `(alist-to-target-table
            '(,@(target-table-to-alist self)))))

(defun make-target-table (&key (size 1024) synchronized)
  (%make-target-table
   :hash-table (make-hash-table :test 'equal
                                :size (max 1024 size))
   :synchronized synchronized))

(defun alist-to-target-table (alist)
  (lret* ((len (length alist))
          (table (make-target-table :size len)))
    (loop for (k . v) in alist
          do (setf (target-table-ref table k) v))))

(defmacro with-target-table-locked ((target-table) &body body)
  (once-only (target-table)
    (with-thunk (body)
      `(if (target-table.synchronized ,target-table)
           (bt:with-recursive-lock-held ((target-table.lock ,target-table))
             (funcall ,body))
           (funcall ,body)))))

(defun target-table-len (table)
  (with-target-table-locked (table)
    (let ((hash-table (target-table.hash-table table))
          (map (target-table.map table)))
      (+ (hash-table-count hash-table)
         (fset:size map)))))

(defun target-table-to-alist (table)
  (collecting
    (let ((hash-table (target-table.hash-table table))
          map)
      (with-target-table-locked (table)
        (setf map (target-table.map table))
        (do-hash-table (k v hash-table)
          (collect (cons k v))))
      (fset:do-map (k v map)
        (collect (cons k v))))))

(defun target-table-ref (table key)
  (with-target-table-locked (table)
    (etypecase-of target key
      (hash-friendly-target
       (let ((hash (target-table.hash-table table)))
         (gethash key hash)))
      (target
       (fset:lookup (target-table.map table) key)))))

(defun (setf target-table-ref) (value table key)
  (prog1 value
    (with-target-table-locked (table)
      (etypecase-of target key
        (hash-friendly-target
         (let ((hash (target-table.hash-table table)))
           (setf (gethash key hash) value)))
        (target
         (callf #'fset:with (target-table.map table) key value))))))

(defun target-table-rem (table key)
  (prog1 nil
    (with-target-table-locked (table)
      (etypecase-of target key
        (hash-friendly-target
         (let ((hash (target-table.hash-table table)))
           (remhash key hash)))
        (target
         (callf #'fset:less (target-table.map table) key))))))

(defun target-table-member (table key)
  (nth-value 1
    (target-table-ref table key)))

(defun (setf target-table-member) (value table key)
  (prog1 value
    (if value
        (with-target-table-locked (table)
          (unless (target-table-member table key)
            (setf (target-table-ref table key) t)))
        (target-table-rem table key))))

(defun target-table-keys (table)
  (with-target-table-locked (table)
    (collecting
      ;; Keys from the hash table.
      (do-hash-table (k v (target-table.hash-table table))
        (declare (ignore v))
        (collect k))
      ;; Keys from the Fset map.
      (fset:do-map (k v (target-table.map table))
        (declare (ignore v))
        (collect k)))))

(defun clear-target-table (table)
  (with-target-table-locked (table)
    (clrhash (target-table.hash-table table))
    (setf (target-table.map table)
          (fset:empty-map))))

(defun deduplicate-targets (targets &key (key #'identity))
  ;; (test-chamber:with-experiment
  ;;     (:class 'test-chamber:noisy-experiment
  ;;      :test (op (set-equal _ _ :test #'target=))
  ;;      :enabled nil)
  ;;   (remove-duplicates targets :test #'target=))
  (collecting
    (fbind key
      (let ((table (make-target-table :size (length targets))))
        (do-each (target targets)
          (let ((count
                  (incf
                   (ensure2 (target-table-ref table (key target))
                     0))))
            (when (= count 1)
              (collect target))))))))



;;; This is bad; how many locks can a program have? There could be
;;; millions of targets.
(defvar *target-locks-table*
  (make-target-table :size 1024))

(defmethod call-with-target-locked ((target t) (fn function))
  (let ((lock
          (let ((table *target-locks-table*))
            (or (target-table-ref table target)
                (synchronized (table)
                  (ensure (target-table-ref table target)
                    (bt:make-lock)))))))
    (synchronized (lock)
      (funcall fn))))


;;; Building targets (scripts).

(define-global-state *tasks* (dict))
(declaim (type hash-table *tasks*))

(define-global-state *top-level-targets* (make-target-table :synchronized t))
(declaim (type target-table *top-level-targets*))

(defun ensure-target-recorded (target)
  (if *parents*
      (record-prereq target)
      (setf (target-table-member *top-level-targets* target) t)))

(defun list-top-level-targets ()
  (target-table-keys *top-level-targets*))

(defun trivial-task (target)
  (task target
        (constantly nil)
        trivial-target))

(defun impossible-task (target)
  (task target
        (constantly nil)
        impossible-target))

(defmethod target-build-script (target)
  (check-not-frozen)
  (assure task
    (etypecase-of target target
      (pathname
       (or (gethash target *tasks*)
           (impossible-task target)))
      (bindable-symbol
       (or (gethash target *tasks*)
           ;; If there is no real target by that name, look for a
           ;; phony target.
           (target-build-script
            (phony-target target))))
      (phony-target
       (let* ((name (phony-target-name target))
              (key `(phony ,name)))
         (or (gethash key *tasks*)
             (impossible-task target))))
      (target (impossible-task target)))))

(defmethod target-default-build-script (target)
  (check-not-frozen)
  (assure task
    (etypecase-of target target
      ((or pathname bindable-symbol phony-target
           impossible-target trivial-target oracle)
       (trivial-task target))
      (root-target
       (task target
             (lambda ()
               ;; NB. Note that we do not get the prereqs of the root
               ;; target from the database. We do not want them to be
               ;; persistent; we only want to build the targets that
               ;; have been defined in this image.
               (let ((*building-root* t))
                 (depends-on-all (list-top-level-targets))))
             trivial-target))
      (pattern-ref
       (let* ((input (pattern-ref.input target))
              (output (pattern-ref.output target))
              (pattern (find-pattern (pattern-ref.pattern target))))
         (task output
               (lambda ()
                 (let ((*input* input)
                       (*output* output))
                   (let ((*base* (pathname-directory-pathname input)))
                     (depends-on input))
                   (pattern-build pattern)))
               (pattern.script pattern))))
      (directory-ref
       (let ((dir (ref.name target)))
         (task target
               (lambda ()
                 (let ((dir (resolve-target dir (base))))
                   (ensure-directories-exist dir)))
               trivial-target)))
      (package-ref
       (with-slots (name use-list nicknames) target
         (task target
               (lambda ()
                 (or (find-package name)
                     (make-package name
                                   :use use-list
                                   :nicknames nicknames)))
               trivial-target)))
      (module-spec
       (let ((cell (module-spec-cell target)))
         (with-slots (lang source) cell
           (task target
                 (lambda ()
                   (let ((*base* (pathname-directory-pathname source)))
                     ;; Depend on the source file.
                     (depends-on source)
                     ;; Let the language tell you what to depend on.
                     (lang-deps lang source))

                   (let ((*language* lang))
                     (load-module-into-cell cell)))
                 trivial-target)))))))

(defmethod build-script-target (script)
  (task-script script))

(defmethod run-script (task &aux (parent (current-parent)))
  (check-not-frozen)
  ;; XXX exhaustive?
  (unless (typep parent
                 '(or impossible-target trivial-target))
    (print-target-being-built parent))
  (funcall (task-thunk task)))

(defun run-save-task (target thunk &optional (script (script-for target)))
  (check-not-frozen)
  (save-task target thunk script)
  (depends-on target))

(defun save-task (target thunk &optional (script (script-for target)))
  (check-not-frozen)
  (etypecase-of target target
    ((or root-target trivial-target impossible-target
         module-spec
         directory-ref package-ref pattern-ref
         oracle)
     (error* "Task for ~a cannot be redefined." target))
    ((or bindable-symbol pathname)
     (let ((task (task target thunk script)))
       (setf (gethash target *tasks*) task)))
    (phony-target
     (let* ((name (phony-target-name target))
            (task (task target thunk script))
            (key `(phony ,name)))
       (setf (gethash key *tasks*) task)))))

(defun task-values (task)
  (values (task-target task)
          (task-thunk task)
          (task-script task)))

(defun system-loaded? (system)
  (let ((system (asdf:find-system system nil)))
    (and system
         (asdf:component-loaded-p system)
         system)))

(defun print-target-being-built (target)
  "Print the target being built.
The idea here (borrowed from Apenwarr redo) is that the user should be
able to simply evaluate the form for a given target to pick up
building from there."
  (let* ((depth (max 0 (1- (length *parents*))))
         (spaces (make-string depth :initial-element #\Space)))
    (message "~a~s"
             spaces
             `(build ,(dump-target/pretty target)))))

(defun dump-target/pretty (target)
  "Return a form which, when evaluated, returns a target equivalent to
TARGET."
  (etypecase-of target target
    (pathname target)
    (bindable-symbol `(quote ,target))
    (root-target 'root-target)
    (trivial-target 'trivial-target)
    (impossible-target 'impossible-target)
    (module-spec
     (let-match1 (module-spec lang path) target
       `(module-spec ,lang ,path)))
    (directory-ref
     `(directory-ref ,(ref.name target)))
    (package-ref
     `(package-ref ,(ref.name target)
                   :nicknames ,(package-ref.nicknames target)
                   :use-list ,(package-ref.use-list target)))
    (pattern-ref
     (let* ((pattern (pattern-ref.pattern target))
            (input (pattern-ref.input target))
            (pattern-name (class-name-of pattern)))
       `(pattern-ref (find-pattern ',pattern-name)
                     ,input)))
    (oracle
     (make-load-form target))
    (phony-target
     `(phony-target ,@(multiple-value-list (deconstruct target))))))

(defun file-stamp (file)
  (let ((size (file-size-in-octets file))
        (timestamp (target-timestamp file)))
    (file-meta size timestamp)))

(defmethod target-stamp (target)
  (assure stamp
    (etypecase-of target target
      ((or root-target
           trivial-target
           impossible-target
           bindable-symbol
           package-ref
           pattern-ref
           ;; TODO?
           directory-ref
           module-spec
           phony-target)
       (target-timestamp target))
      (oracle (oracle-timestamp target))
      (pathname
       (cond ((file-exists-p target)
              (file-stamp target))
             ((directory-pathname-p target)
              (target-timestamp target))
             (t deleted))))))

(defun rebuild-symbol (symbol thunk)
  (lambda ()
    (setf (symbol-value symbol)     (funcall thunk)
          (target-timestamp symbol) (now))))

(defun rebuild-file (file thunk &optional (base (base)))
  (lambda ()
    (let* ((file (resolve-target file base))
           (old (target-timestamp file)))
      (funcall thunk)
      ;; Since we depend on the granularity of the timestamps, all we can
      ;; be sure of is that is not older than the old timestamp.
      (assert (not (timestamp-newer? old (target-timestamp file)))))))

(defun save-file-task (file thunk script)
  (check-type file pathname)
  (check-type thunk function)
  (save-task file
             (rebuild-file file thunk (base))
             script))

(defun update-config-if-changed (name new test)
  "Initialize NAME, if it is not set, or reinitialize it, if the old
value and NEW do not match under TEST."
  (let ((old (symbol-value name)))
    (if (funcall test old new)
        old
        (progn
          (simple-style-warning "Redefining configuration ~s" name)
          (funcall (rebuild-symbol name (lambda () new)))))))


;;; Freezing the build system.

(defun hard-freeze-targets ()
  "Freeze targets."
  ;; The table of module cells needs special handling.
  ;; Variables aren't defined yet.
  (clear-target-table (symbol-value '*top-level-targets*))
  (clrhash (symbol-value '*symbol-timestamps*))
  (clrhash (symbol-value '*tasks*))
  ;; The table of module cells needs special handling.
  (clear-module-cells)
  (clrhash (symbol-value '*claimed-module-names*)))

(add-hook '*before-hard-freeze-hook*
          'hard-freeze-targets)


;;; API and keyword macros

(defun path (path)
  (~> path
      (ensure-pathname :want-pathname t)
      (merge-pathnames (base))))

(defun file (file)
  (assure file-pathname (path file)))

(defun basename (file)
  (enough-pathname file (pathname-directory-pathname file)))

(defun extension (ext)
  (assure pathname
    (etypecase-of (or null string) ext
      (null *nil-pathname*)
      (string (make-pathname :type ext)))))

(defun run (target &optional system-name)
  "Entry point for scripts."
  (mvlet* ((target package-name
            (ematch target
              ((and target (type symbol))
               (values target
                       (package-name (symbol-package target))))
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

(defun build (&rest targets)
  (redo-all targets))

(defun depends-on-all (targets)
  (redo-ifchange-all targets))

(defun depends-on-all* (targets)
  (map nil #'redo-ifchange targets))

(defun pdepends-on-all (targets)
  (redo-ifchange/parallel targets))

(defun depends-on (&rest targets)
  (depends-on-all targets))

(defun depends-on* (&rest targets)
  (depends-on-all* targets))

(defun pdepends-on (&rest targets)
  (pdepends-on-all targets))

(defun depends-not-all (targets)
  (redo-ifcreate-all targets))

(defun depends-not (&rest targets)
  (depends-not-all targets))

(defmacro with-script ((&key) &body body)
  `(macrolet ( ;; Depending on things in general.
              (:depends-on (x &rest xs)
                `(depends-on ,x ,@xs))
              (:depends-on* (x &rest xs)
                `(depends-on* ,x ,@xs))
              (:pdepends-on (x &rest xs)
                `(pdepends-on ,x ,@xs))
              (:depends-on-all (xs)
                `(depends-on-all ,xs))
              (:depends-on-all* (xs)
                `(depends-on-all* ,xs))
              (:pdepends-on-all (xs)
                `(pdepends-on-all ,xs))
              (:depends-not (x &rest xs)
                `(depends-not ,x ,@xs))
              (:depends-not-all (xs)
                `(depends-not-all ,xs))

              ;; Things to depend on.
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
                ;; Is (base) right?
                `(module-spec ',lang
                              (resolve-target (ensure-pathname ,source)
                                              ,(base))))

              ;; Depending on specific things.
              (:use-env (name)
                `(use-env ,name))
              (:use-var (var)
                `(use-var ,var))
              (:always (&optional (bool t))
                `(and ,bool (redo-always)))


              ;; Utilities.
              (:extension (ext)
                `(extension ,ext))
              (:cmd (&rest args)
                `(cmd ,@args))
              (:message (control-string &rest args)
                `(message ,control-string ,@args))
              (:basename (file)
                `(basename ,file)))
     ,@body))


;;; In-Lisp targets.

(defun save-base (form)
  `(let ((*base* ,(base)))
     (with-current-dir (*base*)
       ,form)))

;;; `defconfig' is extremely important and rather tricky. It is one of
;;; the only two types of targets that have inherent timestamps (the
;;; other being files). Semantically is it closer to `defconstant'
;;; than `defvar' -- the provided expression is evaluated at compile
;;; time -- but unlike `defconstant' it can always be redefined.

;;; How does `defconfig' have its own timestamp? When `defconfig' is
;;; being compiled, the current timestamp is dumped directly into the
;;; emitted code, so it persists when the fasl is reloaded.

;;; Note that while configuration timestamps persist across loads,
;;; they do not persist across compilations. If that is the behavior
;;; you need, you should use `define-target-config' and depend on a dummy
;;; file. If we did it for you, a dummy file would have to be involved
;;; anyway, and that is probably something it is better to be explicit
;;; about.

(defmacro defconfig (name init &key (test '#'equal)
                                    documentation)
  (check-type name symbol)
  (let ((init
          (save-base
           `(with-script ()
              ,init))))
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
         (save-task ',name (constantly ,init) trivial-target))
       (eval-always
         (update-config-if-changed ',name ,init ,test))
       ',name)))

(defmacro script-thunk (&body body)
  `(lambda ()
     ,(save-base
       `(with-script ()
          ,@body))))

(defmacro define-script (name &body script)
  `(defconfig ,name ',script
     :test #'source=))

(defmacro define-script-for (name &body body)
  `(define-script ,(script-for name)
     ,@body))

(defun script-for (name)
  (intern (coerce-case (fmt "~a.do" name))
          (symbol-package name)))

(defun source= (x y)
  ;; TODO How to test equality in the presence of macros?
  ;; Maybe expand with a code walker?
  (similar? x y))

(defmacro var-target (name expr &body deps)
  (let ((script (append1 deps expr)))
    `(progn
       (define-script-for ,name
         ,@script)
       (defvar ,name)
       (save-task ',name
                  (rebuild-symbol ',name (script-thunk ,@script)))
       ',name)))

(defmacro define-target-var (name expr &body deps)
  "Define a variable with dependencies.
A dependency can be a file or another variable.

If any of those files or variables change, then the variable is
rebuilt."
  (let ((docstring
          (and (stringp (car deps))
               (pop deps))))
    `(progn
       (var-target ,name ,expr
         ,@deps)
       (depends-on ',name)
       ,@(unsplice
          (when (stringp docstring)
            `(setf (documentation ',name 'variable)
                   ,docstring)))
       ',name)))

;;; NB There is no `config-target' because configuration variables
;;; must always be bound.

(defmacro define-target-config (name expr &body deps)
  "Define a conf with dependencies.
A dependency can be a file or another variable.

If any of those files or variables change, then the variable is
rebuilt."
  (let* ((docstring
           (and (stringp (car deps))
                (pop deps)))
         (script (append1 deps expr)))
    `(progn
       ;; The script must be available at compile time to be depended
       ;; on.
       (define-script-for ,name
         ,@script)
       ,@(unsplice
          (when docstring
            `(setf (documentation ',name 'variable)
                   ,docstring)))
       (define-target-config/aux ,name
         ,@script))))

(defmacro define-target-config/aux (name &body script)
  (unless (boundp name)
    (let* ((*base* (base))
           (script-thunk (eval* `(script-thunk ,@script)))
           (script-thunk (rebuild-symbol name script-thunk)))
      (save-task name script-thunk))
    (depends-on name))
  (let ((init (symbol-value name))
        (timestamp (target-timestamp name)))
    `(progn
       (define-global-var ,name
           (progn
             (setf (target-timestamp ',name) ,timestamp)
             ',init))
       (save-task ',name
                  (rebuild-symbol ',name (script-thunk ,@script)))
       (depends-on ',name)
       ',name)))


;;;; Phony targets.

(defmacro deftask (name &body script)
  "Define a task -- a target that only has dependencies.
This is essentially a convenience to let you use keyword macros to
specify the dependencies you want on build."
  `(progn
     (eval-always
       (assert (not (boundp ',name))))
     (define-script-for ,name
       ,@script)
     (save-task (phony-target ',name)
                (lambda ()
                  (redo-always)
                  ;; Phony targets don't *need* to be built.
                  (unless *building-root*
                    (funcall (script-thunk ,@script))))
                (script-for ',name))
     ',name))


;;; File targets.

(defun file-target-form (pathname script-form args)
  (ematch args
    ((list $1)
     `(let ((,$1 ,pathname))
        ,script-form))
    ((list $1 $3)
     `(call/temp-file-pathname
       ,pathname
       (lambda (,$3)
         (let ((,$1 ,pathname))
           ,script-form))))))

(defmacro file-target (name pathname (in &optional (temp nil temp?)) &body script)
  "If TMP is null, no temp file is used."
  (ensure-pathnamef pathname)
  (check-type pathname tame-pathname)
  (let* ((args (if temp? (list in temp) (list in)))
         (base (base))
         (pathname (resolve-target pathname base))
         (dir (pathname-directory-pathname pathname))
         (script-form
           `(with-current-dir (,dir)
              ,@script)))
    `(progn
       ;; Make the task accessible by name.
       (def ,name ,pathname)
       (define-script-for ,name
         ,script-form)
       (save-file-task ,pathname
                       (script-thunk
                         ,(file-target-form pathname script-form args))
                       (script-for ',name))
       ',pathname)))


;;;; File patterns.

;;; A pattern is an abstract relationship between two files.

(defclass pattern (externalizable)
  ((input-defaults
    :initarg :input-defaults
    :type pathname
    :reader pattern.input-defaults)
   (output-defaults
    :initarg :output-defaults
    :type pathname
    :reader pattern.output-defaults)
   (script
    :initarg :script
    :type target
    :reader pattern.script))
  (:default-initargs
   :input-defaults *nil-pathname*
   :output-defaults *nil-pathname*
   :script trivial-target))

(defmethod load-form-slot-names append ((self pattern))
  '(input-defaults output-defaults script))

(defmethod print-pattern-ref ((pattern pattern)
                              (ref pattern-ref)
                              stream)
  (with-slots ((input name) output) ref
    (let ((pattern-name
            (assure symbol
              (if (symbolp pattern)
                  pattern
                  (class-name-of pattern)))))
      (if *print-escape*
          (format stream "~a~s"
                  (read-eval-prefix ref stream)
                  `(pattern-ref ',pattern-name
                                ,input))
          (print-unreadable-object (ref stream :type t)
            (format stream "~a (~a -> ~a)"
                    pattern-name
                    input
                    output))))))

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
                      &body script)
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
  `(progn
     (define-script-for ,class-name
       ,in
       ,out
       ,@script)
     (with-script ()
       (defclass ,class-name (pattern)
         ()
         (:default-initargs
          :script ',(script-for class-name)
          ,@options)))
     (defmethod pattern-build ((self ,class-name))
       (let ((,in *input*)
             (,out *output*))
         (declare (ignorable ,in))
         (call/temp-file-pathname ,out
                                  (lambda (,out)
                                    (with-script ()
                                      ,@script)))))))


;;; Module cells.

;;; What's a module cell? What we want is a namespace for modules,
;;; indexed by language and source file. The obvious thing would be to
;;; declare a table (with `defvar') store modules there. Basically,
;;; that is what we do. But instead of simply storing the module in
;;; the table, we store an indirection -- a "module cell", a mutable
;;; cell which contains the actual module. Then, using compiler macros
;;; and `load-time-value', we can inject direct references to module
;;; cells into the compiled code. The result is that, for inline
;;; references to modules, no run-time lookup is necessary. This is
;;; key to keeping modules fast while also allowing for modules to be
;;; redefined. And ultimately it is similar to the strategy that Lisp
;;; itself uses to allow functions to be redefined at runtime without
;;; having to recompile all their callers.

(define-global-state *module-cells* (dict)
  "The global table of all module cells.")

(defun list-module-cells ()
  (hash-table-values *module-cells*))

;;; TODO Should this be a structure? But using a class gets us slot
;;; type checking on both SBCL and Clozure. A future optimization, but
;;; we can leave it for now.
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

(defun clear-module-cells ()
  "Delete information not needed at runtime from module cells."
  ;; We don't actually clear the table because there may be cases
  ;; where expansion of compiler macros has been suppressed by
  ;; optimization settings and there is no reference to the module
  ;; cell to keep it from being garbage-collected.
  (maphash (lambda (k mc) (declare (ignore k))
             (with-slots (source timestamp) mc
               (setf source *nil-pathname*
                     timestamp never)))
           *module-cells*))

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
                (locally
                    ;; Prevent recursive expansion.
                    (declare (notinline module-cell))
                  ;; We can't use %ensure-module-cell directly,
                  ;; because it doesn't apply the defaults for the
                  ;; language.
                  (module-cell ,lang ,path)))))
           (otherwise call)))
        ((constantp lang)
         (let ((val (eval lang)))
           (if (eql val lang) call
               `(module-cell ,val ,path))))
        (t call)))

(defun module-spec-cell (spec)
  (let-match1 (module-spec lang path) spec
    (module-cell lang path)))

(defun module-cell-spec (cell)
  (with-slots (lang source) cell
    (module-spec lang source)))

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
    (setf lock
          (bt:make-recursive-lock (fmt "Lock for module ~a" self))))

  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a (~a) (~:[not loaded~;loaded~])"
              source
              lang
              module))))

(defun load-module-into-cell (cell)
  (lret ((module
          (validate-module
           (load-module (module-cell.lang cell)
                        (module-cell.source cell)))))
    (setf
     (module-cell.module cell) module
     (module-cell.timestamp cell) (now))))

(defun unload-module (lang source)
  (declare (notinline module-cell))
  (lret ((m (module-cell lang source)))
    (with-slots (timestamp module) m
      (setf timestamp never)
      (nix module))))

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
                (build (module-cell-spec cell))
                module))))))


;;; Languages

;;; Note that support for languages follows support for file patterns.
;;; A pattern is an abstract relationship between two files; a
;;; language is an abstract relationship between a file and Lisp
;;; binding.

(defun %require-as (lang source *base* &rest args)
  (ensure-pathnamef source)
  (apply #'dynamic-require-as
         lang
         (merge-pathnames* source *base*)
         args))

(defun dynamic-require-default (lang source &key force)
  (let ((module (dynamic-require-as lang source :force force)))
    (module-ref module :default)))

(defun dynamic-require-as (lang source &key force)
  (check-type source (and absolute-pathname file-pathname))
  (ensure lang (guess-lang+pos source))
  (setf lang (lang-name lang))
  (when force
    (dynamic-unrequire-as lang source))
  (assure (not module-cell)
    (let ((spec (module-spec lang source)))
      (depends-on spec)
      (module-cell.module
       (module-spec-cell spec)))))

(defun %unrequire-as (lang source *base*)
  (dynamic-unrequire-as lang
                        (merge-pathnames* source *base*)))

(defun dynamic-unrequire-as (lang source)
  (check-type source (and absolute-pathname file-pathname))
  (unload-module lang source)
  (values))

(defmacro require-as (&rest args)
  "Wrap `%require-as`, resolving the base at macro-expansion time.
A single arg is treated as the source, with the language being inferred.
Two args is treated as the language and the source."
  (receive (lang source)
      (ematch args
        ((list source)
         (values nil source))
        ((list lang source)
         (values lang source)))
    `(%require-as ,lang ,source ,(base))))

(defmacro require-default (&rest args)
  `(module-ref* (require-as ,@args) :default))

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
  "Wrap `%unrequire-as', resolving the base at macro-expansion time."
  `(%unrequire-as ,lang ,source ,(base)))

(defun escape-lang-name (lang-name)
  (check-type lang-name lang-name)
  (url-encode (string lang-name) :encoding :utf-8))

(defun lang-fasl-dir (lang current-dir)
  (assure (and absolute-pathname directory-pathname)
    (let ((lang-string (escape-lang-name lang))
          (suffix
            (~>> current-dir
                 pathname-directory
                 (drop-while #'keywordp)
                 (cons :relative)
                 (make-pathname :directory))))
      (path-join
       (db-version-dir)
       #p"fasls/"
       (make-pathname :directory `(:relative ,lang-string))
       suffix))))

(defun faslize (lang pathname)
  (etypecase-of lang lang
    (package
     (~> lang
         package-name-keyword
         (faslize pathname)))
    (lang-name
     (path-join (lang-fasl-dir lang pathname)
                (make-pathname :name (pathname-name pathname)
                               :type fasl-ext)))))

(defun fasl? (pathname)
  (let ((type (pathname-type pathname)))
    (and (stringp type)
         (string= type fasl-ext))))

(defun load-module (lang source)
  (ensure-pathnamef source)
  (let ((*base* (pathname-directory-pathname source)))
    (load-fasl-lang lang source)))

(defmethod module-static-exports (lang source)
  (check-type source absolute-pathname)
  (let ((lang (resolve-lang-package lang)))
    (if-let (sym (find-external-symbol 'static-exports lang))
      ;; If the language exports a function to parse static exports,
      ;; use it.
      (values (funcall sym source) t)
      (values nil nil))))

(defun module-dynamic-exports (lang source)
  (module-exports* (dynamic-require-as lang source)))


;;; Languages.

;;; This is a generic function so individual langs can define their
;;; own dependencies in :after methods.

;;; TODO Should this use the progn method combination?
(defgeneric lang-deps (lang source)
  (:method ((lang t) (source t))
    nil)

  (:method ((lang symbol) (source t))
    (lang-deps (resolve-lang-package lang) source)))

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
  (let ((p (find-package package-name)))
    (unless (packagep p)
      (error "This macro cannot expand until package ~a exists."
             package-name))
    (let ((syms (mapcar (op (find-symbol (string _) p))
                        (loader-language-exports)))
          (keyword (package-name-keyword package-name)))
      (destructuring-bind (load read ext script) syms
        `(progn
           (declaim (notinline ,load ,read))
           (eval-always
             (define-script ,script ,reader)
             (defparameter ,ext (extension ,extension))
             (defun ,load (,source)
               (default-export-module ,reader))
             (defun ,read (,source _stream)
               (declare (ignore _stream))
               (list ',load ,source))
             (defmethod lang-deps :after ((self (eql ,keyword)) source)
               (declare (ignore source))
               (depends-on ',script))))))))

(defun loader-language-table (val)
  (lambda (module key)
    (if (eql key :default) val
        (error 'no-such-export
               :key key
               :module module))))

(defun load-fasl-lang (lang source)
  (let ((object-file (faslize lang source)))
    (restart-case
        (load-as-module object-file)
      (recompile-object-file ()
        :report "Recompile the object file."
        (delete-file-if-exists object-file)
        (build (module-spec lang source))
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

;;; This can't use `defpattern', for bootstrapping reasons.

(defclass fasl-lang-pattern (pattern)
  ((lang :initarg :lang
         :initform (required-argument :lang))
   (source :initarg :source)))

(defmethods fasl-lang-pattern (self lang source)
  (:method pattern.output-defaults (self)
    (faslize lang source))

  (:method pattern-build (self)
    (let* ((*source* *input*)
           (lang (lang-name lang))
           (*language* lang)
           ;; Must be bound here for macros that intern
           ;; symbols.
           (*package* (user-package (resolve-package lang))))
      (depends-on *source*)
      (compile-to-file
       (wrap-current-module
        (expand-module lang *input*)
        lang *input*)
       (ensure-directories-exist *output*)
       :top-level (package-compile-top-level? lang)
       :source *source*)))

  (:method print-pattern-ref (self (ref pattern-ref) stream)
    (with-slots ((input name)) ref
      (format stream "~a~s"
              (read-eval-prefix self stream)
              `(pattern-ref (fasl-lang-pattern
                             ,(lang-name lang)
                             ,source)
                            ,input)))))

(defun fasl-lang-pattern (lang source)
  (make 'fasl-lang-pattern :lang lang :source source))

(defun fasl-lang-pattern-ref (lang source)
  (pattern-ref (fasl-lang-pattern lang source) source))

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

(defun ensure-lang-exists (lang &optional (cont #'ensure-lang-exists))
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


;;; Importing.

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
           (receive (exports exports?)
               (module-static-exports lang source)
             (if exports? exports
                 (module-dynamic-exports lang source)))))
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

(def module-name-lock (bt:make-recursive-lock "Module name lock"))

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

(defun resolve-import-spec
    (&key lang source bindings values module (base (base)) env prefix)
  (check-type base absolute-pathname)
  (check-type prefix string-designator)
  (mvlet* ((lang source (lang+source lang source module base env))
           (bindings values (bindings+values bindings values
                                             :lang lang
                                             :source source
                                             :prefix prefix)))
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
  (flet ((expand (spec)
           (~> (expand-binding-spec spec lang source)
               canonicalize-bindings
               (apply-prefix prefix))))
    (let ((bindings (expand bindings))
          (values (expand values)))
      (if *always-import-values*
          ;; Macros cannot be imported as values.
          (let* ((macros (filter (of-type 'macro-alias) bindings))
                 (bindings (set-difference bindings macros)))
            (values macros (append bindings values)))
          (values bindings values)))))

(defmacro check-static-bindings-now (lang source bindings)
  "Wrapper around check-static-bindings to force evaluation at compile time.
Can't use eval-when because it has to work for local bindings."
  (check-static-bindings lang source bindings))

(defcondition binding-export-mismatch (overlord-error)
  ((source :initarg :source)
   (bindings :initarg :bindings :type list)
   (exports :initarg :exports :type list))
  (:report (lambda (c s)
             (with-slots (bindings exports source) c
               (format s "Requested bindings do not match exports.~%Source: ~a~%Bindings: ~s~%Exports: ~s"
                       source bindings exports)))))

(defun check-static-bindings (lang source bindings)
  "Check that BINDINGS is free of duplicates. Also, using
`module-static-exports', check that all of the symbols being bound are
actually exported by the module specified by LANG and SOURCE."
  (ensure-lang-exists lang)
  (when bindings
    (check-static-bindings-1
     (ensure-lang-exists lang)
     (if (relative-pathname-p source)
         (merge-pathnames* source (base))
         source)
     (mapcar (op (import-keyword (first _)))
             (canonicalize-bindings bindings)))))

(defun check-exports (source bindings exports)
  "Make sure the bindings are a subset of the exports."
  (unless (subsetp bindings exports :test #'string=)
    (error 'binding-export-mismatch
           :source source
           :bindings bindings
           :exports exports)))

(defun check-static-bindings-1 (lang source bindings)
  (check-type lang keyword)
  (check-type source absolute-pathname)
  ;; (check-type bindings (satisfies setp))
  (unless (setp bindings)
    (error* "Duplicated bindings in ~a" bindings))
  (receive (static-exports exports-statically-known?)
      (module-static-exports lang source)
    (if exports-statically-known?
        (check-exports source bindings static-exports)
        (restart-case
            (let ((exports (module-dynamic-exports lang source)))
              (check-exports source bindings exports))
          (recompile-object-file ()
            :report "Recompile the object file."
            (let ((object-file (faslize lang source))
                  (target (module-spec lang source)))
              (delete-file-if-exists object-file)
              (build target)
              (check-static-bindings lang source bindings)))))))

(defmacro declaim-module (as from)
  `(propagate-side-effect
     (ensure-target-recorded
      (module-spec ,as ,from))))

(defmacro import-module (module &key as from)
  (check-type module var-alias)
  (let ((req-form `(require-as ',as ,from)))
    `(progn
       (overlord/shadows:def ,module ,req-form)
       (declaim-module ,as ,from)
       ',module)))

(defmacro import-module/lazy (module &key as from)
  (check-type module var-alias)
  (let ((lazy-load `(load-module/lazy ',as ,from)))
    `(progn
       (overlord/shadows:define-symbol-macro ,module ,lazy-load)
       (declaim-module ,as ,from)
       ',module)))

(defmacro import-default (var &body (&key as from))
  (let ((module-name (symbolicate '__module-for- var)))
    `(import ,module-name
       :as ,as
       :from ,from
       :binding ((:default :as ,var)))))

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
         ;; NB Core Lisp binds `args' as a symbol macro, and SBCL,
         ;; stickler that it is, objects to dynamic-extent
         ;; declarations for symbol macros.
         (if (equal exp ref)
             `(progn
                (declaim (notinline ,alias))
                (overlord/shadows:defun ,alias (&rest args)
                  #-sbcl (declare (dynamic-extent args))
                  (apply ,ref args)))
             `(progn
                (overlord/shadows:defalias ,alias
                  (function-wrapper
                   (lambda (&rest args)
                    #-sbcl (declare (dynamic-extent args))
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
            (assure function (function-wrapper ,ref)))))
      (macro-alias
       (error 'macro-as-value :name (second alias))))))

(defun import+alias+ref (clause module)
  (destructuring-bind (import alias) (canonicalize-binding clause)
    (let* ((key (import-keyword import))
           (ref
             (etypecase-of import-alias alias
               (var-alias `(module-ref* ,module ',key))
               ((or function-alias macro-alias)
                `(module-fn-ref ,module ',key)))))
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

(defmacro with-import-default ((bind &key from as) &body body)
  (with-unique-names (mod)
    `(with-imports (,mod
                    :from ,from
                    :as ,as
                    :binding ((:default :as ,bind)))
       ,@body)))

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
