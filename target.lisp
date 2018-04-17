(defpackage :overlord/target
  (:use
    :cl
    :alexandria
    :serapeum
    :local-time

    :uiop/filesystem
    :uiop/pathname

    ;; The build logic.
    :overlord/redo
    ;; What we need to implement.
    :overlord/target-protocol
    ;; Timestamps.
    :overlord/stamp
    ;; Digests.
    :overlord/digest
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
    ;; The disk cache.
    :overlord/cache
    ;; ASDF interface.
    :overlord/asdf
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
   :defmethod                           ;Require a generic function
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
   :dynamic-require-as :dynamic-require-default :require-once

   ;; Module protocol.
   :module-meta

   :reintern :reinterning
   :*file-local-variables*
   :find-module

   ;; Emacs integration.
   :require-for-emacs
   :expand-module-for-emacs

   :find-pattern
   :build
   :build-package
   :run

   :depends-on
   :depends-on*
   :pdepends-on
   :depends-on-all
   :depends-on-all*
   :pdepends-on-all
   :depends-not
   :depends-not-all
   :use :use-all

   :with-script

   :module-dynamic-exports
   :faslize
   :ensure-lang-exists
   :guess-source
   :guess-lang+pos
   :resolve-lang
   :module-spec
   :ensure-target-recorded))

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

(defun check-generic-function (method-name)
  (unless (and (fboundp method-name)
               (typep (fdefinition method-name)
                      'generic-function))
    (error "No generic function for ~a" method-name)))

(defmacro defmethod (name &body body)
  `(progn
     (check-generic-function ',name)
     (cl:defmethod ,name ,@body)))


;;; Auxiliary functions for Redo.

(defconst nonexist       :nonexist)
(defconst prereqs        :prereqs)
(defconst prereqs-temp   :prereqs-temp)
(defconst prereqsne      :prereqsne)
(defconst prereqsne-temp :prereqsne-temp)
(defconst stamp          :stamp)
(defconst uptodate       :uptodate)

(defun maybe-delay-symbol (symbol)
  (cond ((not (symbolp symbol))
         symbol)
        ((built-in-symbol? symbol)
         symbol)
        (t (delay-symbol symbol))))

(defun built-in-symbol? (symbol)
  (and (symbolp symbol)
       (or (keywordp symbol)
           (cl-symbol-p symbol)
           (eql (symbol-package symbol)
                #.*package*))))

(defun saved-prereq (x &optional (stamp (target-stamp x)))
  (cons x (assure stamp stamp)))

(defmethod saved-prereq-target (p) (car p))
(defmethod saved-prereq-stamp (p) (cdr p))

(defplace temp-prereqs (target)
  (prop target prereqs-temp (fset:empty-map)))

(defplace temp-prereqsne (target)
  (prop target prereqsne-temp (fset:empty-set)))

(defun current-parent ()
  (or (first *parents*)
      *package*))

(defmethod record-prereq (target &aux (parent (current-parent)))
  (record-parent-prereq parent target))

(defmethod record-prereq ((target trivial-prereq))
  (declare (ignore target)))

(defmethod record-prereq ((target symbol))
  (record-prereq (maybe-delay-symbol target)))

(defgeneric record-parent-prereq (parent target)
  (:method ((parent root-target) target)
    (declare (ignore target)))
  (:method ((parent package) target)
    (declare (ignore target)))
  (:method (parent target)
    (withf (temp-prereqs parent)
           target
           (target-stamp target))))

(defmethod record-prereqne (target &aux (parent (current-parent)))
  (record-parent-prereqne parent target))

(defmethod record-prereqne ((target impossible-prereq))
  (declare (ignore target)))

(defmethod record-prereqne ((target symbol))
  (record-prereqne (maybe-delay-symbol target)))

(defun record-parent-prereqne (parent target)
  (withf (temp-prereqsne parent) target))

(defmethod target-in-db? (target)
  (has-prop? target
             ;; The uptodate key is sort of a fallback for a target
             ;; that, for whatever reason, has no prerequisites. Such
             ;; a target would be built once, and then never again.
             uptodate
             prereqs
             prereqs-temp
             prereqsne
             prereqsne-temp))

(defmethod target-in-db? ((target root-target)) t)
(defmethod target-in-db? ((target package)) t)
(defmethod target-in-db? ((target impossible-prereq)) t)
(defmethod target-in-db? ((target trivial-prereq)) t)

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
    (and (typep set 'fset:set)
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
  ()
  (:documentation "A class that can be externalized. Subclasses
inherit a method on `make-load-form', and need only specialize
`load-form-slot-names' (using the `append' method combination)."))

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

  (:method load-form-slot-names append (self)
    '(name))

  (:method fset:compare (self (other ref))
    (fset:compare-slots self other #'class-name-of #'ref.name))

  (:method hash-target (self)
    (dx-sxhash
     (list (class-name-of self)
           (ref.name self)))))

(fset:define-cross-type-compare-methods ref)

(defclass directory-ref (ref)
  ((name
    :initarg :path
    :reader directory-ref.path
    :type (and absolute-pathname directory-pathname)))
  (:documentation "A reference to a directory."))

(defun directory-ref (name)
  "Wrap NAME as a directory reference."
  (etypecase-of (or string directory-pathname) name
    (string (directory-ref (ensure-pathname name :want-pathname t)))
    (directory-pathname (make 'directory-ref :path name))))

(defmethods directory-ref (target (path name))
  (:method target-exists? (target)
    (~> path
        (resolve-target)
        directory-exists-p))
  (:method target-timestamp (target)
    (let* ((dir path)
           (dir (resolve-target dir)))
      (if (directory-exists-p dir)
          far-future
          never)))
  (:method (setf target-timestamp) (timestamp target)
    (declare (ignore timestamp))
    (if (directory-exists-p path)
        ;; TODO Ditto.
        (error* "Cannot set directory timestamps (yet).")
        (ensure-directories-exist path)))
  (:method resolve-target (target &optional base)
    (if (absolute-pathname-p path)
        target
        (directory-ref
         (assure tame-pathname
           (~> target
               path
               (merge-pathnames* (or base (base))))))))
  (:method target= (target (y directory-ref))
    (pathname-equal path (directory-ref.path y)))
  (:method target-build-script (target)
    (let ((dir path))
      (task target
            (lambda ()
              (let ((dir (resolve-target dir)))
                (ensure-directories-exist dir)))
            trivial-prereq)))
  (:method target-node-label (target)
    (fmt "directory ~a" path))
  (:method hash-target (target)
    (dx-sxhash (list 'directory-ref path))))

(defclass file-digest-ref (ref)
  ((name :type pathname
         :initarg :file
         :reader file-digest-ref.file)))

(defun file-digest-ref (file)
  (make 'file-digest-ref :file (ensure-pathname file)))

(defmethods file-digest-ref (target (file name))
  (:method target-stamp (target)
    (file-stamp/hash file))

  (:method target-exists? (target)
    (file-exists-p file))
  (:method target= (target (other file-digest-ref))
    (pathname-equal file (file-digest-ref.file other)))
  (:method hash-target (target)
    (dx-sxhash (list 'file-digest-ref file)))
  (:method resolve-target (target &optional base)
    (if (absolute-pathname-p file) target
        (make 'file-digest-ref
              :file (merge-pathnames* file (or base (base))))))

  (:method target-timestamp (target)
    (target-timestamp file))
  (:method (setf target-timestamp) (value target)
    (setf (target-timestamp file) value))
  (:method target-build-script (target)
    (target-build-script file))
  (:method target-node-label (target)
    (target-node-label file)))

(defclass pattern-ref (ref)
  ;; Note that the pattern slot has a silly type: a pattern ref can be
  ;; either a symbol or an instance of `pattern', which is not yet
  ;; defined. Being able to directly pass in patterns will be useful
  ;; later when we bootstrap support for languages.
  ((pattern
    :initarg :pattern
    :type (or symbol standard-object delayed-symbol)
    :accessor pattern-ref.pattern)
   (name
    :type pathname
    :initarg :input
    :accessor pattern-ref.input)
   (output
    :type pathname
    :reader pattern-ref.output)))

;;; Re. merge-*-defaults. Originally I was planning on a DSL, but
;;; pathnames seems to work, as long as we're careful about how we
;;; merge them. The order is important. We merge the *provided* inputs
;;; and outputs into the *defaults*, rather than vice-versa. The
;;; choice of merging algorithm is also important. For
;;; merge-input-defaults, we want to preserve the host of the provided
;;; input, so we use uiop:merge-pathnames*. But for
;;; merge-output-defaults, we want to be able to redirect to the
;;; output to a different host, so we use good old cl:merge-pathnames.

(defgeneric merge-input-defaults (pattern input)
  (:method (pattern input)
    (merge-pathnames* (pattern.input-defaults pattern)
                      input)))

(defgeneric merge-output-defaults (pattern input)
  (:method (pattern input)
    (merge-pathnames (pattern.output-defaults pattern)
                     input)))

(defun print-pattern-ref (pattern ref stream)
  (let* ((input (pattern-ref.input ref))
         (pattern (find-pattern pattern))
         (pattern-name (pattern-name pattern)))
    (if *print-escape*
        (format stream "~a~s"
                (read-eval-prefix ref stream)
                `(pattern-ref ,(maybe-delay-symbol pattern-name)
                              ,input))
        (print-unreadable-object (ref stream :type t)
          (format stream "~a (~a)"
                  pattern-name
                  input)))))

(defmethods pattern-ref (self (input name) output pattern)
  (:method initialize-instance :after (self &key)
    (unless (absolute-pathname-p input)
      (let* ((pattern (find-pattern pattern))
             (abs-input (merge-input-defaults pattern input)))
        (setf input abs-input))))

  (:method print-object (self stream)
    (print-pattern-ref pattern self stream))

  (:method slot-unbound (class self (slot-name (eql 'output)))
    (declare (ignore class))
    ;; Since this is idempotent I see no reason to lock.
    (let* ((pattern (find-pattern pattern))
           (abs-output (merge-output-defaults pattern input)))
      (setf output abs-output)))

  (:method load-form-slot-names append (self)
    '(pattern output))

  (:method fset:compare (self (other pattern-ref))
    (fset:compare-slots self other
                        #'pattern-ref.input
                        #'pattern-ref.pattern)))

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

(defconstructor phony-target
  (name symbol))

(defmethod fset:compare ((x phony-target) (y phony-target))
  (fset:compare-slots x y #'phony-target-name))

(fset:define-cross-type-compare-methods phony-target)

;;; NB Figure out whether this actually replaces all possible uses of
;;; ifcreate. (It replaces the original use case, resolving files, but
;;; not all possible use cases.)

(defgeneric relative-file-truename (target))

(defclass relative-file-target (externalizable)
  ((path
    :type relative-pathname
    :initarg :path)))

(defmethods relative-file-target (self path)
  (:method load-form-slot-names append (self)
    '(path))
  (:method target-stamp (self)
    (let ((truename (relative-file-truename self)))
      (if (not (file-exists-p truename))
          never
          (resolved-file truename
                         (target-stamp truename)))))
  (:method target-exists? (self)
    (file-exists-p (relative-file-truename self)))
  (:method target= (self (other relative-file-target))
    (equal (relative-file-truename self)
           (relative-file-truename other)))
  (:method target-build-script (self)
    (target-build-script (relative-file-truename self))))

(defclass system-resource (relative-file-target)
  ((system
    :type string
    :initarg :system
    :reader system-resource.system))
  (:documentation "Depend on a system-relative resource.

The natural (Redo-ish) thing to do here would be to depend directly on
the resolved file, but also add non-existent prerequisites for any
possible path to $system.asd that might, in the future, supersede its
current location.

However, the rules ASDF uses to resolve system locations (see §8.1 in
the ASDF manual) are far too flexible for the enumeration of possible
locations to be practical.

Since exhaustive enumeration is out, we use a different approach. We
use a special kind of stamp that stores the resolved pathname. If the
newly resolved pathname agrees with the stored one, then the metadata
from that file is used. But if they disagree, then the dependency is
treated as out-of-date, regardless of file metadata."))

(defmethods system-resource (self system path)
  (:method print-object (self stream)
    (if (not *print-escape*)
        (print-unreadable-object (self stream :type t)
          (format t "~a ~a" system path))
        (progn
          (write-string (read-eval-prefix self stream) stream)
          (format stream "~s"
                  `(system-resource ,system ,path)))))
  (:method relative-file-truename (self)
    (asdf:system-relative-pathname
     (system-resource.system self)
     path))
  (:method hash-target (self)
    (dx-sxhash (list 'system-resource system path)))
  (:method target-node-label (self)
    (fmt "resource ~a in system ~a" path system)))

(defun system-resource (system path)
  (make 'system-resource
        :system (string-downcase system)
        :path (assure relative-pathname
                (ensure-pathname path :want-pathname t))))

(deftype target ()
  ;; NB Not allowing lists of targets as targets is a conscious
  ;; decision. It would make things much more complicated. In
  ;; particular, there would no longer be a single timestamp for a
  ;; target, because the proper timestamp to use for a list of targets
  ;; would depend on whether it was being depended on (in which case
  ;; we want the /newest/ timestamp) or doing the depending (in which
  ;; case we want the /oldest/ timestamp).
  '(not list))

(defconstructor task
  "A task."
  (target target)
  (thunk function)
  (script target))

(defmethod target-build-script :around ((target t))
  (check-not-frozen)
  (assure task
    (call-next-method)))


;;; Manipulating targets.

(define-global-state *symbol-timestamps* (make-hash-table :size 1024))
(declaim (type hash-table *symbol-timestamps*))

(defun pathname-exists? (path)
  (etypecase-of (or string pathname) path
    (string (pathname-exists? (ensure-pathname path :want-pathname t)))
    (pathname
     (or (file-exists-p path)
         (directory-exists-p path)))))

(defmethod target-exists? ((target root-target))
  t)

(defmethod target-exists? ((target package))
  t)

(defmethod target-exists? ((target trivial-prereq))
  t)

(defmethod target-exists? ((target impossible-prereq))
  nil)

(defmethod target-exists? ((target phony-target))
  nil)

(defmethod target-exists? ((target symbol))
  (boundp target))

(defmethod target-exists? ((target delayed-symbol))
  (target-exists? (force-symbol target)))

(defmethod target-exists? ((target cl:pathname))
  (pathname-exists? (resolve-target target)))

(defmethod target-exists? ((target pattern-ref))
  (~> target
      pattern-ref.output
      pathname-exists?))

(defmethod target-exists? ((target module-spec))
  (~> target
      module-spec-cell
      module-cell.module))

(defmethod target-timestamp ((target root-target))
  never)

(defmethod target-timestamp ((target package))
  never)

(defmethod target-timestamp ((target impossible-prereq))
  never)

(defmethod target-timestamp ((target phony-target))
  never)

(defmethod target-timestamp ((target trivial-prereq))
  far-future)

(defmethod target-timestamp ((target symbol))
  (if (boundp target)
      (let ((now (now)))
        (ensure2 (gethash target *symbol-timestamps*)
          now))
      never))

(defmethod target-timestamp ((target delayed-symbol))
  (target-timestamp (force-symbol target)))

(defmethod target-timestamp ((target cl:pathname))
  (if (pathname-exists? target)
      (file-mtime target)
      never))

(defmethod target-timestamp ((target pattern-ref))
  (with-accessors ((output pattern-ref.output)) target
    (if (pathname-exists? output)
        (file-mtime output)
        never)))

(defmethod target-timestamp ((target module-spec))
  (let* ((cell (module-spec-cell target)))
    (with-slots (module timestamp) cell
      (if (null module) never timestamp))))

(defmethod (setf target-timestamp) :before (timestamp target)
  (declare (ignore target))
  (check-type timestamp target-timestamp)
  (check-not-frozen))

(defmethod (setf target-timestamp) (timestamp target)
  (declare (ignore timestamp))
  (error* "Cannot set timestamp for ~a" target))

(defmethod (setf target-timestamp) (timestamp (target delayed-symbol))
  (let ((target (force-symbol target)))
    (setf (target-timestamp target) timestamp)))

(defmethod (setf target-timestamp) (timestamp (target symbol))
  ;; Configurations need to set the timestamp while unbound
  #+(or) (unless (boundp target)
           (error* "Trying to set timestamp for unbound symbol ~s"
                   target))
  (setf (gethash target *symbol-timestamps*) timestamp))

(defmethod (setf target-timestamp) (timestamp (target cl:pathname))
  (declare (ignore timestamp))
  (if (pathname-exists? target)
      ;; TODO There must be some portable way to do this.
      (error* "Cannot set pathname timestamps (yet).")
      (open target :direction :probe :if-does-not-exist :create)))

(defmethod (setf target-timestamp) (timestamp (target module-spec))
  (let ((cell (module-spec-cell target)))
    (setf (module-cell.timestamp cell) timestamp)))

(defun touch-target (target &optional (date (now)))
  (setf (target-timestamp target) date))

(defun delete-file-or-directory (p)
  (if (directory-pathname-p p)
      (delete-directory-tree p)
      (delete-file-if-exists p)))

(defmethod resolve-target ((target cl:pathname) &optional base)
  (if (absolute-pathname-p target) target
      (let ((path (merge-pathnames* target (or base (base)))))
        (if (wild-pathname-p path)
            (directory* path)
            path))))

(defmethod resolve-target ((target pattern-ref) &optional base)
  (let ((input (pattern-ref.input target)))
    (if (absolute-pathname-p input) target
        (pattern-ref (pattern-ref.pattern target)
                     (merge-pathnames* input
                                       (or base (base)))))))

(defmethod resolve-target ((target module-spec) &optional base)
  (let-match1 (module-spec lang source) target
    (if (absolute-pathname-p source) target
        (module-spec lang
                     (assure tame-pathname
                       (merge-pathnames* source
                                         (or base (base))))))))


;;; Target table abstract data type.

(defmethod target= ((x phony-target) (y phony-target))
  (eql (phony-target-name x)
       (phony-target-name y)))

(defmethod target= ((x delayed-symbol) y)
  (target= (force-symbol x)
           (force-symbol y)))

(defmethod target= ((x cl:pathname) (y cl:pathname))
  (pathname-equal x y))

(defmethod target= ((x module-spec) (y module-spec))
  (multiple-value-match (values x y)
    (((module-spec lang1 path1)
      (module-spec lang2 path2))
     (and (eql lang1 lang2)
          (pathname-equal path1 path2)))))

(defmethod target= ((x pattern-ref) (y pattern-ref))
  (and (equal (pattern-ref.input x)
              (pattern-ref.input y))
       (eql (pattern-ref.pattern x)
            (pattern-ref.pattern y))))

(defmethod hash-target ((target root-target))
  (load-time-value (sxhash root-target)))

(defmethod hash-target ((target trivial-prereq))
  (load-time-value (sxhash trivial-prereq)))

(defmethod hash-target ((target impossible-prereq))
  (load-time-value (sxhash impossible-prereq)))

(defmethod hash-target ((target module-spec))
  (let-match1 (module-spec lang path) target
    (dx-sxhash (list 'module-spec lang path))))

(defmethod hash-target ((target pattern-ref))
  (dx-sxhash
   (list 'package-ref
         (ref.name target))))

(defmethod hash-target ((target phony-target))
  (let ((name (phony-target-name target)))
    (dx-sxhash `(phony ,name))))

(defgeneric hash-friendly? (target)
  (:method ((x root-target)) t)
  (:method ((x package)) t)
  (:method ((x impossible-prereq)) t)
  (:method ((x trivial-prereq)) t)
  (:method ((x symbol)) t)
  (:method ((x cl:pathname)) t)
  (:method (x)
    (declare (ignore x))
    nil))

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

(-> make-target-table
    (&key (:size (integer 0 *)) (:synchronized t))
    target-table)
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

(-> target-table-len (target-table) array-length)
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

(-> target-table-ref (target-table t) (values t boolean))
(defun target-table-ref (table key)
  (with-target-table-locked (table)
    (if (hash-friendly? key)
        (let ((hash (target-table.hash-table table)))
          (gethash key hash))
        (fset:lookup (target-table.map table) key))))

(-> (setf target-table-ref) (t target-table t) t)
(defun (setf target-table-ref) (value table key)
  (prog1 value
    (with-target-table-locked (table)
      (if (hash-friendly? key)
          (let ((hash (target-table.hash-table table)))
            (setf (gethash key hash) value))
          (callf #'fset:with (target-table.map table) key value)))))

(-> target-table-rem (target-table t) null)
(defun target-table-rem (table key)
  (prog1 nil
    (with-target-table-locked (table)
      (if (hash-friendly? key)
          (let ((hash (target-table.hash-table table)))
            (remhash key hash))
          (callf #'fset:less (target-table.map table) key)))))

(-> target-table-member (target-table t) boolean)
(defun target-table-member (table key)
  (nth-value 1
    (target-table-ref table key)))

(-> (setf target-table-member) (t target-table t) boolean)
(defun (setf target-table-member) (value table key)
  (prog1 (true value)
    (if value
        (with-target-table-locked (table)
          (unless (target-table-member table key)
            (setf (target-table-ref table key) t)))
        (target-table-rem table key))))

(-> target-table-keys (target-table) list)
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

(-> clear-target-table (target-table) (values))
(defun clear-target-table (table)
  (with-target-table-locked (table)
    (clrhash (target-table.hash-table table))
    (setf (target-table.map table)
          (fset:empty-map)))
  (values))

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

;;; NB `*tasks*' and `*top-level-targets* cannot be safely reset,
;;; since they contain information that could only be obtained by
;;; reloading all Lisp files.
(defvar *tasks* (dict))
(declaim (type hash-table *tasks*))

(def top-level-target-lock
  (bt:make-recursive-lock))

;;; Which targets belong to which packages. This is bidirectional: a
;;; target can only belong to one package at a time.

(defvar *top-level-targets* (make-target-table))
(declaim (type target-table *top-level-targets*))

(defvar *prereq-packages* (make-target-table))
(declaim (type target-table *prereq-packages*))

(defun package-prereqs-table (package)
  (let ((package (find-package package))
        (table *top-level-targets*))
    (synchronized (top-level-target-lock)
      (ensure2 (target-table-ref table package)
        (make-target-table)))))

(defun target-package (target)
  (let ((table *prereq-packages*))
    (assure package
      (synchronized (top-level-target-lock)
        (target-table-ref table target)))))

(defun (setf target-package) (package target)
  (let ((table *prereq-packages*)
        (new (find-package package)))
    (synchronized (top-level-target-lock)
      (let ((old (shiftf (target-table-ref table target)
                         new)))
        (when (packagep old)
          (let ((old-table (package-prereqs-table old)))
            (nix (target-table-member old-table target))))
        (let ((new-table (package-prereqs-table new)))
          (setf (target-table-member new-table target) t))))))

(defun list-package-prereqs (package)
  (~> package
      package-prereqs-table
      target-table-keys))

(defun record-package-prereq (package target)
  (setf (target-package target) package))

(defun ensure-target-recorded (target)
  (if *parents*
      (record-prereq target)
      (record-package-prereq *package* target)))

(defmethod target-saved-prereqs ((rt root-target))
  (mapcar (op (saved-prereq _1 (target-stamp _1)))
          (list-top-level-targets)))

(defmethod target-saved-prereqs ((pkg package))
  (mapcar (op (saved-prereq _1 (target-stamp _1)))
          (list-package-prereqs pkg)))

(defun trivial-task (target)
  (task target
        (constantly nil)
        trivial-prereq))

(defun impossible-task (target)
  (task target
        (constantly nil)
        impossible-prereq))

(defmethod target-build-script ((target t))
  (impossible-task target))

(defmethod target-build-script ((target trivial-prereq))
  (trivial-task target))

(defmethod target-build-script ((target impossible-prereq))
  (trivial-task target))

(defmethod target-build-script ((target cl:pathname))
  (let ((target (resolve-target target)))
    (or (gethash target *tasks*)
        (impossible-task target))))

(defmethod target-build-script ((target symbol))
  (or (gethash target *tasks*)
      ;; If there is no real target by that name, look for a
      ;; phony target.
      (target-build-script
       (phony-target target))))

(defmethod target-build-script ((target phony-target))
  (let* ((name (phony-target-name target))
         (key `(phony ,name)))
    (or (gethash key *tasks*)
        (impossible-task target))))

(defmethod target-build-script ((target root-target))
  (task target
        (lambda ()
          ;; NB. Note that we do not get the prereqs of the root
          ;; target from the database. We do not want them to be
          ;; persistent; we only want to build the targets that
          ;; have been defined in this image.
          (let ((*suppress-phonies* t))
            (depends-on-all (list-all-packages))))
        trivial-prereq))

(defmethod target-build-script ((target package))
  (task target
        (lambda ()
          ;; NB. Note that we do not get the prereqs of the root
          ;; target from the database. We do not want them to be
          ;; persistent; we only want to build the targets that
          ;; have been defined in this image.
          (let ((*suppress-phonies* t))
            (depends-on-all (list-package-prereqs target))))
        trivial-prereq))

(defmethod target-build-script ((target pattern-ref))
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

(defmethod target-build-script ((target module-spec))
  (let ((cell (module-spec-cell target)))
    (with-slots (lang source) cell
      (task target
            (lambda ()
              ;; Prevent the old module object from persisting if
              ;; there is a problem building the new one.
              (unload-module-cell cell)
              (let ((*base* (pathname-directory-pathname source)))
                ;; Depend on the source file.
                (depends-on source)
                ;; Let the language tell you what to depend on.
                (lang-deps lang source))

              (let ((*language* lang))
                (load-module-into-cell cell)))
            trivial-prereq))))

(defmethod build-script-target ((script task))
  (task-script script))

(defmethod run-script (task &aux (parent (current-parent)))
  (check-not-frozen)
  ;; XXX exhaustive?
  (unless (typep parent
                 '(or impossible-prereq trivial-prereq))
    (print-target-being-built parent))
  (funcall (task-thunk task)))

(defun run-save-task (target thunk &optional (script (script-for target)))
  (check-not-frozen)
  (save-task target thunk script)
  (depends-on target))

(defgeneric save-task* (target thunk script)
  (:method :before (target thunk script)
    (declare (ignore target thunk script))
    (check-not-frozen))
  (:method (target thunk script)
    (declare (ignore thunk script))
    (error* "Task for ~a cannot be redefined." target))
  (:method ((target symbol) thunk script)
    (setf (gethash target *tasks*)
          (task target thunk script)))
  (:method ((target cl:pathname) thunk script)
    (setf (gethash target *tasks*)
          (task target thunk script)))
  (:method ((target delayed-symbol) thunk script)
    (save-task (force-symbol target) thunk script))
  (:method ((target phony-target) thunk script)
    (let* ((name (phony-target-name target))
           (task (task target thunk script))
           (key `(phony ,name)))
      (setf (gethash key *tasks*) task))))

(defun save-task (target thunk &optional (script (script-for target)))
  (check-not-frozen)
  (save-task* target thunk script))

(defun task-values (task)
  (values (task-target task)
          (task-thunk task)
          (task-script task)))

(defun print-target-being-built (target)
  "Print some information about the target being built."
  (let* ((depth (max 0 (1- (length *parents*))))
         (spaces (make-string depth :initial-element #\Space)))
    (message "~aBuilding ~a"
             spaces
             (target-node-label target))))

(defmethod target-node-label ((target cl:pathname))
  (native-namestring target))

(defmethod target-node-label ((target symbol))
  (if (string$= '.do target)
      (fmt "script for '~s"
           (find-symbol
            (slice (symbol-name target) 0 -3)
            (symbol-package target)))
      (fmt "'~s" target)))

(defmethod target-node-label ((target delayed-symbol))
  (target-node-label (force-symbol target)))

(defmethod target-node-label ((target root-target))
  (progn "everything"))

(defmethod target-node-label ((target package))
  (fmt "package ~a" target))

;; Shouldn't happen
(defmethod target-node-label ((target trivial-prereq))
  (progn "TRIVIAL TARGET"))

;;; Shouldn't happen either.
(defmethod target-node-label ((target impossible-prereq))
  (progn "IMPOSSIBLE TARGET"))

(defmethod target-node-label ((target module-spec))
  (let-match1 (module-spec lang path) target
    (let ((path (native-namestring path)))
      (fmt "~a (#lang ~a)"
           path (string-downcase lang)))))

(defmethod target-node-label ((target phony-target))
  (let ((name (phony-target-name target)))
    (fmt "phony target ~a" name)))

(defmethod target-node-label ((target pattern-ref))
  (native-namestring
   (pattern-ref.output target)))

(defun file-stamp (file)
  (let ((size (file-size-in-octets file))
        (timestamp (target-timestamp file)))
    (file-meta size timestamp)))

(defun file-stamp/hash (file)
  (let* ((file (ensure-pathname file))
         (size (file-size-in-octets file))
         (hash (byte-array-to-hex-string (digest-file file))))
    (file-hash size hash)))

(defmethod target-stamp ((target cl:pathname))
  (cond ((file-exists-p target)
         (file-stamp target))
        ((directory-pathname-p target)
         (target-timestamp target))
        (t never)))

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
  (check-type file cl:pathname)
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
    (unless (asdf-system-loaded? system-name)
      (restart-case
          (load-asdf-system system-name)
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

(defun build-package (package)
  (build (find-package package)))

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

(defun use-all* (targets)
  "Depend on each target in TARGET -- as a normal prereq if TARGET
exists, and as a non-existent prereq if TARGET does not exist."
  (do-each (target targets targets)
    (if (target-exists? target)
        (redo-ifchange target)
        (redo-ifcreate target))))

(defun use-all (targets)
  "Like `use-all*', but targets are shuffled."
  (use-all* (reshuffle targets)))

(defun use (&rest targets)
  "Like `use-all', but variadic."
  (use-all targets))

(defun use* (&rest targets)
  "Like `use', but ordered."
  (use-all* targets))

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
              (:file-digest (file)
                `(file-digest-ref (path ,file)))
              (:directory-exists (name)
                `(directory-ref ,name))
              (:pattern (name input)
                `(pattern-ref ,name ,input))
              (:module (lang source)
                ;; Is (base) right?
                `(module-spec ',lang
                              (resolve-target (ensure-pathname ,source)
                                              ,(base))))
              (:system-resource (system path)
                `(system-resource ,system ,path))

              ;; Depending on specific things.
              (:env (name)
                `(env-oracle ,name))
              (:var (name)
                `(var-oracle ,name))
              (:feature (name)
                `(feature-oracle ,name))
              (:use (&rest targets)
                `(use ,@targets))
              (:use* (&rest targets)
                `(use* ,@targets))
              (:use-all (targets)
                `(use-all ,targets))
              (:use-all* (targets)
                `(use-all* ,targets))

              (:always (&optional (bool t))
                `(and ,bool (redo-always)))

              (:system-version (system-name)
                `(system-version-oracle ,system-name))
              (:dist-version (&optional (dist nil dist?))
                (if dist?
                    `(dist-version-oracle ,dist)
                    `(dist-version-oracle)))

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

(defmacro defconfig (name init &body body)
  "BODY can be either a string (a docstring) or keywords."
  (check-type name symbol)
  (mvlet* ((test documentation
             (ematch body
               ((list (and docstring (type string)))
                (values '#'equal docstring))
               ((trivia:lambda-list &key (test '#'equal) documentation)
                (values test documentation))))
           (init
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
         (save-task ',name (constantly ,init) trivial-prereq))
       (eval-always
         (update-config-if-changed ',name ,init ,test))
       ',name)))

(defmacro script-thunk (&body body)
  `(lambda ()
     ,(save-base
       `(with-script ()
          ,@body))))

(defvar *scripts* (make-hash-table)
  "Set of registered scripts.")

(defun register-script (name)
  "Register NAME as a script."
  (setf (gethash (assure symbol name) *scripts*) t))

(defun clear-scripts ()
  "Nix all symbols registered as script."
  (let ((scripts (shiftf *scripts* (make-hash-table))))
    (do-hash-table (k v scripts)
      (declare (ignore v))
      (nix (symbol-value k)))))

(add-hook '*before-hard-freeze-hook* 'clear-scripts)

(defmacro define-script (name &body script)
  `(progn
     (register-script ',name)
     (defconfig ,name ',script
       :test #'source=)))

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
                  (unless *suppress-phonies*
                    (funcall (script-thunk ,@script))))
                (script-for ',name))
     ',name))


;;; File targets.

(defun file-target-form (pathname script-form args)
  (ematch args
    ((list)
     script-form)
    ((list $1)
     `(let ((,$1 ,pathname))
        ,script-form))
    ((list $1 $3)
     `(call/temp-file-pathname
       ,pathname
       (lambda (,$3)
         (let ((,$1 ,pathname))
           ,script-form))))))

(defmacro file-target (name pathname (&optional (in nil in?) (temp nil temp?)) &body script)
  "If TMP is null, no temp file is used."
  (ensure-pathnamef pathname)
  (check-type pathname tame-pathname)
  (let* ((args (cond (temp? (list in temp))
                     (in? (list in))
                     (t nil)))
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

(defgeneric pattern-name (pattern)
  (:method ((pattern symbol))
    pattern))

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
   :script trivial-prereq)
  (:documentation "A file-to-file build pattern."))

(defmethod load-form-slot-names append ((self pattern))
  '(input-defaults output-defaults script))

(defmethod pattern-name ((self pattern))
  (class-name-of self))

(defclass unloaded-pattern (pattern)
  ((name :initarg :name
         :type delayed-symbol
         :reader pattern-name))
  (:documentation "Encapsulates a pattern that has not been loaded in this session."))

(defmethods unloaded-pattern (self name)
  (:method load-form-slot-names append (self)
    '(name))
  (:method pattern-build (self)
    (make (force-symbol name))))

(defun find-pattern (pattern &optional (errorp t))
  (assure pattern
    (etypecase-of (or symbol delayed-symbol pattern) pattern
      (pattern pattern)
      (delayed-symbol (make 'unloaded-pattern :name pattern))
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
         (typecase-of (or string cl:pathname) path
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
    (unload-module-cell cell)
    (setf
     (module-cell.module cell) module
     (module-cell.timestamp cell) (now))))

(defun unload-module-cell (cell)
  (with-slots (timestamp module) cell
    (reset-inline-caches (nix module))
    (setf timestamp never)))

(defun unload-module (lang source)
  (declare (notinline module-cell))
  (unload-module-cell (module-cell lang source)))

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

(defun lang+source (lang source)
  (check-type source (and absolute-pathname file-pathname))
  (let* ((lang (or lang (guess-lang+pos source)))
         (lang (lang-name lang)))
    (values lang source)))

(defun dynamic-require-as (lang source &key force)
  (receive (lang source) (lang+source lang source)
    (when force
      (dynamic-unrequire-as lang source))
    (assure (not module-cell)
      (let ((spec (module-spec lang source)))
        (depends-on spec)
        (module-cell.module
         (module-spec-cell spec))))))

(defun require-once (lang source)
  (receive (lang source) (lang+source lang source)
    (let ((spec (module-spec lang source)))
      (or (module-cell.module
           (module-spec-cell spec))
          (dynamic-require-as lang source)))))

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
  (let ((lang-string (escape-lang-name lang)))
    (shadow-tree-translate
     (make-shadow-tree :prefix (list "fasls" lang-string))
     (pathname-directory-pathname current-dir))))

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
   (source :initarg :source))
  (:documentation "Pattern for building a fasl from a file. Note that
instances of this pattern must be parameterized with a language."))

(defmethods fasl-lang-pattern (self lang source)
  (:method pattern.output-defaults (self)
    (faslize lang source))

  (:method pattern-name (self)
    `(fasl-lang-pattern
      ,(maybe-delay-symbol (lang-name lang))
      ,source))

  (:method pattern-build (self)
    (let* ((*source* *input*)
           (lang (lang-name lang))
           (*language* lang)
           ;; Must be bound here for macros that intern
           ;; symbols.
           (*package* (user-package (resolve-package lang)))
           (*base* (pathname-directory-pathname *source*)))
      (depends-on *source*)
      (compile-to-file
       (wrap-current-module
        (expand-module lang *input*)
        lang *input*)
       (ensure-directories-exist *output*)
       :top-level (package-compile-top-level? lang)
       :source *source*))))

(defun fasl-lang-pattern (lang source)
  (let ((lang (force-symbol lang)))
    (make 'fasl-lang-pattern :lang lang :source source)))

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

(defgeneric maybe-find-asdf-system (lang)
  (:method ((lang no-such-lang))
    (maybe-find-asdf-system (no-such-lang.lang lang)))
  (:method ((lang t))
    (and (not (frozen?))
         (let ((lang (string-downcase lang)))
           (find-asdf-system lang)))))

(defun ensure-lang-exists (lang &optional (cont #'ensure-lang-exists))
  (check-type lang package-designator)
  (check-type cont function)
  (if (packagep lang) lang
      (let ((pkg (resolve-package lang)))
        (or (and pkg (package-name-keyword pkg))
            (restart-case
                (error 'no-such-lang :lang lang)
              (load-same-name-system ()
                :test maybe-find-asdf-system
                :report (lambda (s)
                          (format s "Load the system named ~a and try again" lang))
                (load-asdf-system lang)
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

(defun guess-source (lang alias)
  (~>> (etypecase-of import-alias alias
         (var-alias alias)
         ((or function-alias macro-alias)
          (second alias)))
       string-downcase
       (make-pathname :name)
       (merge-input-defaults lang)))

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
