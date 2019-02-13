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
    ;; Target tables.
    :overlord/target-table
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
    ;; Utilities.
    :overlord/util
    ;; The disk cache.
    :overlord/cache
    ;; ASDF interface.
    :overlord/asdf
    ;; How to infer the base for the current package.
    :overlord/base
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
  ;; How to test similarity (CLHS 3.2.4.2.2) of externalizable objects.
  (:import-from :overlord/similarity
    :similar?)
  (:import-from :uiop
    :implementation-identifier
    :with-temporary-file
    :rename-file-overwriting-target
    :parse-unix-namestring)
  (:import-from :overlord/kernel
    :nproc)
  (:import-from :overlord/build-env
    :build-env-bound?
    :claim-file*)
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
   :define-target-task
   :defconfig
   :define-target-config
   :var-target
   :define-target-var
   :define-var-once
   :file-target

   :ensure-absolute
   :extension
   :change-pathname-type
   :defpattern

   :find-pattern
   :build
   :build-package
   :run

   :depends-on
   :depends-on*
   :depends-on-all
   :depends-on-all*
   :depends-not
   :depends-not-all
   :use :use-all

   :ensure-target-recorded

   :task
   :pattern
   :merge-input-defaults
   :merge-output-defaults
   :pattern.input-defaults
   :pattern.output-defaults
   :pattern-name
   :pattern-build
   :pattern-ref
   :pattern-from
   :pattern-into
   :define-script
   :pattern-ref-static-inputs
   :pattern-ref-outputs
   :clear-package-prereqs
   :list-package-prereqs
   :directory-ref
   :path
   :file))

(in-package :overlord/target)
(in-readtable :standard)


;;; Shadows and preferred alternatives.

(deftype pathname ()
  'cl:pathname)

;;; This would be nice, but it confuses CCL: it thinks our `pathname'
;;; is a built-in type and won't compile the above `deftype' form.

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

(defun check-generic-function (name)
  "Check that NAME is bound to a generic function."
  (unless (and (fboundp name)
               (typep (fdefinition name)
                      'generic-function))
    (error "No generic function for ~a" name)))

(defmacro defmethod (name &body body)
  "Like `cl:defmethod', but raise an error is NAME is not already
bound as a generic function."
  `(progn
     (check-generic-function ',name)
     (cl:defmethod ,name ,@body)))


;;; Auxiliary functions for Redo.

;;; Define properties for targets in the database.
(defconst nonexist       :nonexist)
(defconst prereqs        :prereqs)
(defconst prereqs-temp   :prereqs-temp)
(defconst prereqsne      :prereqsne)
(defconst prereqsne-temp :prereqsne-temp)
(defconst stamp          :stamp)
(defconst uptodate       :uptodate)
(defconst build-time     :build-time)

(defun saved-prereq (x &optional (stamp (target-stamp x)))
  "Make a saved prereq object."
  (cons x (assure stamp stamp)))

(defmethod saved-prereq-target (p) (car p))
(defmethod saved-prereq-stamp (p) (cdr p))

;;; TODO Is there any value at this point to storing temporary prereqs
;;; in the database? They could simply be stored in a special variable
;;; in redo.lisp.

(defplace temp-prereqs (target)
  (prop target prereqs-temp (fset:empty-map)))

(defplace temp-prereqsne (target)
  (prop target prereqsne-temp (fset:empty-set)))

(defun current-parent ()
  "The current parent. If we are building, it is the target being
built; otherwise it is the current package."
  (or (first *parents*)
      *package*))

(defmethod record-prereq (target &aux (parent (current-parent)))
  (record-parent-prereq parent target))

(defmethod record-prereq ((target trivial-prereq))
  (declare (ignore target)))

(defmethod record-prereq ((target null))
  (error 'not-a-target :designator target))

(defmethod record-prereq ((target symbol))
  (let ((target (maybe-delay-symbol target)))
    (if (symbolp target)
        (call-next-method)
        (record-prereq target))))

(defgeneric record-parent-prereq (parent target)
  (:documentation "Record TARGET as a prerequisite of PARENT.")
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
  (let ((target (maybe-delay-symbol target)))
    (if (symbolp target)
        (call-next-method)
        (record-prereqne target))))

(defun record-parent-prereqne (parent target)
  (withf (temp-prereqsne parent) target))

(defmethod target-in-db? (target)
  (and (not *force*)
       (has-prop? target
                  ;; The uptodate key is sort of a fallback for a target
                  ;; that, for whatever reason, has no prerequisites.
                  ;; Otherwise such a target would be built once, and then
                  ;; never again.
                  uptodate
                  prereqs
                  prereqs-temp
                  prereqsne
                  prereqsne-temp)))

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

(defmethod target-build-time (target)
  (prop target build-time 0))

(defmethod (setf target-build-time) (value target)
  (check-type value (integer 0 *))
  (setf (prop target build-time) value))

(-> build-time-from-file (t pathname) (integer 0 *))
(defun build-time-from-file (target file)
  "Get the build time for TARGET from the database, but if there is no
recorded build time, fall back to using the size of FILE.

This heuristic ensures that, in the absence of other information,
larger files will be built before smaller files."
  (check-type file cl:pathname)
  (assert (file-pathname-p file))
  (build-time-from-files target (list file)))

(defun build-time-from-files (target files)
  "Like `build-time-from-file', but the size is a sum over FILES."
  (let* ((unknown :unknown)
         (build-time (prop target build-time unknown)))
    (if (eql build-time unknown)
        (reduce #'+ files :key (op (or (file-size-in-octets _) 0)))
        build-time)))


;;; Types.

(defcondition not-a-target (overlord-error)
  ((designator :initarg :designator))
  (:report (lambda (c s)
             (with-slots (designator) c
               (format s "Not a target: ~a" designator)))))

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

  (:method print-object (self stream)
    (if (not *print-escape*)
        (print-unreadable-object (self stream :type t)
          (format t "~a" name))
        (call-next-method)))

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
  (etypecase-of (or string directory-pathname relative-pathname) name
    (relative-pathname (directory-ref (ensure-absolute name)))
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
           (resolve-file target :base (or base (base)))))))
  (:method target= (target (y directory-ref))
    (pathname-equal path (directory-ref.path y)))
  (:method target-build-script (target)
    (let ((dir path))
      #+sbcl (declare (notinline task))
      (task target
            (lambda ()
              (let ((dir (resolve-target dir)))
                (ensure-directories-exist dir)))
            trivial-prereq)))
  (:method target-node-label (target)
    (fmt "directory ~a" path))
  (:method hash-target (target)
    (dx-sxhash (list 'directory-ref path)))
  (:method print-object (target stream)
    (if (not *print-escape*)
        (call-next-method)
        (format stream "~a~s"
                (read-eval-prefix target stream)
                `(directory-ref ,path))))
  (:method call-with-target-locked (target fn)
    "Lock the directory (file), not target."
    (claim-file* target path)
    (call-with-target-locked path fn))
  ;; The time it takes to create directory -- not worth measuring.
  (:method target-build-time (target) 0)
  (:method (setf target-build-time) (value target)
    (declare (ignore value))))

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
    (target-node-label file))
  (:method print-object (target stream)
    (if (not *print-escape*)
        (call-next-method)
        (format stream "~a~s"
                (read-eval-prefix target stream)
                `(file-digest-ref ,file))))
  (:method call-with-target-locked (target fn)
    (claim-file* target file)
    (call-with-target-locked file fn))
  (:method target-build-time (target)
    (build-time-from-file target file)))

(defclass pattern-ref (ref)
  ;; Note that the pattern slot has a silly type: a pattern ref can be
  ;; either a symbol or an instance of `pattern', which is not yet
  ;; defined. Being able to directly pass in patterns will be useful
  ;; later when we bootstrap support for languages.
  ((pattern
    :initarg :pattern
    :type (or symbol standard-object delayed-symbol)
    :reader pattern-ref-pattern)
   (name
    :initarg :inputs
    :type (list-of pathname)
    :reader pattern-ref-static-inputs)
   (outputs
    :type (list-of pathname)
    :initarg :outputs
    :reader pattern-ref-outputs))
  (:default-initargs
   :inputs  '()
   :outputs '()))

;;; Re. merge-*-defaults. Originally I was planning on a DSL, but
;;; pathnames seems to work, as long as we're careful about how we
;;; merge them. The order is important: we merge the *provided* inputs
;;; and outputs into the *defaults*, rather than vice-versa.

(defun merge-input-defaults (pattern inputs)
  (let ((defaults (pattern.input-defaults pattern)))
    (collecting
      (dolist (input inputs)
        (let ((input
                (if (stringp input)
                    (resolve-file input)
                    input)))
          (dolist (default defaults)
            ;; Here we want to preserve the host of the provided
            ;; input, so we use uiop:merge-pathnames*.
            (collect (merge-pathnames* default input))))))))

(defun merge-output-defaults (pattern inputs)
  (let ((defaults (pattern.output-defaults pattern)))
    (collecting
      (dolist (input inputs)
        (let ((input
                (if (stringp input)
                    ;; Not resolve-file; default should be able to
                    ;; override path.
                    (parse-unix-namestring input)
                    input)))
          (dolist (default defaults)
            ;; We want to be able to redirect to the output to a
            ;; different host, so we use good old cl:merge-pathnames.
            (collect (merge-pathnames default input))))))))

(defmethods pattern-ref (self (inputs name) outputs pattern)
  (:method initialize-instance :after (self &key (merge t))
    (unless (or inputs outputs)
      (error* "~
A pattern ref needs either outputs OR at least one input (or both)."))
    (when merge
      (let ((pattern (find-pattern pattern)))
        (setf inputs
              (merge-input-defaults pattern inputs))
        (setf outputs
              (merge-output-defaults pattern
                                     (or outputs inputs)))))
    (unless outputs
      (error* "Cannot determine outputs for ~a.

You must either provide a list of outputs, or provide a list of inputs from which the outputs can be default."
              self)))

  (:method print-object (self stream)
    (let ((pattern-name (pattern-name pattern)))
      (if *print-escape*
          (let* ((name (maybe-delay-symbol pattern-name))
                 (name-form
                   (if (symbolp name)
                       `(quote ,name)
                       name)))
            (format stream "~a~s"
                    (read-eval-prefix self stream)
                    `(make 'pattern-ref
                           :pattern ,name-form
                           :inputs ',inputs
                           :outputs ',outputs)))
          (print-unreadable-object (self stream :type t)
            (format stream "~a ~a -> ~a"
                    pattern-name
                    inputs
                    outputs)))))

  (:method load-form-slot-names append (self)
    '(pattern inputs outputs))

  (:method fset:compare (self (other pattern-ref))
    (fset:compare-slots self other
                        #'pattern-ref-static-inputs
                        #'pattern-ref-outputs
                        #'pattern-ref-pattern))

  (:method target-stamp (self)
    (multiple-file-stamp outputs))

  (:method resolve-target (self &optional base)
    (if (and (every #'absolute-pathname-p inputs)
             (every #'absolute-pathname-p outputs))
        self
        (let ((base (or base (base))))
          (make 'pattern-ref
                :merge nil
                :pattern (pattern-ref-pattern self)
                ;; Cf. merge-input-defaults, merge-output-defaults.
                :inputs (mapcar (op (ensure-absolute _ :base base)) inputs)
                :outputs (mapcar (op (ensure-absolute _ :base base)) outputs)))))

  (:method target= (self (other pattern-ref))
    ;; Remember the inputs and outputs are always sorted.
    (and (equal inputs (pattern-ref-static-inputs other))
         (equal outputs (pattern-ref-outputs other))
         (target= pattern (pattern-ref-pattern other))))

  (:method hash-target (self)
    (dx-sxhash
     (list 'pattern-ref
           inputs
           outputs)))

  (:method target-build-script (self)
    (let ((pattern (find-pattern pattern)))
      (task self
            (lambda ()
              (depends-on inputs)
              (pattern-build pattern inputs outputs))
            (pattern.script pattern))))

  (:method call-with-target-locked (self fn)
    (dolist (output outputs)
      (claim-file* self output))
    (funcall
     (reduce (lambda (output fn)
               (lambda ()
                 (call-with-target-locked output fn)))
             outputs
             :initial-value fn
             :from-end t)))

  (:method target-node-label (self)
    (fmt "~{~a~^, ~}"
         (mapcar #'native-namestring outputs)))

  (:method delete-target (self)
    (apply #'delete-targets outputs))

  (:method target-build-time (self)
    (build-time-from-files self inputs)))

(defun pattern-ref (pattern inputs)
  "Make a pattern reference."
  (pattern-from pattern inputs))

(defun pattern-from (pattern inputs)
  (make 'pattern-ref
        :pattern pattern
        :inputs (ensure-list inputs)))

(defun pattern-into (pattern outputs)
  (make 'pattern-ref
        :pattern pattern
        :outputs (ensure-list outputs)))

(defun pattern (name inputs)
  (pattern-ref name inputs))

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
    (target-build-script (relative-file-truename self)))
  (:method call-with-locked-target (self fn)
    (let ((file (relative-file-truename self)))
      (call-with-locked-target file fn)))
  (:method target-build-time (self)
    (let ((file (relative-file-truename self)))
      (build-time-from-file self file))))

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

(defmethod target-exists? ((target symbol))
  (boundp target))

(defmethod target-exists? ((target delayed-symbol))
  (target-exists? (force-symbol target)))

(defmethod target-exists? ((target cl:pathname))
  (pathname-exists? (resolve-target target)))

(defmethod target-timestamp ((target root-target))
  never)

(defmethod target-timestamp ((target package))
  never)

(defmethod target-timestamp ((target impossible-prereq))
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

(defmethod (setf target-timestamp) :before (timestamp target)
  (declare (ignore target timestamp))
  (check-not-frozen))

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

(defmethod target= ((x delayed-symbol) y)
  (target= (force-symbol x)
           (force-symbol y)))

(defmethod target= ((x cl:pathname) (y cl:pathname))
  (pathname-equal x y))

(defmethod hash-target ((target root-target))
  (load-time-value (sxhash root-target)))

(defmethod hash-friendly? ((target root-target))
  t)

(defmethod hash-target ((target trivial-prereq))
  (load-time-value (sxhash trivial-prereq)))

(defmethod hash-friendly? ((target trivial-prereq))
  t)

(defmethod hash-target ((target impossible-prereq))
  (load-time-value (sxhash impossible-prereq)))

(defmethod hash-friendly? ((target impossible-prereq))
  t)

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

;;; Locking targets.

(defmethod call-with-target-locked ((target root-target) fn)
  (funcall fn))

(defmethod call-with-target-locked ((target trivial-prereq) fn)
  (funcall fn))

(defmethod call-with-target-locked ((target impossible-prereq) fn)
  (funcall fn))

(defmethod call-with-target-locked ((target delayed-symbol) fn)
  (let ((target (force-symbol target)))
    (call-with-target-locked target fn)))

(defmethod call-with-target-locked ((target cl:pathname) fn)
  (let ((resolved (resolve-target target)))
    (if (equal target resolved)
        (call-next-method)
        (call-with-target-locked target fn))))

;;; Targets not worth metering.

(defmethod target-build-time ((target trivial-prereq))
  0)
(defmethod (setf target-build-time) (value (target trivial-prereq))
  (declare (ignore value))
  (values))

(defmethod target-build-time ((target impossible-prereq))
  0)
(defmethod (setf target-build-time) (value (target impossible-prereq))
  (declare (ignore value)))

(defmethod target-build-time ((target delayed-symbol))
  (target-build-time (force-symbol target)))

(defmethod (setf target-build-time) (value (target delayed-symbol))
  (setf (target-build-time (force-symbol target)) value))

(defmethod target-build-time ((target cl:pathname))
  (build-time-from-file target target))


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

(defvar *prereq-packages* (make-target-table))

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
  "List the current prerequisites of PACKAGE."
  (~> package
      find-package
      package-prereqs-table
      target-table-keys))

(defun clear-package-prereqs (package)
  "Clear the current prerequisites of PACKAGE.
Return PACKAGE."
  (let* ((package (find-package package))
         (table (package-prereqs-table package)))
    (clear-target-table table)
    package))

(defun record-package-prereq (package target)
  "Save TARGET as a prerequisite of PACKAGE.
Return TARGET."
  (setf (target-package target) package)
  target)

(defun record-package-prereq* (target)
  "Save TARGET as a prerequisite of the current PACKAGE.
Return TARGET."
  (record-package-prereq *package* target))

(defun ensure-target-recorded (target)
  "Ensure that TARGET is recorded as a prerequisite.
If there is no current parent, make TARGET a prerequisite of the
current package."
  (if *parents*
      (record-prereq target)
      (record-package-prereq* target)))

(defmethod target-saved-prereqs ((rt root-target))
  (mapcar (op (saved-prereq _1 (target-stamp _1)))
          (list-all-packages)))

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
  (let ((task (gethash target *tasks*)))
    (if (typep task 'task)
        task
        (error 'not-a-target :designator target))))

(defmethod target-build-script ((target root-target))
  (task target
        (lambda ()
          (depends-on-all (list-all-packages)))
        trivial-prereq))

(defmethod target-build-script ((target package))
  (task target
        (lambda ()
          ;; NB. Note that we do not get the prereqs of the package
          ;; target from the database. We do not want them to be
          ;; persistent; we only want to build the targets that have
          ;; been defined in this image.
          (let ((*suppress-phonies* t))
            (depends-on-all (list-package-prereqs target))))
        trivial-prereq))

(defmethod build-script-target ((script task))
  (task-script script))

(defmethod run-script (task &aux (parent (current-parent)))
  (check-not-frozen)
  ;; XXX exhaustive?
  (unless (typep parent
                 '(or impossible-prereq trivial-prereq))
    (print-target-being-built parent))
  (funcall (task-thunk task)))

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
    (save-task (force-symbol target) thunk script)))

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
         (spaces (make-string depth :initial-element #\Space))
         ;; In case we are saving the database.
         (*print-readably*))
    (message "~a@ ~a"
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
  (fmt "package ~a" (package-name target)))

;; Shouldn't happen
(defmethod target-node-label ((target trivial-prereq))
  (progn "TRIVIAL TARGET"))

;;; Shouldn't happen either.
(defmethod target-node-label ((target impossible-prereq))
  (progn "IMPOSSIBLE TARGET"))

(defmethod delete-target ((target cl:pathname))
  (unless (absolute-pathname-p target)
    (error* "Will not attempt to delete a relative pathname."))
  (when (directory-pathname-p target)
    (error* "To delete a directory, call delete-target on a directory-ref."))
  (when (wild-pathname-p target)
    (error* "Will not attempt to delete a wild pathname."))
  (unless (subpathp target (base))
    (error* "File ~a is not relative to current base." target))
  (delete-file-if-exists target))

(defmethod delete-target ((target directory-ref))
  (let ((dir (directory-ref.path target))
        (base (base)))
    (delete-directory-tree dir
                           :if-does-not-exist :ignore
                           :validate (op (subpathp _ base)))))

(defmethod delete-target ((target package))
  (delete-package target))

(defmethod delete-target ((target symbol))
  (makunbound target))

(defmethod delete-target ((target delayed-symbol))
  (when-let (symbol
             (ignoring overlord-error
               (force-symbol target)))
    (delete-target symbol)))

(defun multiple-file-stamp (files)
  (if (null files)
      ;; Is this right? If there are no files there is nothing to
      ;; build and the constraint may be considered to be satisfied.
      far-future
      (loop for file in files
            if (file-exists-p file)
              nconc (list (file-mtime file)
                          (file-size-in-octets file))
                into stamps
            else
              if (directory-exists-p file)
                collect (file-mtime file)
            else do (return never)
            finally (return (fmt "sxhash:~x" (sxhash stamps))))))

(defun file-stamp (file)
  (assert (file-exists-p file))
  (let ((size (file-size-in-octets file))
        (timestamp (target-timestamp file)))
    (file-meta size timestamp)))

(defun file-stamp/hash (file)
  (let* ((file (ensure-pathname file))
         (size (file-size-in-octets file))
         (hash (file-digest-string file)))
    (file-hash size hash)))

(defmethod target-stamp ((target cl:pathname))
  (cond ((file-exists-p target)
         (file-stamp target))
        ((directory-pathname-p target)
         (target-timestamp target))
        (t never)))

(defun rebuild-symbol (symbol value &key config)
  (let ((stamp
          (if config
              (config-stamp value)
              (now))))
    (setf (symbol-value symbol) value
          (target-timestamp symbol) stamp)))

(defun wrap-rebuild-symbol (symbol thunk &key config)
  (lambda ()
    (let* ((value (funcall thunk)))
      (rebuild-symbol symbol value :config config))))

(defun rebuild-file (file thunk &optional (base (base)))
  (lambda ()
    (let* ((file (resolve-target file base))
           (old (target-timestamp file)))
      (funcall thunk)
      ;; Since we do not control the granularity of timestamps (and
      ;; since the user may choose not to update the file), all we can
      ;; be certain of is that the new timestamp is not older than the
      ;; old timestamp.
      (assert (not (timestamp-newer? old (target-timestamp file)))))))

(defun save-file-task (file thunk script)
  (check-type file cl:pathname)
  (check-type thunk function)
  (save-task file
             (rebuild-file file thunk (base))
             script))

(defun config-stamp (value &optional (name :unknown))
  "Compute a stamp for a config.
If VALUE is simple enough to hash, return a hash.

Otherwise nil."
  (flet ((digest-as-printed (x)
           (string-digest-string
            (with-standard-io-syntax
              (prin1-to-string x)))))
    (cl:typecase value
      ((or string cl:pathname number character keyword)
       (digest-as-printed value))
      (otherwise
       (let ((digest
               (ignore-errors
                (digest-as-printed value))))
         (or digest
             (progn
               (message "Cannot use digest for ~a." name)
               nil)))))))

(defun update-config-if-changed (name new test)
  "Initialize NAME, if it is not set, or reinitialize it, if the old
value and NEW do not match under TEST."
  (let ((old (symbol-value name)))
    (if (funcall test old new) old
        (progn
          (simple-style-warning "Redefining configuration ~s" name)
          (setf (symbol-value name) new
                (target-timestamp name)
                (or (config-stamp new)
                    (now)))))))


;;; Freezing the build system.

(defun hard-freeze-targets ()
  "Freeze targets."
  ;; Variables aren't defined yet.
  (clear-target-table (symbol-value '*top-level-targets*))
  (clrhash (symbol-value '*symbol-timestamps*))
  (clrhash (symbol-value '*tasks*)))

(add-hook '*before-hard-freeze-hook*
          'hard-freeze-targets)


;;; API and keyword macros

(defmacro path (string)
  "At compile time, parse STRING using `uiop:parse-unix-namestring'.

The result is a relative pathname object.

This is an alternative to literal pathname syntax for greater
portability."
  (check-type string string)
  (parse-unix-namestring string :want-relative t))

(defun file (path)
  "Parse PATH as a Unix namestring and resolve it.
If PATH is wild, expand it."
  (etypecase-of (or string pathname) path
    (string (file (parse-unix-namestring path :want-relative t)))
    (relative-pathname (file (resolve-file path)))
    (wild-pathname (directory path))
    (pathname path)))

(define-compiler-macro file (&whole call path)
  (if (stringp path)
      `(file (path ,path))
      call))

(defun basename (file)
  (enough-pathname file (pathname-directory-pathname file)))

(defun extension (ext)
  (assure pathname
    (etypecase-of (or null string pathname) ext
      (null *nil-pathname*)
      (string (make-pathname :type ext))
      (pathname ext))))

(defun change-pathname-type (file ext)
  (merge-pathnames* (extension ext) file))

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
            ;; What ASDF wants.
            (string-downcase
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

(defun build (target/s &key force (jobs nproc)
                            debug)
  "Build TARGET/S, a single target or a list of targets."
  (check-type jobs (integer 1 *))
  (when (build-env-bound?)
    (error* "Do not call ~s recursively; use ~s instead."
            'build
            'depends-on))
  (let ((*force* force))
    (redo-all (ensure-list target/s)
              :jobs jobs
              :debug debug)))

;;; build-package-tree? That is, build a package and all of its
;;; sub-packages \(packages beginning with $package/ or $package).

(defun build-package (package &key force)
  (build (find-package package) :force force))

(defun depends-on-all (targets)
  (redo-ifchange-all targets))

(defun depends-on-all* (targets)
  (map nil #'redo-ifchange targets))

(defun depends-on (&rest targets)
  "Depend on each target in TARGETS, in no particular order.
Descends into lists."
  (depends-on-all (flatten targets)))

(defun depends-on* (&rest targets)
  "Like `depends-on', but in order."
  (depends-on-all* (flatten targets)))

(defun depends-not-all (targets)
  (redo-ifcreate-all targets))

(defun depends-not (&rest targets)
  "Depend on the targets in TARGETS not existing.
Descends into lists."
  (depends-not-all (flatten targets)))

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
  (use-all (flatten targets)))

(defun use* (&rest targets)
  "Like `use', but ordered."
  (use-all* (flatten targets)))


;;; In-Lisp targets.

(defun wrap-save-base (form)
  `(let ((*base* ,(base)))
     (with-current-dir (*base*)
       ,form)))

;;; `defconfig' is extremely important and rather tricky. Semantically
;;; is it closer to `defconstant' than `defvar' -- the provided
;;; expression is evaluated at compile time -- but unlike
;;; `defconstant' it can always be redefined.

(defun update-config-stamp (name val)
  "Update the stamp for NAME with VAL."
  (let ((stamp
          (if (boundp name)
              (target-timestamp name)
              (or (config-stamp val)
                  (now)))))
    (touch-target name stamp)
    val))

(defmacro defconfig (name &body (init &body body))
  (check-type name symbol)
  (mvlet* ((test documentation
            (ematch body
              ((list (and docstring (type string)))
               (values '#'equal docstring))
              ((trivia:lambda-list &key (test '#'equal) documentation)
               (values test documentation))))
           (init
            (wrap-save-base init)))
    `(progn
       (eval-always
         (define-global-var ,name
             (update-config-stamp ',name ,init)
           ,@(unsplice documentation)))
       (eval-always
         (save-task ',name (constantly ,init) trivial-prereq))
       (eval-always
         (update-config-if-changed ',name ,init ,test))
       ',name)))

(defmacro script-thunk (&body body)
  `(lambda ()
     ,(wrap-save-base
       `(progn ,@body))))

(defvar *scripts* (make-hash-table)
  "Set of registered scripts.")

(defun register-script (name)
  "Register NAME as a script."
  (setf (gethash (assure symbol name) *scripts*) t))

(defun clear-scripts ()
  "Nix all symbols registered as scripts."
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

(defmacro phony-task-target (name &body script)
  "Like `var-target', but does not actually declare a variable."
  `(progn
     (define-script-for ,name
       ,@script)
     (save-task ',name (script-thunk ,@script))
     (record-package-prereq* ',name)
     ',name))

(defmacro var-target (name expr &body deps)
  "Like `define-target-var', but does not actually evaluate anything."
  `(progn
     (phony-task-target ,name
       ,@deps
       (rebuild-symbol ',name ,expr))
     (defvar ,name)))

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

(defmacro define-var-once (name expr &optional docstring)
  "Like `defvar', but also re-evaluate if EXPR changes."
  (check-type docstring (or null string))
  `(define-target-var ,name ,expr
     ,@(unsplice docstring)))

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
           (script-thunk (wrap-rebuild-symbol name script-thunk :config t)))
      (save-task name script-thunk))
    (depends-on name))
  (assert (boundp name))
  (let ((init (symbol-value name))
        (timestamp (target-timestamp name)))
    `(progn
       (define-global-var ,name
           (progn
             (setf (target-timestamp ',name) ,timestamp)
             ',init))
       (save-task ',name
                  (wrap-rebuild-symbol ',name (script-thunk ,@script)
                                       :config t))
       (depends-on ',name)
       (record-package-prereq* ',name)
       ',name)))


;;;; Phony targets.

(defmacro deftask (name &body body)
  "Define a task -- a target that only has dependencies.

This is essentially a convenience to let you use script syntax to
specify the dependencies you want to build."
  `(define-target-task ,name
     ;; Phony targets don't *need* to be built.
     (unless *suppress-phonies*
       ,@body)))

(defmacro define-target-task (name &body body)
  "Lower-level version of `deftask'.

Unlike tasks defined using `deftask', tasks defined using
`define-target-task' are built before freezing."
  `(progn
     (eval-always
       (assert (not (boundp ',name))))
     (phony-task-target ,name
       (redo-always)                    ;Needless?
       ,@body)))


;;; File targets.

;;; TODO Should be enforce a single name for a file target? That is,
;;; the same file should not be a target under two names, with two
;;; scripts.

(defmacro file-target (name pathname (&key (dest nil dest-supplied?)
                                           (out nil out-supplied?))
                       &body body)
  "Define PATHNAME as a target.

PATHNAME may be a literal pathname or a string (in which case it is
parsed with `uiop:parse-unix-namestring'). Using a string is preferred
for programs that care about portability.

NAME is not a target; it is a lexical binding that serves as a
convenient handle for the name of the target. (It is also necessary to
be able to recognize that the file is out of date when BODY changes.)

The behavior of `file-target' depends on which, if any, bindings are
requested. If DEST is supplied, then it is simply bound to PATHNAME,
after it has been resolved. If only DEST is supplied, or if no
bindings are requested, then you must write directly to the
destination file.

If a binding for OUT is supplied, however, the behavior of
`file-target' changes: OUT is bound to a temporary file, and after
BODY has finished the destination file is atomically overwritten
with OUT.

You should generally prefer OUT to DEST. DEST is most useful when you
are using an external program that lets you specify the input file but
not the output file (a bad design, but unfortunately a common one)."
  (ensure-pathnamef pathname)
  (check-type pathname tame-pathname)
  (check-type dest symbol)
  (check-type out symbol)
  (let* ((base (base))
         (pathname (resolve-target pathname base))
         (dir (pathname-directory-pathname pathname))
         (script
           `(with-current-dir (,dir)
              ,pathname
              ,@body))
         (script
           (if dest-supplied?
               `(let ((,dest ,pathname))
                  ,script)
               script))
         (script
           (if out-supplied?
               `(call/temp-file-pathnames
                 ,pathname
                 (lambda (,out)
                   (let ((,out (first ,out)))
                     ,script)))
               script)))
    `(progn
       ;; Make the task accessible by name.
       (def ,name ,pathname)
       (define-script-for ,name
         ,script)
       (save-file-task ,pathname
                       (script-thunk ,script)
                       (script-for ',name))
       (record-package-prereq* ,pathname)
       ',pathname)))


;;;; File patterns.

;;; A pattern is an abstract relationship between two files.

(defgeneric pattern-name (pattern)
  (:method ((pattern symbol))
    pattern)
  (:method ((pattern delayed-symbol))
    (force-symbol pattern)))

(defgeneric pattern-build (pattern input output)
  (:documentation "Build OUTPUT from INPUT according to PATTERN."))

(defclass pattern (externalizable)
  ((input-defaults
    :initarg :input-defaults
    :type (list-of pathname)
    :reader pattern.input-defaults)
   (output-defaults
    :initarg :output-defaults
    :type (list-of pathname)
    :reader pattern.output-defaults)
   (script
    :initarg :script
    :type target
    :reader pattern.script))
  (:default-initargs
   :input-defaults ()
   :output-defaults ()
   :script trivial-prereq)
  (:documentation "A file-to-file build pattern."))

(defmethod initialize-instance :after ((self pattern) &key)
  (flet ((canonicalize-defaults (defaults)
           (or (substitute *nil-pathname* nil defaults)
               (list *nil-pathname*))))
    (with-slots (input-defaults output-defaults) self
      (callf #'canonicalize-defaults input-defaults)
      (callf #'canonicalize-defaults output-defaults))))

(defmethod load-form-slot-names append ((self pattern))
  '(input-defaults output-defaults script))

(defmethod pattern-name ((self pattern))
  (class-name-of self))

(defmethod fset:compare ((x pattern) (y pattern))
  (fset:compare-slots x y
                      #'pattern-name
                      #'pattern.input-defaults
                      #'pattern.output-defaults
                      #'pattern.script))

(defclass unloaded-pattern (pattern)
  ((name :initarg :name
         :type delayed-symbol
         :reader pattern-name))
  (:documentation "Encapsulates a pattern that has not been loaded in this session."))

(defmethods unloaded-pattern (self name)
  (:method load-form-slot-names append (self)
    '(name))
  (:method pattern-build (self input output)
    (pattern-build (make (force-symbol name))
                   input output)))

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

(defmacro defpattern (class-name
                      (&key (in nil in-supplied?)
                            (out nil out-supplied?)
                            (dest nil dest-supplied?))
                      (&rest initargs &key &allow-other-keys)
                      &body script)
  "Define a file pattern named NAME.

Some build systems let you define file patterns based on extensions or
regular expressions. That won't work for Overlord, because there is no
special namespace for targets, so such a rule would apply everywhere.
It has to have a name.

The behavior of `defpattern' changes based on the bindings you
request. IN is bound to the name of the input file or files.

For the meaning of OUT and DEST, compare the documentation for
`file-target'."
  (mvlet ((class-options script
           (loop for form in script
                 if (and (consp form)
                         (keywordp (car form)))
                   collect form into class-options
                 else collect form into script
                 finally (return (values class-options script))))
          ;; You could do this in the lambda list, but it would make
          ;; it ugly and hard to read.
          (in   (or in   (list (string-gensym 'in))))
          (out  (or out  (list (string-gensym 'out))))
          (dest (or dest (list (string-gensym 'dest))))
          (in-temp   (string-gensym 'in))
          (dest-temp (string-gensym 'dest))
          (out-temp  (string-gensym 'out)))
    `(progn
       (define-script-for ,class-name
         ;; Be careful not to splice in the gensyms.
         ,(and in-supplied? in)
         ,(and out-supplied? out)
         ,(and dest-supplied? dest)
         ,@initargs
         ,@script)
       (defclass ,class-name (pattern)
         ()
         (:default-initargs
          :script ',(script-for class-name)
          ;; Save the base around initforms.
          ,@(loop for (initarg initform) in (batches initargs 2)
                  append `(,initarg ,(wrap-save-base initform))))
         ,@class-options)
       (defmethod pattern-build ((self ,class-name) ,in-temp ,dest-temp)
         (declare
          (ignorable
           ,@(unsplice (unless in-supplied? in-temp))
           ,@(unsplice (unless dest-supplied? dest-temp))))
         ,(let* ((form `(progn ,@script))
                 (form (wrap-save-base form))
                 (form
                   (if (not in-supplied?) form
                       `(cl:multiple-value-bind ,in
                            (values-list ,in-temp)
                          ,form)))
                 (form
                   (if (not dest-supplied?) form
                       `(cl:multiple-value-bind ,dest
                            (values-list ,dest-temp)
                          ,form))))
            (if out-supplied?
                `(call/temp-file-pathnames
                  ,dest-temp
                  (lambda (,out-temp)
                    (cl:multiple-value-bind ,out (values-list ,out-temp)
                      ,form)))
                form))))))
