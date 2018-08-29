;;; High-level build system implementation. This package exports the
;;; functions that are implemented in overlord/target. The idea is
;;; that this should be written at a high enough level that, with a
;;; different set of definitions, it could drive an ordinary
;;; file-based Redo build system.

(defpackage :overlord/redo
  (:use #:cl #:alexandria #:serapeum
    #:overlord/specials
    #:overlord/target-protocol
    #:overlord/target-table)
  (:import-from #:overlord/types #:error*)
  (:import-from #:overlord/db #:saving-database)
  (:import-from #:overlord/parallel
    #:with-our-kernel)
  (:import-from #:overlord/stamp
    #:stamp-satisfies-p)
  (:import-from #:lparallel #:pmap)
  (:nicknames :redo)
  (:export
   #:building?
   #:redo
   #:redo-all
   #:redo/parallel
   #:redo-ifchange
   #:redo-ifchange-all
   #:redo-ifchange/parallel
   #:redo-ifcreate
   #:redo-ifcreate-all
   #:redo-always
   #:*parents*
   #:target-tree))
(in-package #:overlord/redo)

;;; NB This file is only concerned with the logic of the build system.
;;; It is not concerned with what targets are, what timestamps are, or
;;; so forth.

(defvar *parents* '()
  "The chain of parents being built.")

(defun building? ()
  (true *parents*))

(defvar *run-env*)

(defclass run-env ()
  ((lock :initform (bt:make-lock)
         :reader serapeum:monitor)
   (target-info
    :initform (make-target-table)
    :reader run-env.table)
   (hits :initform 0
         :accessor run-env.hits)
   (misses :initform 0
           :accessor run-env.misses))
  (:documentation "Metadata for the build run."))

(defclass target-meta ()
  ((target :initarg :target
           :reader target-meta.target)
   (lock :initform (bt:make-lock)
         :reader target-meta.lock
         :reader monitor)
   (stamp :accessor target-meta.stamp
          :initform nil))
  (:documentation "Run-scoped metadata for an individual target."))

(defparameter *print-hit-or-miss* nil)

(defun call/run-env (fn)
  (if (boundp '*run-env*)
      (funcall fn)
      (let ((*run-env* (make 'run-env)))
        (values (funcall fn)
                (progn
                  (when *print-hit-or-miss*
                    (format t "~&Hit: ~a, Miss: ~a"
                            (run-env.hits *run-env*)
                            (run-env.misses *run-env*)))
                  *run-env*)))))

(defmacro with-run-env ((&key) &body body)
  (with-thunk (body)
    `(call/run-env ,body)))

(defun target-meta (target)
  (let* ((table (run-env.table *run-env*)))
    (or (target-table-ref table target)
        (synchronized (table)
          (ensure (target-table-ref table target)
            (make 'target-meta :target target))))))

;;; Should target locks be stored in the database?
(defun call-with-target-locked (target fn)
  (synchronized ((target-meta target))
    (funcall fn)))

(defmacro with-target-locked ((target) &body body)
  (with-thunk (body)
    `(call-with-target-locked
      ,target
      ,body)))

(defplace cached-stamp (target)
  (target-meta.stamp (target-meta target)))

(defun hit-or-miss (hit miss &aux (env *run-env*))
  "Track cache hits and misses."
  (if hit
      (progn
        (incf (run-env.hits env))
        hit)
      (progn
        (incf (run-env.misses env))
        miss)))

(defun target-exists?/cache (target)
  "Skip hitting the filesystem to check if a target exists if we
already built it."
  (if (not (boundp '*run-env*))
      (target-exists? target)
      (hit-or-miss
       (true (cached-stamp target))
       (target-exists? target))))

(defun target-stamp/cache (target)
  "Skip hitting the filesystem to check a target's stamp if we already
built it."
  (if (not (boundp '*run-env*))
      (target-stamp target)
      (hit-or-miss
       (cached-stamp target)
       (target-stamp target))))

(defun target? (target)
  "Is TARGET actually a target (not a source file)?"
  (or
   ;; (not (target-exists? target))
   ;; (target-in-db? target)

   ;; NB This is a deviation from the Redo model. We don't want to
   ;; depend on the database to tell what is or is not a target,
   ;; because the database is cleared every time Overlord, or the
   ;; underlying Lisp, is upgraded. Instead, what makes something a
   ;; target is that it has a build script. (This idea comes from
   ;; Gup). However (see `changed?' below) a target is still
   ;; considered out of date if it has no presence in the DB.
   (target-has-build-script? target)))

(defun redo (&rest args)
  (redo-all (or args (list root-target))))

(defparameter *specials*
  '(*parents*
    *trace-output*                      ;For debugging.
    *base*
    *input*
    *output*
    *deps*
    *source*
    *language*
    *program-preamble*
    *default-pathname-defaults*
    *compile-file-truename*
    *load-truename*
    *cli*
    *suppress-phonies*
    *save-pending*
    *run-env*)
  "Special variables whose bindings, if any, should be propagated into
  subthreads.")

(defun walk-targets (fn seq)
  (let ((seq (reshuffle seq))
        (fn (ensure-function fn)))
    (if (use-threads-p)
        (with-our-kernel ()
          (pmap nil (dynamic-closure *specials* fn)
                seq))
        (map nil fn seq))))

(defun redo-target (target)
  (setf target (resolve-target target))
  (when-let (stamp (cached-stamp target))
    (return-from redo-target stamp))
  (with-target-locked (target)
    (when (member target *parents* :test #'target=)
      (error* "Recursive dependency: ~a depends on itself" target))
    (unless (target? target)
      (return-from redo-target
        (setf (cached-stamp target)
              (target-stamp target))))
    (clear-temp-prereqs target)
    (clear-temp-prereqsne target)
    (let ((build-script (resolve-build-script target)))
      (nix (target-up-to-date? target))
      (unwind-protect
           (let ((*parents* (cons target *parents*)))
             (run-script build-script))
        (save-temp-prereqs target)
        (save-temp-prereqsne target))
      (setf (target-up-to-date? target) t
            (cached-stamp target) (target-stamp target)))))

(defun redo/parallel (targets)
  (with-run-env ()
    (saving-database
      (if (single targets)
          (redo-all targets)
          (walk-targets #'redo-target targets)))))

(defun redo-all (targets)
  (with-run-env ()
    (saving-database
      (do-each (target (reshuffle targets))
        (redo-target target)))))

(defun target-build-script-target (target)
  (build-script-target
   (target-build-script target)))

(defun target-has-build-script? (target)
  (target-exists? (target-build-script-target target)))

(defun resolve-build-script (target)
  ;; TODO What directory should be current? Or should the script take care of that?
  (setf target (resolve-target target))
  (let* ((script (target-build-script target))
         (script-target (build-script-target script)))
    (if (target-exists? script-target)
        (let ((*parents* (cons target *parents*)))
          (redo-ifchange script-target)
          script)
        (error* "No script found for ~a" target))))

(defun unchanged? (saved-prereq)
  (let* ((req (saved-prereq-target saved-prereq))
         (old-stamp (saved-prereq-stamp saved-prereq))
         (new-stamp (target-stamp/cache req)))
    (stamp-satisfies-p new-stamp old-stamp)))

(-> changed? (t) boolean)
(defun changed? (target)
  (mvlet* ((prereqsne (target-saved-prereqsne target))
           (prereqs (target-saved-prereqs target))
           (target-does-not-exist? (not (target-exists?/cache target)))
           (non-existent-prereqs-exist? (some #'target-exists?/cache prereqsne))
           (regular-prereqs-changed?
            ;; If we were ever to adopt parallelism as the default, we
            ;; could store information about which targets take
            ;; longest to build and build them from slowest to
            ;; fastest.
            (let* ((reqs (map 'vector #'saved-prereq-target prereqs))
                   (outdated (filter #'changed? reqs)))
              (redo-all outdated)
              (notevery #'unchanged? prereqs)))
           (not-in-db?
            (and (target? target)
                 (not (target-in-db? target)))))
    (or target-does-not-exist?
        non-existent-prereqs-exist?
        regular-prereqs-changed?
        not-in-db?)))

(defun redo-ifchange (&rest args)
  (redo-ifchange-all args))

(defun redo-ifchange-target (target)
  (setf target (resolve-target target))
  (when (changed? target)
    (redo target))
  (record-prereq target))

(defun redo-ifchange-all (args)
  (with-run-env ()
    (do-each (i (reshuffle args))
      (redo-ifchange-target i))))

(defun redo-ifchange/parallel (targets)
  (if (single targets)
      (redo-ifchange-all targets)
      (walk-targets #'redo-ifchange-target
                    targets)))

(defun redo-ifcreate (&rest targets)
  (redo-ifcreate-all targets))

(defun redo-ifcreate-all (targets)
  (setf targets (map 'vector #'resolve-target targets))
  (when-let (i (some #'target-exists?/cache targets))
    (error* "Non-existent prerequisite ~a already exists" i))
  (with-run-env ()
    (do-each (i (reshuffle targets))
      (record-prereqne i))))

(defun redo-always ()
  (record-prereq impossible-prereq))

(defun target-tree (&optional (target root-target))
  "Return a list of (target . deps), where each dep is also a target
tree.

As a second value, return the non-existent prereqs."
  (if (not (target? target))
      (values nil nil)
      (let* ((saved-prereqs (target-saved-prereqs target))
             (targets (mapcar #'saved-prereq-target saved-prereqs))
             (deps (mapcar #'target-tree targets))
             (tree (cons target deps)))
        (values tree
                (target-saved-prereqsne target)))))
