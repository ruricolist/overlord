;;; High-level build system implementation. This package exports the
;;; functions that are implemented in overlord/target. The idea is
;;; that this should be written at a high enough level that, with a
;;; different set of definitions, it could drive an ordinary
;;; file-based Redo build system.

(defpackage :overlord/redo
  (:use #:cl #:alexandria #:serapeum
    #:overlord/specials
    #:overlord/target-protocol
    #:overlord/target-table
    #:overlord/build-env)
  (:import-from #:overlord/kernel
    #:with-meta-kernel
    #:nproc)
  (:import-from #:lparallel
    #:psome #:pmap #:*kernel*)
  (:import-from #:overlord/types
    #:overlord-error
    #:overlord-error-target)
  (:import-from #:overlord/db
    #:*db*)
  (:import-from #:overlord/stamp
    #:stamp-satisfies-p)
  (:nicknames :redo)
  (:export
   #:recursive-dependency
   #:missing-script
   #:building?
   #:redo
   #:redo-all
   #:redo-ifchange
   #:redo-ifchange-all
   #:redo-ifcreate
   #:redo-ifcreate-all
   #:redo-always
   #:*parents*
   #:target-tree))
(in-package #:overlord/redo)

;;; NB This file is only concerned with the logic of the build system.
;;; It is not concerned with what targets are, what timestamps are, or
;;; so forth.

(defcondition target-error (overlord-error)
  ((target :initarg :target
           :reader overlord-error-target)))

(defcondition recursive-dependency (target-error)
  ()
  (:report (lambda (c s)
             (format s "Recursive dependency: ~a depends on itself"
                     (overlord-error-target c)))))

(defcondition missing-script (target-error)
  ()
  (:report (lambda (c s)
             (format s "No script found for target ~a."
                     (overlord-error-target c)))))

(defcondition non-existent-exists (target-error)
  ()
  (:report (lambda (c s)
             (format s "Non-existent prerequisite ~a exists."
                     (overlord-error-target c)))))

(defvar *parents* '()
  "The chain of parents being built.")

(defun building? ()
  "Return T if anything is being built."
  (true *parents*))

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

(defun redo (&rest targets)
  "Unconditionally build each target in TARGETS."
  (redo-all (or targets (list root-target))))

(defun target-build-script-target (target)
  (build-script-target
   (target-build-script target)))

(defun target-has-build-script? (target)
  (target-exists? (target-build-script-target target)))

(defun redo-target (target)
  "Unconditionally build TARGET."
  (let ((target (resolve-target target)))
    (ensure (cached-stamp target)
      ;; This only needs to be checked if we are actually building.
      ;; E.g. `trivial-prereq'.
      (with-target-locked (target)
        (when (member target *parents* :test #'target=)
          (error 'recursive-dependency
                 :target target))
        (when (target? target)
          (clear-temp-prereqs target)
          (clear-temp-prereqsne target)
          (let ((build-script (resolve-build-script target)))
            (nix (target-up-to-date? target))
            (unwind-protect
                 (let ((*parents* (cons target *parents*)))
                   (run-script build-script))
              (save-temp-prereqs target)
              (save-temp-prereqsne target))
            (setf (target-up-to-date? target) t)))
        (target-stamp target)))))

(defvar *specials*
  '(*parents*
    *force*
    *base*
    *standard-output*
    *trace-output*
    *error-output*
    *kernel*
    *db*
    *db-version*)
  "Specials that need to be propagated when to worker threads.")

(defun walk-targets (fn targets)
  (assert (build-env-bound?))
  (let ((targets (reshuffle targets)))
    (if (and (use-threads-p)
             (>= (length targets) nproc))
        (let* ((fn (build-env-closure fn))
               (fn (dynamic-closure *specials* fn)))
          (pmap nil fn :parts nproc targets))
        (map nil fn targets))))

(defun redo-all (targets)
  "Unconditionally build each target in TARGETS."
  (with-build-env ()
    (walk-targets #'redo-target targets)))

(defun resolve-build-script (target)
  "Find a build script for TARGET, and depend on it.
If there is no script for TARGET, signal an error."
  ;; TODO What directory should be current? Or should the script take care of that?
  (setf target (resolve-target target))
  (let* ((script (target-build-script target))
         (script-target (build-script-target script)))
    (if (target-exists? script-target)
        (let ((*parents* (cons target *parents*)))
          (redo-ifchange script-target)
          script)
        (progn
          (cerror "Retry"
                  'missing-script
                  :target target)
          (resolve-build-script target)))))

(defun prereq-changed? (saved-prereq)
  "Take SAVED-PREREQ, which has slots for a target and its last stamp,
and return T if the stamp has changed."
  (let* ((req (saved-prereq-target saved-prereq))
         (old-stamp (saved-prereq-stamp saved-prereq))
         (new-stamp (target-stamp/cache req)))
    (not (stamp-satisfies-p new-stamp old-stamp))))

(defun some* (fn seq)
  "Like `some', but possibly parallel."
  (if (use-threads-p)
      (with-meta-kernel ()
        (psome (build-env-closure fn) seq))
      (some fn seq)))

(defun out-of-date? (target)
  "Return T if TARGET needs rebuilding.
Note that this rebuilds any previously saved dependencies of TARGET
that are themselves out of date."
  (mvlet* ((prereqsne (target-saved-prereqsne target))
           (prereqs (target-saved-prereqs target))
           (target-does-not-exist? (not (target-exists?/cache target)))
           (non-existent-prereqs-exist?
            (some* #'target-exists?/cache prereqsne))
           (regular-prereqs-changed?
            ;; If we were ever to adopt parallelism as the default, we
            ;; could store information about which targets take
            ;; longest to build and build them from slowest to
            ;; fastest.
            (let* ((reqs (map 'vector #'saved-prereq-target prereqs))
                   (outdated (filter #'out-of-date? reqs)))
              (redo-all outdated)
              (some* #'prereq-changed? prereqs)))
           (not-in-db?
            (and (target? target)
                 (not (target-in-db? target)))))
    (or target-does-not-exist?
        non-existent-prereqs-exist?
        regular-prereqs-changed?
        not-in-db?)))

(defun redo-ifchange (&rest targets)
  "Rebuild each target in TARGETS if it is out of date."
  (redo-ifchange-all targets))

(defun redo-ifchange-target (target)
  "Rebuild TARGET if it is out of date."
  (setf target (resolve-target target))
  (when (out-of-date? target)
    (redo target))
  (record-prereq target))

(defun redo-ifchange-all (targets)
  "Rebuild each target in TARGETS if it is out of date."
  (with-build-env ()
    (walk-targets #'redo-ifchange-target targets)))

(defun redo-ifcreate (&rest targets)
  "Depend on the non-existence of each target in TARGETS."
  (redo-ifcreate-all targets))

(defun redo-ifcreate-all (targets)
  "Depend on the non-existence of each target in TARGETS."
  ;; Probably not worth parallelizing.
  (with-build-env ()
    (let ((targets (map 'vector #'resolve-target targets)))
      (do-each (target (reshuffle targets))
        (assert (not (target-exists?/cache target)) ()
                "Target exists: ~a" target)
        (record-prereqne target)))))

(defun redo-always ()
  "Depend on an impossible prerequisite.
This ensures that the script for the current target is always run, no
matter what."
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
