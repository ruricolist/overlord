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
    #:receive-result
    #:psome #:pmap
    #:task-handler-bind
    #:invoke-transfer-error
    #:*task-priority*)
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
(register-worker-special '*parents*)

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
   ;; Gup). However (see `out-of-date?' below) a target is still
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
  (let ((target (resolve-target target))
        start end)
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
                   (setf start (get-internal-real-time))
                   (run-script build-script)
                   (setf end (get-internal-real-time)))
              (save-temp-prereqs target)
              (save-temp-prereqsne target))
            (setf (target-build-time  target) (- end start)
                  (target-up-to-date? target) t)))
        (target-stamp target)))))

(defvar *already-sorted* nil)
(register-worker-special '*already-sorted*)

(defun walk-targets (fn targets)
  "Call FN on each targets in TARGETS, in some order, and possibly in
parallel."
  (check-type fn function)
  (assert (build-env-bound?))
  ;; We wrap the FN regardless of whether we are using parallelism or
  ;; not, to prevent reliance on side-effects.
  (labels ((walk-targets/parallel (fn targets)
             (assert (vectorp targets)) ;Targets should have been shuffled.
             (if (eql *task-priority* :low)
                 (map nil #'receive-result
                      (target-channels fn targets :low))
                 (walk-targets/slowest-first fn targets)))
           (where-to-split (seq)
             "Where to split SEQ to isolate the slowest targets at the beginning."
             ;; Assuming the durations are normally distributed, we
             ;; want the left tail.
             (floor (* (length seq) 0.32)))
           (sort-slowest-first (targets)
             "Sort TARGETS (partially) so that the slowest targets are first."
             (if *already-sorted* targets
                 (let ((targets
                         ;; Decorate the seq of targets with build times.
                         (map 'vector
                              (op (cons (target-build-time _1) _1))
                              targets))
                       (split (where-to-split targets)))
                   ;; Partially sort the targets.
                   (nth-best! split targets #'> :key #'car)
                   (map 'vector #'cdr targets))))
           (target-channels (fn targets priority)
             (let ((*task-priority* priority))
               (run-or-spawn-jobs
                (map 'list
                     (lambda (target)
                       (partial fn target))
                     targets))))
           (walk-targets/slowest-first (fn targets)
             (mvlet* ((targets (sort-slowest-first targets))
                      (*already-sorted* t)
                      (slow fast (halves targets (where-to-split targets)))
                      (fast (nreverse fast))
                      ;; At this point slow is ordered slowest to
                      ;; fastest, and fast is ordered fastest to
                      ;; slowest.
                      (channels
                       (concatenate 'vector
                                    (target-channels fn slow :default)
                                    (target-channels fn fast :low))))
               (map nil #'receive-result channels))))
    (let ((fn (wrap-worker-specials fn))
          (targets (reshuffle targets)))
      (if (use-threads-p)
          (walk-targets/parallel fn targets)
          (map nil fn targets)))))

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
  (let ((fn (wrap-worker-specials fn)))
    (if (use-threads-p)
        (with-meta-kernel ()
          (task-handler-bind ((error #'invoke-transfer-error))
            (psome fn seq)))
        (some fn seq))))

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
    ;; (or target-does-not-exist?
    ;;     non-existent-prereqs-exist?
    ;;     regular-prereqs-changed?
    ;;     not-in-db?)
    ;; Return keywords to ease debugging.
    (cond (target-does-not-exist? :new)
          (non-existent-prereqs-exist? :prereqs)
          (regular-prereqs-changed? :changes)
          (not-in-db? :unknown)
          (t nil))))

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
