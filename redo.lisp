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
    #:make-channel
    #:receive-result
    #:psome #:pmap
    #:task-handler-bind
    #:invoke-transfer-error
    #:submit-task)
  (:import-from #:overlord/types
    #:overlord-error
    #:overlord-error-target)
  (:import-from #:overlord/db
    #:*db*)
  (:import-from #:overlord/stamp
    #:stamp-satisfies-p)
  (:import-from #:overlord/makespan
    #:minimize-makespan
    #:optimal-machine-count)
  (:import-from #:local-time
    #:now)
  (:import-from #:overlord/util
    #:timestamp-diff)
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
                   (setf start (now))
                   (run-script build-script)
                   (setf end (now)))
              (save-temp-prereqs target)
              (save-temp-prereqsne target))
            (setf (target-build-time  target) (timestamp-diff end start)
                  (target-up-to-date? target) t)))
        (target-stamp target)))))

(defvar *already-sorted* nil)
(register-worker-special '*already-sorted*)

(defun walk-targets (fn targets &key (jobs nproc))
  "Call FN on each targets in TARGETS, in some order, and possibly in
parallel."
  (check-type fn function)
  (check-type jobs (integer 1 *))
  (assert (build-env-bound?))
  ;; We wrap the FN regardless of whether we are using parallelism or
  ;; not, to prevent reliance on side-effects.
  (labels ((walk-targets/serial (fn targets)
             (map nil fn targets))
           (try-get-tokens (build-times)
             (let ((ideal (optimal-machine-count build-times)))
               (loop for n below (min jobs
                                      ideal
                                      (length build-times))
                     for token = (ask-for-token*)
                     while token
                     collect token)))
           (walk-targets/parallel (fn targets)
             (let* ((build-times
                      (pmap* 'list #'target-build-time targets))
                    (tokens (try-get-tokens build-times)))
               (if (null tokens)
                   (walk-targets/serial fn targets)
                   (let* ((batches
                            (minimize-makespan
                             ;; Remember we are also using the current
                             ;; thread.
                             (1+ (length tokens))
                             targets
                             build-times))
                          (channels
                            (loop repeat (length tokens)
                                  collect (make-channel))))
                     (assert (= (1- (length batches))
                                (length tokens)
                                (length channels)))
                     (loop for batch in batches
                           for token in tokens
                           for ch in channels
                           do (submit-task ch
                                           ;; Watch out, loop can
                                           ;; mutate its variables.
                                           (let ((batch batch)
                                                 (token token))
                                             (wrap-worker-specials ;We just need the environment.
                                              (lambda ()
                                                (unwind-protect
                                                     (walk-targets/serial fn batch)
                                                  (return-token* token)))))))
                     ;; The last batch is handled by the current
                     ;; thread.
                     (walk-targets/serial fn (lastcar batches))
                     (map nil #'receive-result channels))))))
    (let ((fn (wrap-worker-specials fn))
          (targets (reshuffle targets)))
      (if (and (use-threads-p)
               ;; Don't bother with parallelism if there is only one
               ;; target to build.
               (length> targets 1))
          (walk-targets/parallel fn targets)
          (walk-targets/serial fn targets)))))

(defun redo-all (targets &key (jobs nproc)
                              debug)
  "Unconditionally build each target in TARGETS."
  (unless (emptyp targets)
    (with-build-env (:jobs jobs :debug debug)
      (walk-targets #'redo-target targets :jobs jobs))))

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

(defun psome* (fn seq)
  "Like `some', but possibly parallel."
  (let ((fn (wrap-worker-specials fn)))
    (if (use-threads-p)
        (with-meta-kernel ()
          (psome fn seq))
        (some fn seq))))

(defun pmap* (type fn seq)
  "Like `map', but possibly parallel."
  (let ((fn (wrap-worker-specials fn)))
    (if (use-threads-p)
        (with-meta-kernel ()
          (pmap type fn seq))
        (map type fn seq))))

(defun out-of-date? (target)
  "Return T if TARGET needs rebuilding.
Note that this rebuilds any previously saved dependencies of TARGET
that are themselves out of date."
  (mvlet* ((prereqsne (target-saved-prereqsne target))
           (prereqs (target-saved-prereqs target))
           (target-does-not-exist? (not (target-exists?/cache target)))
           (non-existent-prereqs-exist?
            (psome* #'target-exists?/cache prereqsne))
           (regular-prereqs-changed?
            ;; If we were ever to adopt parallelism as the default, we
            ;; could store information about which targets take
            ;; longest to build and build them from slowest to
            ;; fastest.
            (let* ((reqs (map 'vector #'saved-prereq-target prereqs))
                   (outdated (filter #'out-of-date? reqs)))
              (redo-all outdated)
              (psome* #'prereq-changed? prereqs)))
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

(defun redo-ifchange-all (targets
                          &key (jobs nproc)
                               debug)
  "Rebuild each target in TARGETS if it is out of date."
  (unless (emptyp targets)
    (with-build-env (:jobs jobs :debug debug)
      (walk-targets #'redo-ifchange-target targets :jobs jobs))))

(defun redo-ifcreate (&rest targets)
  "Depend on the non-existence of each target in TARGETS."
  (redo-ifcreate-all targets))

(defun redo-ifcreate-all (targets
                          &key (jobs nproc)
                               debug)
  "Depend on the non-existence of each target in TARGETS."
  ;; Probably not worth parallelizing.
  (unless (emptyp targets)
    (with-build-env (:jobs jobs :debug debug)
      (let ((targets (map 'vector #'resolve-target targets)))
        (do-each (target (reshuffle targets))
          (assert (not (target-exists?/cache target)) ()
                  "Target exists: ~a" target)
          (record-prereqne target))))))

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
