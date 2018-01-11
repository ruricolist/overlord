;;; High-level build system implementation. This package exports the
;;; functions that are implemented in overlord/target. The idea is
;;; that this should be written at a high enough level that, with a
;;; different set of definitions, it could drive an ordinary
;;; file-based Redo build system.

(defpackage :overlord/redo
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:overlord/types #:error*)
  (:nicknames :redo)
  (:export
   #:redo
   #:redo-all
   #:redo-ifchange
   #:redo-ifchange-all
   #:redo-ifcreate
   #:redo-ifcreate-all
   #:redo-always
   #:*parents*
   ;; Functions to implement.

   ;; NB Would it be worthwhile to implement these as generic
   ;; functions, so in the future we could drive different kinds of
   ;; build systems?
   #:root-target
   #:target-stamp
   #:stamp=
   #:target-exists?
   #:target=
   #:target-build-script-target
   #:target-default-build-script-target
   #:run-script
   #:record-prereq
   #:record-prereqne
   #:target-in-db?
   #:target-saved-prereqs
   #:target-saved-prereqsne
   #:saved-prereq-target
   #:saved-prereq-stamp
   #:target-up-to-date?
   #:save-temp-prereqs
   #:clear-temp-prereqs
   #:save-temp-prereqsne
   #:clear-temp-prereqsne
   #:generate-impossible-target))
(in-package #:overlord/redo)

(defgeneric root-target ())
(defgeneric target-stamp (target))
(defgeneric stamp= (stamp1 stamp2))
(defgeneric target-exists? (target))
(defgeneric target= (target1 target2))
(defgeneric target-build-script-target (target))
(defgeneric target-default-build-script-target (target))
(defgeneric run-script (task))
(defgeneric record-prereq (target))
(defgeneric save-temp-prereqs (target))
(defgeneric record-prereqne (target))
(defgeneric save-temp-prereqsne (target))
(defgeneric target-in-db? (target))
(defgeneric target-saved-prereqs (target))
(defgeneric target-saved-prereqsne (target))
(defgeneric saved-prereq-target (prereq))
(defgeneric saved-prereq-stamp (prereq))
(defgeneric target-up-to-date? (target))
(defgeneric clear-temp-prereqs (target))
(defgeneric clear-temp-prereqsne (target))
(defgeneric generate-impossible-target ())

(defvar *parents* '()
  "The chain of parents being built.")

(defun target? (target)
  "Is TARGET actually a target (not a source file)?"
  (or (not (target-exists? target))
      ;; (target-in-db? target)

      ;; NB This is a deviation from the Redo model. We don't want to
      ;; depend on the database to tell what is or is not a target,
      ;; because the database is cleared every time Overlord, or the
      ;; underlying Lisp, is upgraded. Instead, what makes something a
      ;; target is that it has a build script. (This idea comes from
      ;; Gup).
      (target-has-build-script? target)))

(defun redo (&rest args)
  (redo-all (or args (list (root-target)))))

(defun redo-all (args)
  ;; NB This is where you would add parallelism.
  (do-each (target (reshuffle args))
    (when (target? target)
      (clear-temp-prereqs target)
      (clear-temp-prereqsne target)
      (let ((build-script (resolve-build-script target)))
        (nix (target-up-to-date? target))
        (let ((*parents* (cons target *parents*)))
          (run-script build-script))
        (save-temp-prereqs target)
        (save-temp-prereqsne target)
        (setf (target-up-to-date? target) t)))))

(defun target-has-build-script? (target)
  (let ((script-target (target-build-script-target target)))
    (or (target-exists? script-target)
        (let ((default (target-default-build-script-target target)))
          (target-exists? default)))))

(defun resolve-build-script (target)
  ;; TODO What directory should be current? Or should the script take care of that?
  (let ((script-target (target-build-script-target target)))
    (if (target-exists? script-target)
        (let ((*parents* (cons target *parents*)))
          (redo-ifchange script-target)
          script-target)
        (let ((default (target-default-build-script-target target)))
          (if (target-exists? default)
              (let ((*parents* (cons target *parents*)))
                (redo-ifchange default)
                (redo-ifcreate script-target)
                default)
              (error* "No script found for ~a" target))))))

;;; Should be (target).
(-> changed? (t) boolean)
(defun changed? (target)
  (let ((changed? nil))
    ;; This can't be an or, since we have to check (and possibly
    ;; rebuild in turn) each target.
    (unless (target-exists? target)
      (setf changed? t))
    (let* ((prereqs (target-saved-prereqs target))
           (reqs (map 'list #'saved-prereq-target prereqs)))
      ;; Check regular prerequisites.
      (let* ((outdated
               ;; Although they will be reshuffled by `redo', we still
               ;; want to shuffle them here so the prereqs are built
               ;; unpredictably.
               (~> reqs
                   reshuffle
                   (filter #'changed? _)
                   (coerce 'list))))
        (when outdated
          ;; TODO redo $outdated || result=0
          (apply #'redo outdated))
        (flet ((unchanged? (prereq)
                 (let ((req   (saved-prereq-target prereq))
                       (stamp (saved-prereq-stamp prereq)))
                   (stamp= stamp (target-stamp req)))))
          ;; Check regular prerequisitves.
          (unless (every #'unchanged? (reshuffle prereqs))
            (setf changed? t)))))
    ;; Check non-existent prereqs.
    (let ((prereqsne (target-saved-prereqsne target)))
      (when (some #'target-exists? prereqsne)
        (setf changed? t)))
    changed?))

;;; The only thing special about redo-ifchange is that it writes out
;;; stamps for its deps.
(defun redo-ifchange (&rest args)
  (redo-ifchange-all args))

(defun redo-ifchange-all (args)
  ;; NB This is where you would add parallelism.
  (do-each (i (reshuffle args))
    (when (changed? i)
      (redo i))
    (record-prereq i)))

(defun redo-ifcreate (&rest targets)
  (redo-ifcreate-all targets))

(defun redo-ifcreate-all (targets)
  ;; NB This is where you would add parallelism.
  (do-each (i (reshuffle targets))
    (when (target-exists? i)
      (error* "Non-existent prerequisite ~a already exists" i))
    (record-prereqne i)))

(defun redo-always ()
  (record-prereq (generate-impossible-target)))
