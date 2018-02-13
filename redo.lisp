;;; High-level build system implementation. This package exports the
;;; functions that are implemented in overlord/target. The idea is
;;; that this should be written at a high enough level that, with a
;;; different set of definitions, it could drive an ordinary
;;; file-based Redo build system.

(defpackage :overlord/redo
  (:use #:cl #:alexandria #:serapeum
    #:overlord/specials)
  (:import-from #:overlord/types #:error*)
  (:import-from #:overlord/parallel
    #:with-our-kernel)
  (:import-from #:lparallel #:pmap)
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
   #:generate-impossible-target
   #:call-with-target-locked))
(in-package #:overlord/redo)

;;; NB This file is only concerned with the logic of the build system.
;;; It is not concerned with what targets are, what timestamps are, or
;;; so forth.

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
(defgeneric (setf target-up-to-date?) (value target))
(defgeneric clear-temp-prereqs (target))
(defgeneric clear-temp-prereqsne (target))
(defgeneric generate-impossible-target ())
(defgeneric call-with-target-locked (target fn))

(defvar *parents* '()
  "The chain of parents being built.")

(defmacro with-target-locked ((target) &body body)
  (with-thunk (body)
    `(call-with-target-locked
      ,target
      ,body)))

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
    *building-root*)
  "Special variables whose bindings, if any, should be propagated into
  subthreads.")

(defun redo-all (targets)
  ;; NB This is where you would add parallelism.
  (with-our-kernel ()
    (flet ((redo (target)
             (when (member target *parents* :test #'target=)
               (error* "Recursive dependency: ~a depends on itself" target))
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
                 (setf (target-up-to-date? target) t)))))
      (if (single targets)
          (redo (elt targets 0))
          (funcall (if *use-threads* #'lparallel:pmap #'map)
                   nil
                   (dynamic-closure *specials* #'redo)
                   (reshuffle targets))))))

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

(defun unchanged? (saved-prereq)
  (let ((req   (saved-prereq-target saved-prereq))
        (stamp (saved-prereq-stamp  saved-prereq)))
    (stamp= stamp (target-stamp req))))

;;; Should be (target).
(-> changed? (t) boolean)
(defun changed? (target)
  (with-our-kernel ()
    (let* ((prereqsne (target-saved-prereqsne target))
           (prereqs (target-saved-prereqs target))
           (target-does-not-exist? (not (target-exists? target)))
           (non-existent-prereqs-exist? (some #'target-exists? prereqsne))
           (regular-prereqs-changed?
             (let* ((reqs (map 'vector #'saved-prereq-target prereqs))
                    (outdated (filter #'changed? reqs)))
               (redo-all outdated)
               (notevery #'unchanged? prereqs))))
      (or target-does-not-exist?
          non-existent-prereqs-exist?
          regular-prereqs-changed?))))

(defun redo-ifchange (&rest args)
  (redo-ifchange-all args))

(defun redo-ifchange-all (args)
  (do-each (i (reshuffle args))
    (with-target-locked (i)
      (when (changed? i)
        (redo i)))
    (record-prereq i)))

(defun redo-ifcreate (&rest targets)
  (redo-ifcreate-all targets))

(defun redo-ifcreate-all (targets)
  (with-our-kernel ()
    (when-let (i (some #'target-exists? targets))
      (error* "Non-existent prerequisite ~a already exists" i)))
  (do-each (i (reshuffle targets))
    (record-prereqne i)))

(defun redo-always ()
  (record-prereq (generate-impossible-target)))
