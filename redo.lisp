;;; High-level build system implementation. This package exports the
;;; functions that are implemented in overlord/impl. The idea is that
;;; this should be written at a high enough level that, with a
;;; different set of definitions, it could drive an ordinary
;;; file-based Redo build system.

(defpackage :overlord/redo
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:overlord/types #:error*)
  (:export
   #:redo
   #:redo-ifchange
   #:redo-ifcreate
   #:redo-always
   #:redo-stamp
   #:*parent*
   ;; Functions to implement.

   ;; NB Would it be worthwhile to implement these as generic
   ;; functions, so in the future we could drive different kinds of
   ;; build systems?
   #:target-stamper
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

;;; ASDF doesn't allow :lock.
#+sbcl
(sb-ext:lock-package :overlord/redo)

(declaim (notinline
          target-stamper
          stamp=
          target-exists?
          target=
          target-build-script-target
          target-default-build-script-target
          run-script
          record-prereq
          save-temp-prereqs
          record-prereqne
          save-temp-prereqsne
          target-in-db?
          target-saved-prereqs
          target-saved-prereqsne
          saved-prereq-target
          saved-prereq-stamp
          target-up-to-date?
          clear-temp-prereqs
          clear-temp-prereqsne))

(defvar-unbound *parent* "Parent of the target being built.")

(defvar-unbound *custom-stamp* "Custom stamp.")

(defconst source :source)

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
  ;; NB This is where you would add parallelism.
  (do-each (target (reshuffle args))
    (when (target? target)
      (clear-temp-prereqs target)
      (clear-temp-prereqsne target)
      (let ((build-script (resolve-build-script target)))
        (nix (target-up-to-date? target))
        (let ((*parent* target))
          (run-script build-script))
        (save-temp-prereqs target)
        (save-temp-prereqsne target)
        (setf (target-up-to-date? target) t)))))

(defun target-has-build-script? (target)
  (let ((script-target (target-build-script-target target)))
    (or (target-exists? script-target)
        (let ((default (target-default-build-script-target)))
          (target-exists? default)))))

(defun resolve-build-script (target)
  ;; TODO What directory should be current? Or should the script take care of that?
  (let ((script-target (target-build-script-target target)))
    (if (target-exists? script-target)
        (let ((*parent* target))
          (redo-ifchange script-target)
          script-target)
        (let ((default (target-default-build-script-target target)))
          (if (target-exists? default)
              (let ((*parent* target))
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
               (~> reqs
                   reshuffle
                   (filter #'changed? _)
                   (coerce 'list))))
        (when outdated
          ;; TODO redo $outdated || result=0
          (apply #'redo outdated))
        (fbind* ((target-stamp (target-stamper target))
                 ;; NB The idea with target-stamper is that it should
                 ;; be a function that looks up the stamp that was
                 ;; saved in the temporary list of prerequisites
                 ;; (which has not yet been saved). This is more
                 ;; efficient, since it avoids stamping the same file
                 ;; twice, and also necessary for `redo-stamp' to
                 ;; work.
                 (unchanged?
                  (lambda (prereq)
                    (let ((req   (saved-prereq-target prereq))
                          (stamp (saved-prereq-stamp prereq)))
                      (stamp= stamp (target-stamp req))))))
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
  ;; NB This is where you would add parallelism.
  (do-each (i (reshuffle args))
    (let ((*custom-stamp* nil))
      (when (changed? i)
        (redo i))
      (if *custom-stamp*
          (record-prereq i *custom-stamp*)
          (record-prereq i)))))

(defun redo-ifcreate (&rest targets)
  ;; NB This is where you would add parallelism.
  (do-each (i (reshuffle targets))
    (when (target-exists? i)
      (error* "Non-existent prerequisite ~a already exists" i))
    (record-prereqne i)))

(defun redo-always ()
  (declare (notinline generate-impossible-target))
  (record-prereq (generate-impossible-target)))

(defun redo-stamp (string)
  (setf (bound-value '*custom-stamp*)
        (assure string string)))
