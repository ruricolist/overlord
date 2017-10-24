;;; High-level build system implementation. This package exports the
;;; functions that are implemented in overlord/impl. The idea is that
;;; this should be written at a high enough level that, with a
;;; different set of definitions, it could drive an ordinary
;;; file-based Redo build system.

(defpackage :overlord/redo
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:overlord/specials #:*parent*)
  (:import-from #:overlord/types #:error*)
  (:export
   #:redo
   #:redo-ifchange
   #:redo-ifcreate
   #:redo-always
   #:redo-stamp
   ;; Functions to implement.

   ;; NB Would it be worthwhile to implement these as generic
   ;; functions, so in the future we could drive different kinds of
   ;; build systems?
   #:target-stamp
   #:stamp=
   #:target-exists?
   #:target=
   #:target-build-script-target
   #:target-default-build-script-target
   #:run-script
   #:record-prereq
   #:record-prereqne
   #:target-kind
   #:target-saved-prereqs
   #:target-saved-prereqsne
   #:saved-prereq-target
   #:saved-prereq-stamp
   #:target-up-to-date?))
(in-package #:overlord/redo)

(defconst source    :source)
(defconst target    :target)
(defconst nonexist  :nonexist)
(defconst prereqs   :prereqs)
(defconst prereqsne :prereqsne)
(defconst stamp     :stamp)
(defconst uptodate  :uptodate)

(defun check-parent ()
  (unless (boundp '*parent*)
    (error* "No parent.")))

;;; The only thing special about redo-ifchange is that it writes out
;;; hashes for its deps.
(defun redo (&rest args)
  ;; NB This is where you would add parallelism.
  (do-each (target (reshuffle args))
    (unless (eql source (target-kind target))
      (let ((build-script
              ;; TODO What directory should be current? Or should the script take care of that?
              (let ((script-target (target-build-script-target target)))
                ;; TODO Should we support default build scripts?
                (if (target-exists? script-target)
                    (let ((*parent* target))
                      (redo-ifchange script-target)
                      script-target)
                    (let ((default (target-default-build-script-target target)))
                      (if (target-exists? default)
                          (let ((*parent* target))
                            (redo-ifchange default) (redo-ifcreate script-target)
                            default)
                          (error* "No script found for ~a" target)))))))
        (nix (target-up-to-date? target))
        (let ((*parent* target))
          (run-script build-script))
        (setf (target-up-to-date? target) t)))))

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
      (let* ((outdated (filter #'changed? (reshuffle reqs)))
             (outdated (coerce outdated 'list)))
        (when outdated
          ;; TODO redo $outdated || result=0
          (apply #'redo outdated))
        ;; Have any of the stamps changed?
        (flet ((unchanged? (prereq)
                 (let ((req   (saved-prereq-target prereq))
                       (stamp (saved-prereq-stamp prereq)))
                   (stamp= stamp (target-stamp req)))))
          (unless (every #'unchanged? (reshuffle prereqs))
            (setf changed? t)))))
    ;; Check non-existent prereqs.
    (let ((prereqsne (target-saved-prereqsne target)))
      (when (some #'target-exists? prereqsne)
        (setf changed? t)))
    changed?))

(defun redo-ifchange (args)
  ;; NB This is where you would add parallelism.
  ;; TOOD check-parent?
  (do-each (i (reshuffle args))
    (when (changed? i)
      (redo i))
    (record-prereq i)))

(defun redo-ifcreate (&rest targets)
  (check-parent)
  (do-each (i (reshuffle targets))
    (when (target-exists? i)
      (error* "Non-existent prerequisite ~a already exists" i))
    (record-prereqne i)))
