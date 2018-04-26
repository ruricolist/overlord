(defpackage :overlord/target-table
  (:use :cl :alexandria :serapeum)
  (:import-from :overlord/target-protocol
    :hash-target)
  (:export
   :target-table
   :hash-friendly?
   :make-target-table
   :with-target-table-locked
   :target-table-len

   :target-table-ref
   :target-table-rem
   :target-table-member
   :target-table-keys
   :clear-target-table))
(in-package :overlord/target-table)

(defgeneric hash-friendly? (target)
  (:documentation "Can TARGET be used as a key in a EQUAL hash
  table?")
  (:method ((x package)) t)
  (:method ((x symbol)) t)
  (:method ((x pathname)) t)
  (:method (x) (declare (ignore x))
    nil))

(defstruct (target-table (:conc-name target-table.)
                         (:constructor %make-target-table))
  "A table for storing targets.
This wraps an Fset map (for custom target types) and a hash table \(for built-in types) and keeps them in sync."
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
