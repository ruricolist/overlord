(defpackage :overlord/db
  (:use :cl :alexandria :serapeum
    :overlord/specials
    :overlord/types
    :overlord/message
    :overlord/global-state
    :overlord/cache)
  (:import-from :uiop
    :with-temporary-file
    :rename-file-overwriting-target
    :file-exists-p
    :xdg-cache-home
    :subpathp
    :ensure-directory-pathname
    :merge-pathnames*
    :pathname-directory-pathname
    :delete-file-if-exists)
  (:import-from :bordeaux-threads
    :make-thread)
  (:import-from :trivial-file-size :file-size-in-octets)
  (:import-from :fset)
  (:import-from :local-time)
  (:export
   :prop :has-prop? :delete-prop
   :save-database
   :saving-database
   :unload-db
   :deactivate-db
   :delete-versioned-db))
(in-package :overlord/db)

;;; TODO The long-range plan is to rewrite this to use a binary
;;; format, or at least for the database on disk to be gzipped. For
;;; now, however, we use plain text for ease of extending and
;;; debugging.

(deftype db-key ()
  '(not null))

(deftype db-value ()
  't)

(define-modify-macro withf (&rest item-or-tuple) fset:with)
(define-modify-macro lessf (&rest item-or-tuple) fset:less)

(defgeneric db.ref (db key))
(defgeneric (setf db.ref) (value db key))
(defgeneric db.del (db key))
(defgeneric db.sync (db))

(defunit tombstone)

(defstruct-read-only (log-record (:conc-name log-record.))
  (timestamp (get-universal-time) :type (integer 0 *))
  (data :type fset:map))

(defun delete-versioned-db (&optional (version (db-version)))
  (let ((dir (current-cache-dir version)))
    (when (uiop:directory-exists-p dir)
      (uiop:delete-directory-tree
       dir
       :validate (op (subpathp _ (xdg-cache-home)))))))

(locally (declare (optimize safety))
  (defclass db ()
    ((version
      :initform (db-version)
      :reader db.version)
     (current-map
      :initarg :current-map
      :type fset:map
      :accessor db.current-map)
     (last-saved-map
      :initarg :last-saved-map
      :type fset:map
      :accessor db.last-saved-map)
     (log
      :initarg :log
      :type pathname
      :reader db.log))
    (:default-initargs
     :current-map (fset:empty-map)
     :last-saved-map (fset:empty-map)
     :log (log-file-path))))

(defun db-alist (&optional (db (db)))
  "For debugging."
  (let ((map (db.current-map db)))
    (collecting
      (fset:do-map (k v map)
        (collect (cons k v))))))

(-> log-file-size (pathname) (integer 0 *))
(defun log-file-size (log)
  (if (file-exists-p log)
      (file-size-in-octets log)
      0))

(defmethods db (self version current-map last-saved-map log)
  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      ;; version record-count byte-count saved or not?
      (format stream "v.~a ~d record~:p, ~:d byte~:p~@[ (~a)~]"
              version
              (fset:size current-map)
              (log-file-size log)
              (and (not (eql current-map last-saved-map))
                   "unsaved"))))

  (:method db.ref (self key)
    (multiple-value-bind (value bool)
        (fset:lookup current-map
                     (assure db-key key))
      (if (eq value tombstone)
          (values nil nil)
          (values (assure db-value value)
                  (assure boolean bool)))))

  (:method (setf db.ref) (value self key)
    (check-type key db-key)
    (check-type value db-value)
    (prog1 value
      ;; TODO More CAS.
      #+sbcl
      (sb-ext:atomic-update (slot-value self 'current-map)
                            (lambda (map)
                              (fset:with map key value)))
      #-sbcl
      (synchronized (self)
        (withf current-map key value))))

  (:method db.del (self key)
    (setf (db.ref self key) tombstone)
    (values))

  (:method db.sync (self)
    (make-thread
     (lambda ()
       (synchronized (self)
         (log.update log last-saved-map current-map)
         (setf last-saved-map current-map)))
     :name "Overlord: saving database")))

(defclass dead-db (db)
  ())

(defmethods dead-db (self)
  (:method db.ref (self key)
    (declare (ignore key))
    (values nil nil))
  (:method (setf db.ref) (value self key)
    (declare (ignore key))
    value)
  (:method db.del (self key)
    (declare (ignore key))
    (values))
  (:method db.sync (self)
    (values)))

;;; TODO use a binary representation for the log.

(def db-readtable
  (lret ((*readtable*
          (copy-readtable fset::*fset-rereading-readtable*)))
    (local-time:enable-read-macros)))

(defun call/standard-io-syntax (fn)
  "Like `with-standard-io-syntax', but if there is an error, unwind
the stack so the error itself can be printed."
  (values-list
   (funcall
    (block escape
      (handler-bind ((serious-condition
                       (lambda (e)
                         ;; Mutate the local binding only.
                         (return-from escape
                           (lambda ()
                             (error e))))))
        (with-standard-io-syntax
          (constantly (multiple-value-list (funcall fn)))))))))

(defmacro with-standard-io-syntax* (&body body)
  "Macro wrapper for `call/standard-io-syntax'."
  (with-thunk (body)
    `(call/standard-io-syntax ,body)))

(defun db-write (obj stream)
  (with-standard-io-syntax*
    ;; It's possible a writer may look at the current readtable.
    (let ((*readtable* db-readtable))
      (write obj :stream stream
                 :readably t
                 :pretty nil
                 :circle nil))))

(defun log.update (log last-saved-map current-map)
  (unless (eql last-saved-map current-map)
    (let ((diff (fset:map-difference-2 current-map last-saved-map)))
      (unless (fset:empty? diff)
        (let ((record (make-log-record :data diff)))
          (with-output-to-file (out (ensure-directories-exist log)
                                    :element-type 'character
                                    :if-does-not-exist :create
                                    :if-exists :append)
            (db-write record out)
            (finish-output out)))))))

(defun strip-tombstones (map)
  (let ((out map))
    (fset:do-map (k v map)
      (when (eq v tombstone)
        (lessf out k)))
    out))

(defun log.load (log)
  (declare (optimize safety debug))
  (if (not (file-exists-p log))
      (values (fset:empty-map) 0)
      (tagbody
       :retry
         (restart-case
             (return-from log.load
               (with-standard-input-syntax
                 (let* ((*readtable* db-readtable)
                        (records
                          (with-input-from-file (in log :element-type 'character)
                            ;; TODO ignore errors?
                            (loop with eof = "eof"
                                  for record = (read in nil eof)
                                  until (eq record eof)
                                  collect record)))
                        (maps
                          (mapcar #'log-record.data records))
                        (map
                          (reduce #'fset:map-union maps
                                  :initial-value (fset:empty-map)))
                        (map (strip-tombstones map)))
                   (values map (length maps)))))
           (retry ()
             :report "Try loading the database again."
             (go :retry))
           (truncate-db ()
             :report "Treat the database as corrupt and discard it."
             (delete-file-if-exists log)
             (return-from log.load
               (values (fset:empty-map) 0)))))))

(defun squash-data (log map map-count)
  (let (temp)
    (when (> map-count 1)
      (message "Compacting log")
      (uiop:with-temporary-file (:stream s
                                 :pathname p
                                 :keep t
                                 :direction :output
                                 :element-type 'character
                                 ;; Ensure the temp file is on the
                                 ;; same file system as the log so the
                                 ;; rename is atomic.
                                 :directory (pathname-directory-pathname log))
        (setq temp p)
        (db-write (make-log-record :data map) s))
      (rename-file-overwriting-target temp log)))
  log)

(defun log.squash (log)
  (receive (map map-count) (log.load log)
    (squash-data log map map-count)))

(defun empty-db ()
  (make 'db))

(defun log-file-path ()
  (assure absolute-pathname
    (path-join
     (current-cache-dir)
     #p"log/"
     #p"log.sexp")))

(defun reload-db ()
  (let ((log (log-file-path)))
    (message "Reloading database (~:d byte~:p)"
             (log-file-size log))
    (receive (map map-count)
        (log.load log)
      (lret ((db (make 'db
                       :current-map map
                       :last-saved-map map
                       :log log)))
        (make-thread
         (lambda ()
           (synchronized (db)
             (squash-data log map map-count)))
         :name "Overlord: compacting log")))))

(define-global-state *db* nil)

(defun db ()
  (synchronized ('*db)
    (ensure-db)
    (check-version))
  *db*)

(defun ensure-db ()
  (ensure *db*
    (reload-db)))

(defun unload-db ()
  "Clear the DB out of memory in such a way that it can still be
reloaded on demand."
  ;; TODO Force a full GC afterwards?
  (synchronized ('*db*)
    (nix *db*)))

(defun deactivate-db ()
  "Clear the DB out of memory in such a way that it will not be reloaded on demand."
  (synchronized ('*db*)
    (setq *db* (make 'dead-db))))

(defun check-version ()
  (unless (= (db.version *db*)
             (db-version))
    (cerror "Load the correct database"
            "Database version mismatch")
    (setq *db* (reload-db))))

(defplace db-ref* (key)
  (db.ref (db) key))

;;; But should be manipulating stored plists instead?

(defun db-protect (x)
  (typecase x
    (keyword x)
    (symbol
     (eif (eql (symbol-package x)
               #.(find-package :cl))
          x
          (let* ((p (symbol-package x)))
            (cons (and p (package-name p))
                  (symbol-name x)))))
    (package
     (cons :package (package-name x)))
    (otherwise x)))

(defun prop-key (obj prop)
  (cons (db-protect obj)
        (db-protect prop)))

(defplace prop-1 (obj prop)
  (db.ref (db) (prop-key obj prop)))

(defun prop (obj prop &optional default)
  (multiple-value-bind (val val?) (prop-1 obj prop)
    (if val?
        (values val t)
        (values default nil))))

(defun (setf prop) (value obj prop &optional default)
  (declare (ignore default))
  (setf (prop-1 obj prop) value))

(defun has-prop? (obj prop &rest props)
  (some (op (nth-value 1 (prop-1 obj _)))
        (cons prop props)))

(defun has-props? (obj prop &rest props)
  (every (op (nth-value 1 (prop-1 obj _)))
         (cons prop props)))

(defun delete-prop (obj prop)
  (db.del (db) (prop-key obj prop)))

(defun save-database ()
  (db.sync (db))
  (values))

(uiop:register-image-dump-hook 'save-database)

(defun call/saving-database (thunk)
  (if *save-pending*
      (funcall thunk)
      (let ((*save-pending* t))
        (unwind-protect
             (funcall thunk)
          (save-database)))))

(defmacro saving-database (&body body)
  (with-thunk (body)
    `(call/saving-database ,body)))
