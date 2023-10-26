(defpackage :overlord/db
  (:use :cl :alexandria :serapeum
    :overlord/specials
    :overlord/types
    :overlord/message
    :overlord/global-state
    :overlord/cache
    :overlord/asdf)
  (:import-from :uiop
    :implementation-type
    :with-temporary-file
    :rename-file-overwriting-target
    :file-exists-p
    :xdg-cache-home
    :subpathp
    :ensure-directory-pathname
    :merge-pathnames*
    :pathname-directory-pathname
    :delete-file-if-exists
    :directory-exists-p
    :delete-directory-tree
    :register-image-dump-hook
    :pathname-parent-directory-pathname)
  (:import-from :bordeaux-threads
    :make-thread)
  (:import-from :trivial-file-size :file-size-in-octets)
  (:import-from :fset)
  (:import-from :local-time)
  (:import-from :lparallel
    :kernel-worker-index)
  (:import-from :exit-hooks
    :add-exit-hook)
  (:import-from :overlord/util
    :withf :lessf)
  (:export
   :prop :has-prop? :delete-prop
   :save-database
   :saving-database
   :unload-db
   :deactivate-db
   :delete-versioned-db
   :db-loaded?
   :require-db))
(in-package :overlord/db)

;;; The database is a single file, an append-only log. If the log is
;;; too long, it is compacted during the initial load.

;;; The log is stored at a path that incorporates the Lisp
;;; implementation and version, as well as the Overlord version.
;;; Different Lisps get their own databases, and changes to the Lisp
;;; version, or the Overlord version, automatically result in a clean
;;; database.

;;; Any change to the database format should be accompanied by bumping
;;; the Overlord version. (This is, in fact, why Overlord is
;;; versioned.)

;;; The database is thread-safe within a single Lisp instance, but it
;;; should not be accessed from multiple Lisp instances
;;; simultaneously.

;;; At the moment database records are just Lisp objects, written with
;;; `write' and read in with `read'. This is more than fast enough. If
;;; this becomes an impediment, the next step would be to introduce
;;; streaming compression, so each record is compressed as it is
;;; written to disk. (This might require zlib.) Only if that becomes
;;; an impediment would be worthwhile to introduce a binary format.

(defvar *save-pending* nil
  "Is there a save pending?")

(deftype db-key ()
  "Type of database keys."
  '(not null))

(deftype db-value ()
  "Type for database values."
  't)

(defcondition db-error (overlord-error)
  ())

(defcondition version-mismatch (db-error)
  ((new-version :initarg :new-version)
   (old-version :initarg :old-version))
  (:report (lambda (c s)
             (with-slots (new-version old-version) c
               (format s "Database version mismatch: wanted ~a, got ~a."
                       new-version
                       old-version)))))

(defcondition locked-db (db-error)
  ((saved-pid :initarg :saved-pid))
  (:report (lambda (c s)
             (with-slots (pid saved-pid) c
               (format s "The database is already locked by another process (pid ~a)."
                       saved-pid)))))

(defun db-error (control-str &rest args)
  (make-condition 'db-error
                  :format-control control-str
                  :format-arguments args))

(define-compiler-macro db-error (&whole call control-str &rest args)
  (if (stringp control-str)
      `(db-error (formatter ,control-str) ,@args)
      call))

(defgeneric db.ref (db key)
  (:documentation "Lookup KEY in DB."))

(defgeneric (setf db.ref) (value db key)
  (:documentation "Set the value of KEY in DB."))

(defgeneric db.del (db key)
  (:documentation "Delete a key in the database."))

(defgeneric db.sync (db)
  (:documentation "Sync the database to disk."))

(defgeneric lock-db (db)
  (:documentation "Create a lockfile for the DB.")
  (:method ((db t)) nil))

(defgeneric unlock-db (db)
  (:documentation "Remove the lockfile for the DB.")
  (:method ((db t)) nil))

(defunit tombstone "A tombstone value.")

(defstruct-read-only (log-record (:conc-name log-record.))
  "A single record in a log file."
  (timestamp (get-universal-time) :type (integer 0 *))
  (data :type fset:map))

(defstruct-read-only (log-data (:conc-name log-data.))
  "The data recovered from a log file."
  (map-count 0 :type (integer 0 *))
  (map (fset:empty-map) :type fset:map))

(def no-log-data (make-log-data)
  "An empty set of log data.")

(defun delete-versioned-db (&key (version (db-version)))
  "Delete a specific version of the database.
The database is always implicitly versioned."
  (let ((dir (pathname-parent-directory-pathname (current-cache-dir version))))
    (when (directory-exists-p dir)
      (delete-directory-tree
       dir
       :validate (op (subpathp _ (xdg-cache-home)))))))

;;; NB. This is a structure rather than a CLOS class so the slots work
;;; with SBCL's compare-and-swap.
(defstruct (db (:conc-name db.))
  "The database."
  (version (db-version) :read-only t)
  (log-file (log-file-path) :type pathname :read-only t)
  (current-map (fset:empty-map) :type fset:map)
  (last-saved-map (fset:empty-map) :type fset:map))

(defun db-alist (&optional (db (db)))
  "Return the database's data as an alist.
For debugging."
  (let ((map (db.current-map db)))
    (collecting
      (fset:do-map (k v map)
        (collect (cons k v))))))

(-> log-file-size (pathname) (integer 0 *))
(defun log-file-size (log-file)
  "Return the size on disk of LOG-FILE."
  (if (file-exists-p log-file)
      (or (values (file-size-in-octets log-file))
          0)
      0))

(defmethods db (db (version #'db.version)
                   (log-file #'db.log-file)
                   (current-map #'db.current-map)
                   (last-saved-map #'db.last-saved-map))
  (:method print-object (db stream)
    (print-unreadable-object (db stream :type t)
      ;; version record-count byte-count saved or not?
      (format stream "v.~a ~d record~:p, ~:d byte~:p~@[ (~a)~]"
              version
              (fset:size current-map)
              (log-file-size log-file)
              (and (not (eql current-map last-saved-map))
                   "unsaved"))))

  (:method db.ref (db key)
    (receive (value bool)
        (fset:lookup current-map
                     (assure db-key key))
      (if (eq value tombstone)
          (values nil nil)
          (values (assure db-value value)
                  (assure boolean bool)))))

  (:method (setf db.ref) (value db key)
    (check-type key db-key)
    (check-type value db-value)
    (prog1 value
      ;; TODO More CAS.
      #+sbcl
      (sb-ext:atomic-update (db.current-map db)
                            (lambda (map)
                              (fset:with map key value)))
      #-sbcl
      (synchronized (db)
        (withf current-map key value))))

  (:method db.del (db key)
    (setf (db.ref db key) tombstone)
    (values))

  (:method db.sync (db)
    (let ((output *message-stream*))
      (flet ((sync ()
               (synchronized (db)
                 (when (append-to-log log-file
                                      last-saved-map
                                      current-map)
                   (setf last-saved-map current-map)
                   (let ((*message-stream* output))
                     (message "Wrote database"))))))
        (if (use-threads-p)
            (make-thread #'sync :name "Overlord: saving database")
            (sync)))))

  (:method db.lock-file (db)
    (make-pathname :type "lock" :defaults log-file))

  (:method lock-db (db)
    ;; XXX This locking scheme is currently effectively disabled.
    (let* ((file (db.lock-file db))
           (pid (or (getpid)
                    ;; Just in case we missed one.
                    (load-time-value
                     (let ((*random-state* (make-random-state t)))
                       (random 1000000))))))
      (ensure-directories-exist file)
      (tagbody
       :retry
         (handler-case
             (with-output-to-file (out file
                                       :if-exists :error
                                       :if-does-not-exist :create
                                       :external-format :ascii)
               ;; "HDB UUCP lock file format" (according to the FHS).
               (format out "~10d~%" pid))
           (file-error ()
             (go :try)))
       :try
         (let ((saved-pid
                 (parse-integer
                  (read-file-into-string file
                                         :external-format :ascii))))
           (unless (= saved-pid pid)
             ;; (cerror "Steal the database"
             ;;         'locked-db
             ;;         :saved-pid saved-pid)
             ;; (message "Database was locked by ~a, stealing." saved-pid)
             (delete-file-if-exists file)
             (go :retry))))))

  (:method unlock-db (db)
    (delete-file-if-exists (db.lock-file db))))

(defun getpid ()
  ;; Adapted from the sources of Sly. Should there be a trivial-getpid
  ;; library?
  #+ccl (ccl::getpid)
  #+sbcl (sb-posix:getpid)
  #+ecl (ext:getpid)
  #+clisp (os:process-id)
  #+cmucl (unix:unix-getpid)
  #+abcl (ext:get-pid)
  #+allegro (excl.osi:getpid)
  #+(and lispworks win32) (win32:get-current-process-id)
  #+(and lispworks (not win32))
  (system::getpid)
  #+mkcl (mkcl:getpid)
  #+scl (unix:unix-getpid)
  #+clasp (si:getpid)
  #+cormanlisp ccl:*current-process-id*)

(defstruct (dead-db (:include db)
                    (:conc-name db.))
  (state nil :type null :read-only t))

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

(def db-readtable
  ;; Extend the readtable with local-time and fset.
  (lret ((*readtable*
          (copy-readtable fset::*fset-rereading-readtable*)))
    (local-time:enable-read-macros))
  "The readtable for reading back the log.")

(defun call/standard-io-syntax (fn)
  "Like `with-standard-io-syntax', but if there is an error, unwind
the stack so the error itself can be printed."
  (values-list
   (funcall
    (block escape
      (handler-bind ((serious-condition
                       (lambda (e)
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
  "Write OBJ to STREAM using the database syntax."
  (with-standard-io-syntax*
    ;; It's possible a writer may look at the current readtable.
    (let ((*readtable* db-readtable))
      (write obj :stream stream
                 :readably t
                 :pretty nil
                 :circle nil))))

(-> append-to-log (t fset:map fset:map) boolean)
(defun append-to-log (log last-saved-map current-map)
  "Compute the difference between CURRENT-MAP and LAST-SAVED-MAP and
write it into LOG.

If there is no difference, write nothing.

Return T if something was written, nil otherwise."
  (unless (eql last-saved-map current-map)
    (let ((diff
            ;; This returns just the pairs that are new in current-map
            (fset:map-difference-2 current-map last-saved-map)))
      (unless (fset:empty? diff)
        (let ((record (make-log-record :data diff)))
          (with-output-to-file (out (ensure-directories-exist log)
                                    :element-type 'character
                                    :if-does-not-exist :create
                                    :if-exists :append)
            (db-write record out)
            (finish-output out)
            t))))))

(defun strip-tombstones (map)
  "Strip key-value pairs with tombstone values from MAP."
  (let ((out map))
    (fset:do-map (k v map)
      (when (eq v tombstone)
        (lessf out k)))
    out))

(defun load-log-data (log-file)
  "Load the data from LOG-FILE."
  (declare (optimize safety debug))
  (if (not (file-exists-p log-file))
      no-log-data
      (tagbody
       :retry
         (restart-case
             (return-from load-log-data
               (with-standard-input-syntax
                 (let* ((*readtable* db-readtable)
                        (records
                          (with-input-from-file (in log-file :element-type 'character)
                            (let ((eof "eof"))
                              (nlet rec ((records '()))
                                (let ((data (read in nil eof)))
                                  (cond ((eq data eof)
                                         (nreverse records))
                                        ((typep data 'log-record)
                                         (rec (cons data records)))
                                        (t
                                         (error (db-error "Invalid database log entry: ~a"
                                                          data)))))))))
                        (maps
                          (mapcar #'log-record.data records))
                        (map
                          (reduce #'fset:map-union maps
                                  :initial-value (fset:empty-map)))
                        (map (strip-tombstones map)))
                   (make-log-data
                    :map map
                    :map-count (length maps)))))
           (retry ()
             :report "Try loading the database again."
             (go :retry))
           (truncate-db ()
             :report "Treat the database as corrupt and discard it."
             (delete-file-if-exists log-file)
             no-log-data)))))

(defun squash-data (log-data log-file)
  "If needed, write a compacted version of LOG-DATA into LOG-FILE."
  (let ((map (log-data.map log-data))
        (map-count (log-data.map-count log-data))
        temp)
    (when (> map-count 1)
      (message "Compacting log-file")
      (with-temporary-file (:stream s
                            :pathname p
                            :keep t
                            :direction :output
                            :element-type 'character
                            ;; Ensure the temp file is on the
                            ;; same file system as the log-file so the
                            ;; rename is atomic.
                            :directory (pathname-directory-pathname log-file))
        (setq temp p)
        (db-write (make-log-record :data map) s))
      (rename-file-overwriting-target temp log-file))
    log-file))

(defun log-file-path (&optional (version (db-version)))
  "Compute the path of the log file for the current database version."
  (assure absolute-pathname
    (path-join
     (current-cache-dir version)
     #p"log/"
     #p"log.sexp")))

(defun reload-db ()
  "Reload the current version of the database from its log file."
  (when (in-worker?)
    (error (db-error "Cannot load the DB from within a worker.")))
  (lret* ((log-file (log-file-path))
          (log-data
           (progn
             (message "Reloading database (~:d byte~:p)"
                      (log-file-size log-file))
             (load-log-data log-file)))
          (map (log-data.map log-data))
          (db (make-db
               :log-file log-file
               :current-map map
               :last-saved-map map)))
    (make-thread
     (lambda ()
       (synchronized (db)
         (squash-data log-data log-file)))
     :name "Overlord: compacting log file")))

(define-global-state *db* nil
  "The database.")
(register-worker-special '*db*)

(defun in-worker? ()
  (true (kernel-worker-index)))

(defun db ()
  "Get the current database, loading it if necessary."
  (synchronized ('*db)
    (ensure *db*
      (reload-db))
    (check-version)
    (lock-db *db*))
  *db*)

(defun db-loaded? ()
  (not (null *db*)))

(defun require-db ()
  "Load the DB."
  (unless (db-loaded?)
    (db)
    (assert (db-loaded?))))

(defun unload-db ()
  "Clear the DB out of memory in such a way that it can still be
reloaded on demand."
  ;; TODO Force a full GC afterwards?
  (synchronized ('*db*)
    (unlock-db *db*)
    (nix *db*)))

(defun deactivate-db ()
  "Clear the DB out of memory in such a way that it will not be reloaded on demand."
  (synchronized ('*db*)
    (unlock-db *db*)
    (setq *db* (make-dead-db))))

(defun check-version ()
  "Check that the database version matches the Overlord system version."
  (let ((new-version (db-version))
        (old-version (db-version)))
    (unless (= new-version old-version)
      (cerror "Reload the database"
              'version-mismatch
              :new-version new-version
              :old-version old-version)
      (setq *db* (reload-db)))))

(defplace db-ref* (key)
  (db.ref (db) key)
  "Access KEY in the current database.")

(defgeneric db-protect (x)
  (:documentation "Try to avoid writing symbols or package objects into the database.
This allows the database to be reloaded without those packages being
required.")
  (:method ((x symbol))
    (eif (memq (symbol-package x)
               '#.(list (find-package :keyword)
                        (find-package :cl)
                        (find-package :overlord/db)))
        x
        (let* ((p (symbol-package x)))
          (cons (and p (package-name p))
                (symbol-name x)))))
  (:method ((x package))
    (cons :package (package-name x)))
  (:method (x) x))

(defun prop-key (obj prop)
  "Convert OBJ and PROP into a single key."
  (cons (db-protect obj)
        (db-protect prop)))

(defplace prop-1 (obj prop)
  (db.ref (db) (prop-key obj prop))
  "Access the database record keyed by OBJ and PROP.")

(defun prop (obj prop &optional default)
  "Look up a property for an object in the database."
  (receive (val val?) (prop-1 obj prop)
    (if val?
        (values val t)
        (values default nil))))

(defun (setf prop) (value obj prop &optional default)
  "Set an object's property in the database."
  (declare (ignore default))
  (setf (prop-1 obj prop) value))

(defun has-prop? (obj prop &rest props)
  "Test if an object has a property in the database."
  (some (op (nth-value 1 (prop-1 obj _)))
        (cons prop props)))

(defun has-props? (obj prop &rest props)
  "Check if an object in the database has all of the supplied
properties."
  (every (op (nth-value 1 (prop-1 obj _)))
         (cons prop props)))

(defun delete-prop (obj prop)
  "Delete a property from OBJ."
  (db.del (db) (prop-key obj prop)))

(defun save-database ()
  "Save the current database."
  (db.sync (db))
  (values))

(defun release-database ()
  (when *db*
    (save-database)
    (unload-db)))

(add-exit-hook #'release-database)

(defun call/saving-database (thunk)
  "Call THUNK, saving the database afterwards, unless a save is
already pending."
  (if *save-pending*
      (funcall thunk)
      (let ((*save-pending* t))
        (db)                            ;Ensure the DB is loaded.
        (unwind-protect
             (funcall thunk)
          (save-database)))))

(defmacro saving-database (&body body)
  "Macro wrapper for `call/saving-database'."
  (with-thunk (body)
    `(call/saving-database ,body)))
