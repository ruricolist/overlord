(defpackage :overlord/db
  (:use :cl :alexandria :serapeum
    :overlord/specials
    :overlord/types
    :overlord/message
    :overlord/global-state)
  (:import-from :uiop
    :with-temporary-file
    :rename-file-overwriting-target
    :file-exists-p
    :xdg-cache-home
    :subpathp
    :ensure-directory-pathname
    :merge-pathnames*
    :pathname-directory-pathname)
  (:import-from :trivial-file-size :file-size-in-octets)
  (:import-from :fset)
  (:import-from :local-time)
  (:export
   :prop :has-prop? :delete-prop
   :save-database
   :compact-database
   :saving-database
   :unload-db
   :deactivate-db
   :delete-versioned-db
   :db-version-dir))
(in-package :overlord/db)

(deftype kv-key ()
  '(not null))

(deftype kv-value ()
  't)

(define-modify-macro withf (&rest item-or-tuple) fset:with)
(define-modify-macro lessf (&rest item-or-tuple) fset:less)

(defgeneric kv.ref (kv key))
(defgeneric (setf kv.ref) (value kv key))
(defgeneric kv.del (kv key))
(defgeneric kv.sync (kv))
(defgeneric kv.squash (kv))

(defunit tombstone)

(defstruct-read-only (log-record (:conc-name log-record.))
  (timestamp (get-universal-time) :type (integer 0 *))
  (data :type fset:map))

(defun delete-versioned-db (&optional (version (db-version)))
  (let ((dir (db-version-dir version)))
    (when (uiop:directory-exists-p dir)
      (uiop:delete-directory-tree
       dir
       :validate (op (subpathp _ (xdg-cache-home)))))))

(defun db-version-dir (&optional (version (db-version)))
  (ensure-directory-pathname
   (xdg-cache-home "overlord"
                   (fmt "v~a" version)
                   :implementation)))

(defclass kv ()
  ((version
    :initform (db-version)
    :reader kv.version)
   (current-map
    :initarg :current-map
    :type fset-map
    :accessor kv.current-map)
   (last-saved-map
    :initarg :last-saved-map
    :type fset-map
    :accessor kv.last-saved-map)
   (log
    :initarg :log
    :type :pathname
    :reader kv.log))
  (:default-initargs
   :current-map (fset:empty-map)
   :last-saved-map (fset:empty-map)
   :log (log-file-path)))

(defun kv-alist (kv)
  "For debugging."
  (let ((map (kv.current-map kv)))
    (collecting
      (fset:do-map (k v map)
        (collect (cons k v))))))

(defmethods kv (self version current-map last-saved-map log)
  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      ;; version record-count byte-count saved or not?
      (format stream "v.~a ~d record~:p, ~:d byte~:p~@[ (~a)~]"
              version
              (fset:size current-map)
              (if (file-exists-p log)
                  (file-size-in-octets log)
                  0)
              (and (not (eql current-map last-saved-map))
                   "unsaved"))))

  (:method kv.ref (self key)
    (multiple-value-bind (value bool)
        (fset:lookup current-map
                     (assure kv-key key))
      (if (eq value tombstone)
          (values nil nil)
          (values (assure kv-value value)
                  (assure boolean bool)))))

  (:method (setf kv.ref) (value self key)
    (check-type key kv-key)
    (check-type value kv-value)
    (prog1 value
      ;; TODO More CAS.
      #+sbcl
      (sb-ext:atomic-update (slot-value self 'current-map)
                            (lambda (map)
                              (fset:with map key value)))
      #-sbcl
      (synchronized (self)
        (withf current-map key value))))

  (:method kv.del (self key)
    (setf (kv.ref self key) tombstone)
    (values))

  (:method kv.sync (self)
    (synchronized (self)
      (log.update log last-saved-map current-map)
      (setf last-saved-map current-map)))

  (:method kv.squash (self)
    (synchronized (self)
      (kv.sync self)
      (log.squash log))))

(defclass dead-kv (kv)
  ())

(defmethods dead-kv (self)
  (:method kv.ref (self key)
    (declare (ignore key))
    (values nil nil))
  (:method (setf kv.ref) (value self key)
    (declare (ignore key))
    value)
  (:method kv.del (self key)
    (declare (ignore key))
    (values))
  (:method kv.sync (self)
    (values))
  (:method kv.squash (self)
    (values)))

;;; TODO use a binary representation for the log.

(def kv-readtable
  (lret ((*readtable*
          (copy-readtable fset::*fset-rereading-readtable*)))
    (local-time:enable-read-macros)))

(def standard-input-syntax-vars
  '(*package*
    *read-base*
    *read-default-float-format*
    *read-eval*
    *read-suppress*
    *readtable*)
  "Reader control variables bound by `with-standard-io-syntax'.")

(def standard-input-syntax-values
  (with-standard-io-syntax
    (mapcar #'symbol-value standard-input-syntax-vars))
  "Values of the standard reader control variables.")

(defun call/standard-input-syntax (fn)
  (progv standard-input-syntax-vars
      standard-input-syntax-values
    (funcall fn)))

(defmacro with-standard-input-syntax (&body body)
  "Like `with-standard-io-syntax', but only bind the variables that
control the reader, not the printer.

This may be preferable to using `with-standard-io-syntax' when loading
data, as it will not effect how errors are printed, thus preserving
debugging information."
  (with-thunk (body)
    `(call/standard-input-syntax ,body)))

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

(defun kv-write (obj stream)
  (with-standard-io-syntax*
    ;; It's possible a writer may look at the current readtable.
    (let ((*readtable* kv-readtable))
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
            (kv-write record out)
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
                 (let* ((*readtable* kv-readtable)
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
             (return-from log.load
               (values (fset:empty-map) 0)))))))

(defun log.squash (log)
  (mvlet ((map map-count (log.load log))
         temp)
    (when (> map-count 1)
      (message "Compacting log")
      (uiop:with-temporary-file (:stream s
                                 :pathname p
                                 :keep t
                                 :direction :output
                                 :element-type 'character
                                 ;; Ensure the temp file is on the
                                 ;; same file system as the log.
                                 :directory (pathname-directory-pathname log))
        (setq temp p)
        (kv-write (make-log-record :data map) s))
      (rename-file-overwriting-target temp log)))
  log)

(defun empty-kv ()
  (make 'kv))

(defun load-kv (log)
  (let ((map (log.load log)))
    (make 'kv
          :current-map map
          :last-saved-map map
          :log log)))

(defun log-file-path ()
  (assure absolute-pathname
    (path-join
     (db-version-dir)
     #p"log/"
     #p"log")))

(defun reload-kv ()
  (message "Reloading database")
  (let ((path (log-file-path)))
    (log.squash path)
    (load-kv path)))

(define-global-state *kv* nil)

(defun kv ()
  (synchronized ('*kv)
    (ensure-kv)
    (check-version))
  *kv*)

(defun ensure-kv ()
  (ensure *kv*
    (reload-kv)))

(defun unload-db ()
  "Clear the DB out of memory in such a way that it can still be
reloaded on demand."
  ;; TODO Force a full GC afterwards?
  (synchronized ('*kv*)
    (nix *kv*)))

(defun deactivate-db ()
  "Clear the DB out of memory in such a way that it will not be reloaded on demand."
  (synchronized ('*kv*)
    (setq *kv* (make 'dead-kv))))

(defun check-version ()
  (unless (= (kv.version *kv*)
             (db-version))
    (cerror "Load the correct database"
            "Database version mismatch")
    (setq *kv* (reload-kv))))

(defplace kv-ref* (key)
  (kv.ref (kv) key))

;;; But should be manipulating stored plists instead?

(defun unqualify-symbol (x)
  (eif (symbolp x)
      (eif (keywordp x)
          x
          (eif (eql (symbol-package x)
                    #.(find-package :cl))
              x
              (let* ((p (symbol-package x)))
                (cons (and p (package-name p))
                      (symbol-name x)))))
      x))

(defun prop-key (obj prop)
  (cons (unqualify-symbol obj)
        (unqualify-symbol prop)))

(defplace prop-1 (obj prop)
  (kv.ref (kv) (prop-key obj prop)))

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
  (kv.del (kv) (prop-key obj prop)))

(defun save-database-message (time-units)
  (let ((seconds (save-database-seconds? time-units)))
    (message "Saving database~@[ (after ~as)~]."
             seconds)))

(defun time-units->seconds (time-units)
  (/ time-units
     internal-time-units-per-second))

(defun save-database-seconds? (time-units)
  (when time-units
    (let ((seconds (float (time-units->seconds time-units))))
      (when (> seconds 0)
        seconds))))

(defun save-database (&optional time-units)
  (save-database-message time-units)
  (kv.sync (kv))
  (values))

(uiop:register-image-dump-hook 'save-database)

(defun compact-database ()
  (kv.squash (kv)))

(defun call/saving-database (thunk)
  (if *save-pending*
      (funcall thunk)
      (let ((*save-pending* t)
            (start (get-internal-real-time)))
        (unwind-protect
             (funcall thunk)
          (let ((end (get-internal-real-time)))
            (save-database (- end start)))))))

(defmacro saving-database (&body body)
  (with-thunk (body)
    `(call/saving-database ,body)))
