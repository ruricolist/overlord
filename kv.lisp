(defpackage :overlord/kv
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
    :merge-pathnames*
    :pathname-directory-pathname)
  (:import-from :fset)
  (:import-from :local-time)
  (:export
   :prop :has-prop? :delete-prop
   :save-database
   :compact-database
   :saving-database
   :unload-db
   :deactivate-db))
(in-package :overlord/kv)

(deftype kv-key ()
  '(not null))

(deftype kv-value ()
  't)

(defgeneric kv.ref (kv key))
(defgeneric (setf kv.ref) (value kv key))
(defgeneric kv.del (kv key))
(defgeneric kv.sync (kv))
(defgeneric kv.squash (kv))

(defconst tombstone '%tombstone)

(defstruct-read-only (log-record (:conc-name log-record.))
  (timestamp (get-universal-time) :type (integer 0 *))
  (data :type fset:map))

(defclass kv ()
  ((version
    :initform *fasl-version*
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

(defmethods kv (self version current-map last-saved-map log)
  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      ;; version record-count byte-count saved or not?
      (format stream "v.~a ~d record~:p, ~:d byte~:p~@[ (~a)~]"
              version
              (fset:size current-map)
              (let ((log log))
                (if (file-exists-p log)
                    (file-size log)
                    0))
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
      (sb-ext:atomic-update (slot-value kv 'current-map)
                            (lambda (map)
                              (fset:with map key value)))
      #-sbcl
      (synchronized (self)
        (setf current-map
              (fset:with current-map
                         key value)))))

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

(defun kv-write (obj stream)
  (with-standard-io-syntax
    (write obj :stream stream
               :readably t
               :pretty nil
               :circle nil)))

(defun log.update (log last-saved-map current-map)
  (unless (eql last-saved-map current-map)
    (let ((diff (fset:map-difference-2 current-map last-saved-map)))
      (unless (fset:empty? diff)
        (let ((record (make-log-record :data diff)))
          (with-standard-io-syntax
            (with-output-to-file (out (ensure-directories-exist log)
                                      :element-type 'character
                                      :if-does-not-exist :create
                                      :if-exists :append)
              (kv-write record out)
              (finish-output out))))))))

(defun map-union/tombstones (map1 map2)
  (fset:do-map (k v map2)
    (setf map1
          (if (eql v tombstone)
              (fset:less map1 k)
              (fset:with map1 k v))))
  map1)

(defun log.load (log)
  (if (not (file-exists-p log))
      (values (fset:empty-map) 0)
      (restart-case
          (let* ((*readtable* kv-readtable)
                 ;; So symbols can be read properly.
                 (*package* (find-package :keyword))
                 (records
                   (with-standard-io-syntax
                     (with-input-from-file (in log :element-type 'character)
                       ;; TODO ignore errors?
                       (loop with eof = "eof"
                             for record = (read in nil eof)
                             until (eq record eof)
                             collect record))))
                 (maps
                   (mapcar #'log-record.data records)))
            (values
             (reduce #'map-union/tombstones maps
                     :initial-value (fset:empty-map))
             (length maps)))
        (truncate-db ()
          :report "Ignore the corrupt database"
          (return-from log.load
            (values (fset:empty-map) 0))))))

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
        (kv-write map s))
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
  (merge-pathnames*
   #p"overlord.log"
   (xdg-cache-home "overlord"
                   (fmt "v~a" *fasl-version*)
                   :implementation
                   "log")))

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
             *fasl-version*)
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
              (cons (package-name (symbol-package x))
                    (symbol-name x))))
      x))

(defun prop-key (obj prop)
  (cons (unqualify-symbol obj)
        (unqualify-symbol prop)))

(defplace prop (obj prop)
  (kv.ref (kv) (prop-key obj prop)))

(defun has-prop? (obj prop)
  (nth-value 1 (prop-key obj prop)))

(defun delete-prop (obj prop)
  (kv.del (kv) (prop-key obj prop)))

(defun save-database ()
  (message "Saving Overlord database")
  (kv.sync (kv)))

(defun compact-database ()
  (kv.squash (kv)))

(defvar *save-pending* nil)

(defun call/saving-database (thunk)
  (if *save-pending*
      (funcall thunk)
      (let ((*save-pending* t))
        (funcall thunk)
        (save-database))))

(defmacro saving-database (&body body)
  (with-thunk (body)
    `(call/saving-database ,body)))
