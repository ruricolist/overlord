(defpackage :overlord/kv
  (:use :cl :alexandria :serapeum
    :overlord/specials
    :overlord/message
    :overlord/global-state)
  (:import-from :uiop
    :with-temporary-file
    :rename-file-overwriting-target
    :file-exists-p
    :xdg-cache-home
    :merge-pathnames*)
  (:import-from :fset)
  (:import-from :local-time)
  (:export
   :prop :has-prop? :delete-prop
   :save-database
   :compact-database
   :saving-database))
(in-package :overlord/kv)

(deftype kv-key ()
  '(not null))

(deftype kv-value ()
  't)

(defconst tombstone '%tombstone)

(defclass kv ()
  ((current-map
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
    :accessor kv.log)))

(defun kv.ref (kv key)
  (multiple-value-bind (value bool)
      (fset:lookup (kv.current-map kv)
                   (assure kv-key key))
    (if (eql value tombstone)
        (values nil nil)
        (values (assure kv-value value)
                (assure boolean bool)))))

(defun (setf kv.ref) (value kv key)
  (check-type key kv-key)
  (check-type value kv-value)
  (prog1 value
    ;; TODO More CAS.
    #+sbcl
    (sb-ext:atomic-update (slot-value kv 'current-map)
                          (lambda (map)
                            (fset:with map key value)))
    #-sbcl
    (synchronized (kv)
      (setf (kv.current-map kv)
            (fset:with (kv.current-map kv)
                       key value)))))

(defun kv.del (kv key)
  (setf (kv.ref kv key) tombstone))

;;; TODO use a binary representation for the log.

(def kv-readtable
  (lret ((*readtable*
          (copy-readtable fset::*fset-rereading-readtable*)))
    (local-time:enable-read-macros)))

(defun kv-write (obj stream)
  (write obj :stream stream
             :readably t
             :pretty nil
             :circle nil))

(defun log.update (log last-saved-map current-map)
  (unless (eql last-saved-map current-map)
    (let ((diff (fset:map-difference-2 current-map last-saved-map)))
      (unless (fset:empty? diff)
        (with-open-file (out log
                             :direction :output
                             :element-type 'character
                             :if-does-not-exist :create
                             :if-exists :append)
          (kv-write diff out)
          (finish-output out))))))

(defun map-union/tombstones (map1 map2)
  (fset:do-map (k v map2)
    (setf map1
          (if (eql v tombstone)
              (fset:less map1 k)
              (fset:with map1 k v))))
  map1)

(defun log.load (log)
  (if (not (file-exists-p log)) (fset:empty-map)
      (let* ((*readtable* kv-readtable)
             ;; So symbols can be read properly.
             (*package* (find-package :keyword))
             (maps
               (with-input-from-file (in log)
                 ;; TODO ignore errors?
                 (loop for map = (read in nil nil)
                       while (typep map 'fset:map)
                       collect map))))
        (values
         (reduce #'map-union/tombstones maps
                 :initial-value (fset:empty-map))
         (length maps)))))

(defun log.squash (log)
  (mvlet ((map map-count (log.load log))
         temp)
    (when (> map-count 1)
      (message "Compacting log")
      (uiop:with-temporary-file (:stream s :pathname p :keep t
                                 :direction :output
                                 :element-type 'character)
        (setq temp p)
        (kv-write map s))
      (rename-file-overwriting-target temp log)))
  log)

(defun kv.sync (kv)
  (with-slots (last-saved-map current-map log) kv
    (synchronized (kv)
      (log.update log last-saved-map current-map)
      (setf last-saved-map current-map))))

(defun kv.squash (kv)
  (synchronized (kv)
    (kv.sync kv)
    (log.squash (kv.log kv))))

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

(define-global-state *kv*
  (progn
    (message "Reloading database")
    (let ((path (log-file-path)))
      (log.squash path)
      (load-kv path))))

(defplace kv-ref* (key)
  (kv.ref *kv* key))

;;; But should be manipulating stored plists instead?

(defplace prop (obj prop)
  (kv.ref *kv* (cons obj prop)))

(defun has-prop? (obj prop)
  (nth-value 1 (prop obj prop)))

(defun delete-prop (obj prop)
  (kv.del *kv* (cons obj prop)))

(defun save-database ()
  (message "Saving Overlord database")
  (kv.sync *kv*))

(defun compact-database ()
  (kv.squash *kv*))

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
