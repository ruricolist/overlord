;;;; The current base.

(defpackage :overlord/base
  (:use :cl :alexandria :serapeum
    :overlord/types
    :overlord/global-state
    :overlord/asdf)
  (:import-from :overlord/specials
    :*base* :*cli* :use-threads-p)
  (:import-from :named-readtables
    :find-readtable)
  (:import-from :uiop
    :pathname-directory-pathname
    :absolute-pathname-p
    :directory-pathname-p
    :merge-pathnames*
    :chdir :getcwd :pathname-equal
    :*nil-pathname*)
  (:import-from :overlord/util
    :locate-dominating-file)
  (:export
   :current-dir!
   :*base* :base
   :set-package-base
   :base-relative-pathname
   :infer-system
   :ensure-absolute
   :with-current-dir))

(in-package :overlord/base)

(deftype absolute-directory-pathname ()
  '(and absolute-pathname directory-pathname))

(defun absolute-directory-pathname? (x)
  "Is X an absolute directory pathname?"
  (and (absolute-pathname-p x)
       (directory-pathname-p x)))

(defun absolute-directory-pathname (x)
  "Resolve X as an absolute directory pathname."
  (assure absolute-directory-pathname
    (if (absolute-directory-pathname? x) x
        (let ((x (truename x)))
          (if (directory-pathname-p x) x
              (pathname-directory-pathname x))))))

(-> current-dir! () absolute-directory-pathname)
(defun current-dir! ()
  "Return the current directory.

If `*default-pathname-defaults*' is an absolute directory pathname, return that.

Otherwise, resolve `*default-pathname-defaults*' to an absolute directory, set `*default-pathname-defaults*' to the new value, and return the new value."
  (if (use-threads-p)
      (let ((dpd *default-pathname-defaults*))
        (if (absolute-directory-pathname? dpd) dpd
            (setf *default-pathname-defaults*
                  (absolute-directory-pathname dpd))))
      (lret ((dpd *default-pathname-defaults*)
             (cwd (getcwd)))
        (unless (pathname-equal cwd dpd)
          (setf *default-pathname-defaults* cwd)))))

(defun (setf current-dir!) (dir)
  (lret ((dir (absolute-directory-pathname dir)))
    (ensure-directories-exist dir)
    (unless (use-threads-p)
      (unless (pathname-equal dir (getcwd))
        (chdir dir)))
    (unless (pathname-equal dir *default-pathname-defaults*)
      (setf *default-pathname-defaults* dir))))

(defun call/current-dir (thunk dir)
  (let ((*default-pathname-defaults* *nil-pathname*))
    (setf (current-dir!) dir)
    (funcall thunk)))

(defmacro with-current-dir ((dir &key) &body body)
  (with-thunk (body)
    `(call/current-dir ,body ,dir)))

(defun ensure-absolute (pathname)
  (assure absolute-pathname
    (etypecase pathname
      (absolute-pathname pathname)
      (relative-pathname
       (merge-pathnames* pathname (base))))))

(define-global-state *package-bases*
    (dict* (make-hash-table)
           (find-package :cl-user) (user-homedir-pathname)))

(defun set-package-base* (base &optional (system nil system-supplied?))
  "Set the base for the current package.
If SYSTEM is supplied, resolve BASE as a system-relative pathname."
  (setf (gethash *package* *package-bases*)
        (assure (and absolute-pathname directory-pathname)
          (if system-supplied?
              (asdf-system-relative-pathname system base)
              base))))

(defmacro set-package-base (base &optional (system nil system-supplied?))
  `(eval-always
     (set-package-base* ,base
                        ,@(if system-supplied? (list system) nil))))

(define-global-state *supplied-package-systems* (make-hash-table))

(defun base-relative-pathname (pathname)
  (merge-pathnames pathname (base)))

(defun base ()
  #+ () (or *compile-file-truename*
            *load-truename*)
  (absolute-directory-pathname
   (if (boundp '*base*) *base*
       (infer-base-from-package))))

(defun infer-base-1 (&key (errorp t))
  (or (gethash *package* *package-bases*)
      (system-base (infer-system :errorp errorp))))

(defun infer-base-from-package (&key (errorp t))
  (let ((base (infer-base-1 :errorp errorp)))
    (if (absolute-pathname-p base)
        base
        (and errorp
             (error* "Cannot infer base.~%Package: ~a~%File: "
                     *package*
                     (current-lisp-file))))))

(defun find-system (system &optional error-p)
  (let ((*readtable* (find-readtable :standard))
        (*read-base* 10)
        (*read-default-float-format* 'double-float))
    (find-asdf-system system :error (not error-p))))

(defun system-base (system)
  (setf system (find-system system))
  (let ((base (asdf-system-relative-pathname system "")))
    (if (absolute-pathname-p base)
        base
        (if (package-inferred-asdf-system? system)
            (let* ((system-name (primary-asdf-system-name system))
                   (base-system-name (take-while (op (not (eql _ #\/))) system-name))
                   (base-system (find-system base-system-name)))
              (system-base base-system))
            (error* "System ~a has no base." system)))))

(defun infer-system (&key (errorp t))
  (assure (or null (satisfies asdf-system?))
    (or (infer-system-from-package)
        (look-for-asd)
        (and errorp
             (assure (satisfies asdf-system?)
               (ensure2 (gethash *package* *supplied-package-systems*)
                 (cerror* "Supply a system name"
                          "Cannot infer a system for ~a" *package*)
                 (read-system-by-name)))))))

(defun read-system-by-name ()
  (format t "~&Type a system name: ")
  (let ((name (make-keyword (string (read)))))
    (or (find-system name nil)
        (progn
          (cerror* "Supply another name"
                   "No such system as ~a" name)
          (read-system-by-name)))))

(defun current-lisp-file ()
  (or *compile-file-truename* *load-truename*))

(defun infer-system-from-package (&optional (package *package*))
  (or (infer-system-from-package-names package)
      (infer-system-from-package-prefix package)))

(defun infer-system-from-package-names (package)
  (some #'guess-system-from-package-name
        (package-names package)))

(defun infer-system-from-package-prefix (package)
  (let ((name (package-name package)))
    (and (find #\/ name)
         (let ((prefix (first (split-sequence #\/ name :count 1))))
           (and prefix
                (guess-system-from-package-name
                 (string-downcase prefix)))))))

(defun guess-system-from-package-name (name)
  (when-let (guess (package-name-asdf-system name))
    (find-system guess nil)))

(defun package-names (p)
  (cons (package-name p)
        (package-nicknames p)))

(defun look-for-asd ()
  "Look for the nearest .asd file and return its name."
  (and-let* ((file (current-lisp-file))
             ((not (typep file 'temporary-file)))
             (.asd (nearest-asdf-file file)))
    (find-system (pathname-name .asd) nil)))

(defun nearest-asdf-file (file)
  (locate-dominating-file file "*.asd"))
