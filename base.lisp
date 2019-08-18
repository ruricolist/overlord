;;;; The current base.

(defpackage :overlord/base
  (:use :cl :alexandria :serapeum
    :overlord/types
    :overlord/global-state
    :overlord/asdf)
  (:import-from :overlord/specials
    :*base* :*cli*
    :*base-package* :base-package)
  (:import-from :uiop
    :pathname-directory-pathname
    :absolute-pathname-p
    :directory-pathname-p
    :merge-pathnames*
    :pathname-equal
    :*nil-pathname*)
  (:import-from :overlord/util
    :locate-dominating-file
    :ensure-pathname*)
  (:export
   :current-dir!
   :*base* :base
   :set-package-base
   :base-relative-pathname
   :ensure-absolute
   :with-current-dir
   :package-base
   :package-system
   :current-system
   :resolve-file))

(in-package :overlord/base)

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
  (let ((dpd *default-pathname-defaults*))
    (if (absolute-directory-pathname? dpd) dpd
        (setf *default-pathname-defaults*
              (absolute-directory-pathname dpd)))))

(defun (setf current-dir!) (dir)
  (lret ((dir (absolute-directory-pathname dir)))
    (ensure-directories-exist dir)
    (unless (pathname-equal dir *default-pathname-defaults*)
      (setf *default-pathname-defaults* dir))))

(defun call/current-dir (thunk dir)
  (ensure-directories-exist dir)
  (let ((*default-pathname-defaults* *nil-pathname*))
    (setf (current-dir!) dir)
    (funcall thunk)))

(defmacro with-current-dir ((dir &key) &body body)
  (with-thunk (body)
    `(call/current-dir ,body ,dir)))

(defun ensure-absolute (pathname &key (base (base)))
  (assure absolute-pathname
    (etypecase pathname
      (absolute-pathname pathname)
      (relative-pathname
       (merge-pathnames* pathname base)))))

(defun resolve-file (file &key (base (base)))
  (~> file
      ensure-pathname*
      (ensure-absolute :base base)))

(deftype pkg-base-spec ()
  "One of the three ways of specifying the base of a package: (1) an
  absolute pathname, (2) a system (whose base should be used), or (3)
  a pair of a system and a relative pathname (in which case the
  relative pathname should be merged with the system base)."
  '(or
    asdf-system absolute-pathname
    (cons asdf-system relative-pathname)))

(define-global-state *package-bases*
    (dict* (make-hash-table)
           (find-package :cl-user) (user-homedir-pathname)))

(defun set-package-base-1 (package base system)
  "Set the base and/or system of PACKAGE."
  (setf package (find-package package))
  (setf (gethash package *package-bases*)
        (assure pkg-base-spec
          (econd
            ((and base system)
             (cons (find-asdf-system system) base))
            (system (find-asdf-system system))
            (base
             (assure absolute-directory-pathname
               base))
            (t (error "No path or system."))))))

(defun set-package-base* (base &optional system)
  "Set the base and/or system, for the current package."
  (set-package-base-1 (base-package) base system))

(defmacro set-package-base (base &optional system)
  "Set the base and/or system, for the current package, at compile
time as well as load time."
  `(eval-always
     (set-package-base* ,base ,system)))

(defun base ()
  "Return the current base, which is either the current value of
`*base*' (if that is bound) or the base of the current package."
  #+(or) (or *compile-file-truename* *load-truename*)
  (absolute-directory-pathname
   (if (boundp '*base*) *base*
       (package-base (base-package)))))

(defun saved-package-base (package)
  "If a base has been set for PACKAGE, return it."
  (setf package (find-package package))
  (let ((spec (gethash package *package-bases*)))
    (and spec
         (etypecase-of pkg-base-spec spec
           (asdf-system (asdf-system-base spec))
           (absolute-pathname spec)
           ((cons asdf-system relative-pathname)
            (asdf-system-relative-pathname (car spec) (cdr spec)))))))

(defun saved-package-system (package)
  "If a system has been set for PACKAGE, return it."
  (setf package (find-package package))
  (let ((spec (gethash package *package-bases*)))
    (and spec
         (etypecase-of pkg-base-spec spec
           (asdf-system spec)
           (absolute-pathname nil)
           ((cons asdf-system relative-pathname)
            (car spec))))))

(defun package-base (package &key (errorp t))
  "Retrieve or infer the base of PACKAGE."
  (setf package (find-package package))
  (let* ((base
           (or (saved-package-base package)
               (asdf-system-base
                (package-system package :errorp errorp)))))
    (if (absolute-pathname-p base) base
        (and errorp
             (error* "Cannot infer base.~%Package: ~a~%File: ~a"
                     package
                     (current-lisp-file))))))

(defun current-system ()
  "Retrieve or infer the system the current package comes from."
  (package-system (base-package)))

(defun package-system (package &key errorp)
  "Retrieve or infer the system PACKAGE comes from."
  (or (saved-package-system package)
      (infer-system package :errorp errorp)))

(defun infer-system (package &key (errorp t))
  (setf package (find-package package))
  (assure (or null asdf-system)
    (or (infer-system-from-package package)
        (look-for-asd)
        (and errorp
             (setf (gethash package *package-bases*)
                   (assure asdf-system
                     (progn
                       (cerror* "Supply a system name"
                                "Cannot infer a system for ~a.

To avoid this error in the future, use ~s."
                                (base-package)
                                'set-package-base)
                       (read-system-by-name))))))))

(defun read-system-by-name ()
  (format t "~&Type a system name: ")
  (assure asdf-system
    (let ((name (make-keyword (string (read)))))
      (or (find-asdf-system name :error nil)
          (progn
            (cerror* "Supply another name"
                     "No such system as ~a" name)
            (read-system-by-name))))))

(defun current-lisp-file ()
  (or *compile-file-truename* *load-truename*))

(defun infer-system-from-package (&optional (package (base-package)))
  (or (infer-system-from-package-names package)
      (infer-system-from-package-affix package)))

(defun infer-system-from-package-names (package)
  (some #'guess-system-from-package-name
        (package-names package)))

(defun infer-system-from-package-affix (package)
  (let ((name (package-name package)))
    (or (and (find #\/ name)
             (let ((prefix (first (split-sequence #\/ name :count 1))))
               (and prefix
                    (guess-system-from-package-name
                     (string-downcase prefix)))))
        (let ((-user "-USER"))
          (and (string$= -user name)
               (let ((pkg
                       (find-package
                        (slice name 0 (- (length -user))))))
                 (and pkg
                      (infer-system-from-package pkg))))))))

(defun guess-system-from-package-name (name)
  (when-let (guess (package-name-asdf-system name))
    (find-asdf-system guess :error nil)))

(defun look-for-asd ()
  "Look for the nearest .asd file and return its name."
  (and-let* ((file (current-lisp-file))
             ((not (typep file 'temporary-file)))
             (.asd (nearest-asdf-file file)))
    (find-asdf-system (pathname-name .asd) :error nil)))

(defun nearest-asdf-file (file)
  (locate-dominating-file file "*.asd"))
