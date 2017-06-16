;;;; The current base.

(defpackage :overlord/base
  (:use :cl :alexandria :serapeum
    :overlord/types
    :overlord/global-state)
  (:import-from :overlord/specials
    :*base* :*cli*
    :ensure-absolute)
  (:import-from :uiop
    :pathname-directory-pathname
    :absolute-pathname-p
    :merge-pathnames*
    :chdir :getcwd :pathname-equal)
  (:import-from :overlord/util
    :locate-dominating-file)
  (:export
   :current-dir!
   :build-env :build-env-case
   :*base* :base
   :set-package-base
   :base-relative-pathname
   :with-defaults-from-base
   :hygienic-pathnames
   :infer-system))

(in-package :overlord/base)

(defun current-dir! ()
  (assure (and absolute-pathname directory-pathname)
    (let ((cwd (getcwd))
          (dpd *default-pathname-defaults*))
      (unless (pathname-equal cwd dpd)
        (setf *default-pathname-defaults* cwd))
      cwd)))

(defun (setf current-dir!) (dir)
  (check-type dir (and absolute-pathname directory-pathname))
  (unless (pathname-equal dir (getcwd))
    (chdir dir))
  (unless (pathname-equal dir *default-pathname-defaults*)
    (setf *default-pathname-defaults* dir))
  dir)

(defun call/current-dir! (thunk dir)
  (setf (current-dir!) dir)
  (funcall thunk))

(defun ensure-absolute (pathname)
  (assure absolute-pathname
    (etypecase pathname
      (absolute-pathname pathname)
      (relative-pathname
       (merge-pathnames* pathname (current-dir!))))))

(deftype build-env ()
  '(member :cli :repl :compile-file :load-file))

(defun build-env ()
  (assure build-env
    (cond (*cli* :cli)
          (*compile-file-truename* :compile-file)
          (*load-truename* :load-file)
          (t :repl))))

(defmacro build-env-case (&body clauses)
  `(ecase-of build-env (build-env)
     ,@clauses))

(defmacro with-defaults-from-base (&body body)
  "Wrapper for `call-with-defaults-from-base'."
  `(let ((*default-pathname-defaults*
           (pathname-directory-pathname
            *base*)))
     ,@body))

(define-global-state *package-bases* (make-hash-table))

(defun set-package-base* (base &optional (system nil system-supplied?))
  "Set the base for the current package.
If SYSTEM is supplied, use it with `asdf:system-relative-pathname' on BASE."
  (setf (gethash *package* *package-bases*)
        (assure (and absolute-pathname directory-pathname)
          (if system-supplied?
              (asdf:system-relative-pathname system base)
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
  (if (boundp '*base*) *base*
      (infer-base)))

(defun infer-base-1 (&key (errorp t))
  (or (gethash *package* *package-bases*)
      (system-base (infer-system :errorp errorp))))

(defun infer-base (&key (errorp t))
  (let ((base (infer-base-1 :errorp errorp)))
    (if (absolute-pathname-p base)
        base
        (and errorp
             (error* "Cannot infer base.~%Package: ~a~%File: "
                     *package*
                     (current-lisp-file))))))

(defun system-base (system)
  (setf system (asdf:find-system system))
  (let ((base (asdf:system-relative-pathname system "")))
    (if (absolute-pathname-p base)
        base
        (if (typep system 'asdf:package-inferred-system)
            (let* ((system-name (asdf:primary-system-name system))
                   (base-system-name (take-while (op (not (eql _ #\/))) system-name))
                   (base-system (asdf:find-system base-system-name)))
              (system-base base-system))
            (error* "System ~a has no base." system)))))

(defun infer-system (&key (errorp t))
  (assure (or null asdf:system)
    (or (infer-system-from-package)
        (look-for-asd)
        (and errorp
             (assure asdf:system
               (ensure2 (gethash *package* *supplied-package-systems*)
                 (cerror* "Supply a system name"
                          "Cannot infer a system for ~a" *package*)
                 (read-system-by-name)))))))

(defun read-system-by-name ()
  (format t "~&Type a system name: ")
  (let ((name (make-keyword (string (read)))))
    (or (asdf:find-system name nil)
        (progn
          (cerror* "Supply another name"
                   "No such system as ~a" name)
          (read-system-by-name)))))

(defun current-lisp-file ()
  (or *compile-file-truename* *load-truename*))

(defun infer-system-from-package ()
  (some (lambda (name)
          (when-let (guess (package-name-system name))
            (asdf:find-system guess nil)))
        (package-names *package*)))

;;; XXX
(defun package-name-system (n)
  (asdf/package-inferred-system::package-name-system n))

(defun package-names (p)
  (cons (package-name p)
        (package-nicknames p)))

(defun look-for-asd ()
  "Look for the nearest .asd file and return its name."
  (and-let* ((file (current-lisp-file))
             ((not (typep file 'temporary-file)))
             (.asd (nearest-asdf-file file)))
    (asdf:find-system (pathname-name .asd) nil)))

(defun nearest-asdf-file (file)
  (locate-dominating-file file "*.asd"))

(defun hygienic-pathnames (form &optional (base (base)))
  (leaf-map (lambda (form)
              (if (typep form 'relative-pathname)
                  (merge-pathnames* form base)
                  form))
            form))
