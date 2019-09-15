(defpackage :overlord/project-system
  (:use :cl :alexandria :serapeum)
  (:import-from :overlord/types :error*)
  (:import-from :overlord/base :*base*)
  (:import-from :overlord/target :build)
  (:import-from :quickproject :make-project)
  (:import-from :uiop :ensure-directory-pathname)
  (:documentation "Very basic, experimental ASDF extension. Looks for
a package with the same name as the system and a symbol named `all' in
that package, and builds that.

Use `asdf:make' to get the desired behavior.")
  (:export :overlord-project-system :start-project))
(in-package :overlord/project-system)

(defclass overlord-project-system (asdf:system)
  ((target-name
    :initarg :target-name
    :initform (string 'all)
    :reader project-system-target-name)))

(defun build-default-system-target (system)
  (let* ((system-name
           (string-upcase
            (asdf:component-name system)))
         (target-name
           (string-upcase
            (project-system-target-name system)))
         (package
           (or (find-package system-name)
               (error* "No such package as ~a" system-name)))
         (symbol
           (or (find-symbol (string-upcase target-name)
                            package)
               (error* "No such symbol as ~a in ~a"
                       'all package)))
         (*base* (asdf:system-source-directory system)))
    (build symbol)))

(defmethod asdf:operate :after ((op asdf-utilities:build-op)
                                (system overlord-project-system)
                                &key)
  (build-default-system-target system))

(def template-directory
  (asdf:system-relative-pathname :overlord #p"template/"))

(defun start-project (directory
                      &rest keys
                      &key (target-name "all") depends-on
                      &allow-other-keys)
  "Initialize a new Overlord project.
If DIRECTORY is relative, it is created as a subdirectory of
"
  (apply #'make-project
         (path-join #p"~/common-lisp/" (ensure-directory-pathname directory))
         :template-parameters (list :target (string target-name))
         :template-directory template-directory
         :include-copyright nil
         :depends-on (cons "overlord" depends-on)
         (remove-from-plist keys :target-name)))
