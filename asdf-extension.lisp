(defpackage :overlord/asdf-extension
  (:use :cl :alexandria :serapeum)
  (:import-from :overlord/types :error*)
  (:documentation "Very basic, experimental ASDF extension. Looks for
a package with the same name as the system and a symbol named `all' in
that package, and builds that.

Use `asdf:make' to get the desired behavior.")
  (:export :overlord-system))
(in-package :overlord/asdf-extension)

(defclass overlord-system (asdf:system)
  ((target-name
    :initarg :target-name
    :initform (string 'all)
    :reader overlord-system-target-name)))

(defun build-default-system-target (system)
  (let* ((system-name (string-upcase (asdf:component-name system)))
         (target-name (string (overlord-system-target-name system)))
         (package (or (find-package system-name)
                      (error* "No such package as ~a" system-name)))
         (symbol (or (find-symbol target-name package)
                     (error* "No such symbol as ~a in ~a"
                             'all package)))
         (overlord:*base* (asdf:system-source-directory system)))
    (overlord:build symbol)))

(defmethod asdf:operate :after ((op asdf-utilities:build-op)
                                (system overlord-system)
                                &key)
  (build-default-system-target system))
