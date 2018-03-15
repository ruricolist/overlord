(defpackage :overlord/asdf
  (:documentation "This package contains wrappers for ASDF functions.

The idea is to be able to trivially audit exactly how Overlord uses ASDF.

If you want to call an ASDF function in another package, don't! Add a wrapper for it here and import that.")
  (:use :cl :alexandria :serapeum)
  (:export
   :find-asdf-system
   :asdf-system-version
   :asdf-system-relative-pathname
   :package-name-asdf-system
   :package-inferred-asdf-system?
   :primary-asdf-system-name
   :asdf-system?
   :asdf-system-loaded?
   :load-asdf-system))
(in-package :overlord/asdf)

(defun find-asdf-system (system &key error)
  (asdf:find-system system (not error)))

(defun asdf-system-version (system &key error)
  (if-let (system (asdf:find-system system (not error)))
    (asdf:component-version system)
    nil))

(defun asdf-system-relative-pathname (system pathname)
  (asdf:system-relative-pathname system pathname))

(defun package-name-asdf-system (n)
  (asdf/package-inferred-system::package-name-system n))

(defun package-inferred-asdf-system? (system)
  (typep system 'asdf:package-inferred-system))

(defun primary-asdf-system-name (system)
  (asdf:primary-system-name system))

(defun asdf-system? (system)
  (typep system 'asdf:system))

(defun asdf-system-loaded? (system)
  (let ((system (asdf:find-system system nil)))
    (and system
         (asdf:component-loaded-p system)
         system)))

(defun load-asdf-system (system)
  (asdf:load-system system))
