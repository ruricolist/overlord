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
   :load-asdf-system
   :asdf-system-base
   :require-asdf-system
   :asdf-system-name-keyword
   :asdf-system-name))
(in-package :overlord/asdf)

;;; Did you know that, in SBCL, calls to `asdf:find-system' from
;;; multiple threads can result in a deadlock, due to the fact that
;;; `uiop:coerce-class' calls `subtypep', which can lead to taking the
;;; world lock? Anyway, we shouldn't assume ASDF is thread-safe.
(defun find-asdf-system (system &key error)
  (asdf:find-system system (not error)))

(defun asdf-system-version (system &key error)
  (if-let (system (asdf:find-system system (not error)))
    (asdf:component-version system)
    nil))

(defun asdf-system-relative-pathname (system pathname)
  (asdf:system-relative-pathname system pathname))

(defun package-name-asdf-system (n)
  ;; XXX Internal symbol.
  (asdf/package-inferred-system::package-name-system n))

(defun package-inferred-asdf-system? (system)
  (typep system 'asdf:package-inferred-system))

(defun primary-asdf-system-name (system)
  (asdf:primary-system-name system))

(defun asdf-system? (system)
  (typep system 'asdf:system))

(-> asdf-system-name-keyword
    ((or asdf:system string keyword))
    (values keyword &optional))
(defun asdf-system-name-keyword (system)
  (etypecase system
    (asdf:system
     (~> system
         asdf:component-name
         asdf-system-name-keyword))
    (string
     (~> system
         string-upcase
         make-keyword))
    (keyword system)))

(defun asdf-system-name (system)
  (asdf:component-name (asdf:find-system system)))

(defun asdf-system-loaded? (system)
  (let ((system (asdf:find-system system nil)))
    (and system
         (asdf:component-loaded-p system)
         system)))

(defun load-asdf-system (system)
  (asdf:load-system system))

(defun asdf-system-base (system)
  (asdf:system-relative-pathname system ""))

(defun require-asdf-system (system)
  ;; For some reason (why?) asdf:require-system is deprecated.
  (unless (asdf:component-loaded-p system)
    (asdf:load-system system)))
