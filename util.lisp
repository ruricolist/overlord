(cl:defpackage #:overlord/util
  (:use :cl :alexandria :serapeum)
  (:import-from :overlord/specials :*base*)
  (:import-from :overlord/types :case-mode)
  (:import-from :uiop
    :pathname-directory-pathname
    :pathname-parent-directory-pathname
    :file-exists-p
    :run-program
    :native-namestring)
  (:export
   #:package-exports
   #:locate-dominating-file
   #:quoted-symbol?
   #:class-name-of
   #:once
   #:mv-once
   #:package-name-keyword
   #:find-external-symbol
   #:coerce-case
   #:eval*
   #:dx-sxhash))
(cl:in-package #:overlord/util)

(defun package-exports (p)
  (collecting
    (do-external-symbols (s p)
      (collect s))))

(defun locate-dominating-file (file name)
  (nlet rec ((dir (pathname-directory-pathname file))
             (name (pathname name)))
    (if (equal dir (user-homedir-pathname))
        nil
        (let ((file (make-pathname :defaults dir
                                   :name (pathname-name name)
                                   :type (pathname-type name))))
          (flet ((rec ()
                   (let ((parent (pathname-parent-directory-pathname dir)))
                     (if (equal parent dir)
                         nil
                         (rec parent name)))))
            (if (wild-pathname-p file)
                (let ((matches (directory file)))
                  (if matches
                      (values (first matches) (rest matches))
                      (rec)))
                (or (file-exists-p file)
                    (rec))))))))

(defun quoted-symbol? (x)
  (and (consp x)
       (= (length x) 2)
       (eql (first x) 'quote)
       (symbolp (second x))))

(defsubst class-name-of (x)
  (class-name (class-of x)))

(defmacro once (expr)
  "Evaluate EXPR exactly once and store the result for future calls.

This works by allocating a cell using `load-time-value'. At run time,
we check if the cell has been set. If the cell has been set, then we
return the value stored in the cell. If the cell has not been set,
then we set its value inside a critical section."
  (with-unique-names (cell value)
    (let ((empty "empty"))
      `(let* ((,cell (load-time-value (box ,empty)))
              (,value (unbox ,cell)))
         (if (eq ,value ,empty)         ;Identity.
             (synchronized ()
               ;; "Double-checked locking."
               (let ((,value (unbox ,cell)))
                 (if (eq ,value ,empty)
                     (setf (unbox ,cell) ,expr)
                     ,value)))
             ;; The value has been set, just return it.
             ,value)))))

(defmacro mv-once (expr)
  `(values-list (once (multiple-value-list ,expr))))

(defun package-name-keyword (x)
  (assure keyword
    (etypecase-of (or package string-designator) x
      (package (make-keyword (package-name x)))
      (keyword x)
      ((or string character symbol) (make-keyword x)))))

(defun find-external-symbol (name package)
  (multiple-value-bind (sym status)
      (find-symbol (string name) package)
    (and sym (eql status :external) sym)))

(defun coerce-case (string &key (readtable *readtable*))
  (if (stringp string)
      (ecase-of case-mode (readtable-case readtable)
        (:upcase (string-upcase string))
        (:downcase (string-downcase string))
        (:preserve string)
        (:invert (string-invert-case string)))
      (string string)))

(defun eval* (expr)
  "Evaluate EXPR by compiling it to a thunk, then calling that thunk."
  (funcall (compile nil (eval `(lambda () ,expr)))))

(defmacro dx-sxhash (expr)
  "Like SXHASH, but try to stack-allocate EXPR."
  (with-unique-names (temp)
    `(let ((,temp ,expr))
       (declare (dynamic-extent ,temp))
       (sxhash ,temp))))
