(cl:defpackage #:overlord/util
  (:use :cl :alexandria :serapeum :trivial-file-size)
  (:import-from :overlord/types
    :case-mode :file-pathname :tame-pathname)
  (:import-from :fset :with :less)
  (:import-from :uiop
    :pathname-directory-pathname
    :pathname-parent-directory-pathname
    :file-exists-p
    :run-program
    :native-namestring
    :ensure-pathname
    :with-temporary-file
    :rename-file-overwriting-target
    :delete-file-if-exists)
  (:import-from :babel :string-to-octets)
  (:export
   #:compare
   #:package-exports
   #:locate-dominating-file
   #:quoted-symbol?
   #:class-name-of
   #:package-name-keyword
   #:find-external-symbol
   #:coerce-case
   #:eval*
   #:dx-sxhash
   #:ensure-pathnamef
   #:read-file-form
   #:write-form-as-file
   #:write-file-if-changed
   #:copy-file-if-changed
   #:call/temp-file-pathname
   #:withf
   #:lessf
   #:with-absolute-package-names
   #:resolve-package
   #:file-mtime
   #:propagate-side-effect))
(cl:in-package #:overlord/util)

(define-modify-macro withf (&rest item-or-tuple) with)
(define-modify-macro lessf (&rest item-or-tuple) less)

;;; TODO This cries out for a compiler macro.
(-> compare (function function &rest t) t)
(defun compare (test accessor &rest xs)
  (cond ((null xs) t)
        ((single xs) t)
        (t (let* ((accessor (ensure-function accessor))
                  (test (ensure-function test))
                  (xs (mapcar accessor xs)))
             (every test xs (rest xs))))))

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

(defsubst ensure-pathname* (x)
  (ensure-pathname x :want-pathname t))

(define-modify-macro ensure-pathnamef () ensure-pathname*)

(defun read-file-form (file)
  (when (file-exists-p file)
    (with-standard-io-syntax
      (with-open-file (in file :direction :input
                               :if-does-not-exist nil)
        (when in
          (prog1 (read in)
            (ignoring end-of-file
              (read in)
              (error "More than one form in ~a" file))))))))

(defun write-form-as-file (form file)
  (with-standard-io-syntax
    (with-open-file (out file
                         :direction :output
                         ;; It is possible, when using :supersede, for
                         ;; the old timestamp to be preserved.
                         :if-exists :rename-and-delete)
      (write form :stream out
                  :readably t))))

(defun existing-file-unchanged? (data file &key (buffer-size 4096))
  (labels ((make-buffer (size)
             (make-array (assure array-length size)
                         :element-type 'octet)))
    (let ((buffer (make-buffer buffer-size)))
      (with-input-from-file (stream file :element-type 'octet)
        (let ((len (file-length stream)))
          (and (= (length data) len)
               (loop for offset from 0 by buffer-size below len
                     for end1 = (read-sequence buffer stream)
                     always (vector= buffer data
                                     :start2 offset
                                     :end1 end1))))))))

(defun call/temp-file-pathname (dest fn)
  "Call FN on a freshly allocated temporary pathname; if it completes
safely, overwrite DEST with the contents of the temporary file."
  (let* ((ok nil)
         (tmp (with-temporary-file (:pathname p :keep t)
                (funcall fn p)
                (setq ok t)
                p)))
    (if ok
        ;; Cross-device?
        (flet ((rename-by-copying ()
                 (copy-file tmp dest :if-to-exists :rename-and-delete)))
          (if (equal (pathname-device tmp)
                     (pathname-device dest))
              (handler-case
                  (rename-file-overwriting-target tmp dest)
                (error ()
                  (rename-by-copying)))
              (rename-by-copying)))
        (delete-file-if-exists tmp))))

(defun replace-file-atomically (data dest)
  "Write DATA into DEST"
  (check-type data octet-vector)
  (check-type dest (and file-pathname tame-pathname))
  (let (temp)
    (with-temporary-file (:stream out
                          :direction :output
                          :element-type 'octet
                          :pathname p
                          :keep t
                          ;; Use the same directory so the overwrite is atomic.
                          :directory (pathname-directory-pathname dest))
      (write-sequence data out)
      (setf temp p))
    (rename-file-overwriting-target temp dest)))

(defun write-file-if-changed (data file &key (encoding :utf-8)
                                             (buffer-size 4096))
  "Write DATA into FILE only if FILE would change.
DATA may be a string or a byte vector.

Cf. Shake."
  (check-type file pathname)
  (etypecase (assure vector data)
    (string
     (write-file-if-changed
      (string-to-octets data :encoding encoding)
      file))
    ((and vector (not octet-vector))
     (write-file-if-changed
      (coerce data 'octet-vector) file))
    (octet-vector
     (cond ((not (file-exists-p file))
            (replace-file-atomically data file))
           ((existing-file-unchanged? data file :buffer-size buffer-size)
            (values))
           (t
            (replace-file-atomically data file))))))

(defun copy-file-if-changed (from to)
  (if (not (file-exists-p to))
      (copy-file from to)
      (unless (file= from to)
        (copy-file from to :if-to-exists :rename-and-delete))))

;;; Make sure that we treat package names consistently, whether or not
;;; the Lisp implementation uses package-relative nicknames.

(defmacro with-absolute-package-names ((&key) &body body)
  `(let ((*package* (find-package :keyword)))
     ,@body))

;; Maybe this should shadow `find-package'; I'm not sure.
(defun resolve-package (package-designator)
  "Like `find-package', but make sure the package is resolved in
absolute terms even if the Lisp implementation supports local package
nicknames."
  (with-absolute-package-names ()
    (find-package package-designator)))

(defun file-mtime (pathname)
  "Same as `file-write-date'.
This is provided in case we ever want to offer more precise timestamps
on Lisp/OS/filesystem combinations that support it."
  (cl:file-write-date pathname))

(defmacro propagate-side-effect (&body body &environment env)
  "Force BODY to be evaluated both at compile time AND load time (but
not run time).

Note that BODY should be idempotent, as it may be evaluated more than
once."
  ;; Evaluate it right now, unless we're at the top level.
  (unless (null env)
    (eval `(progn ,@body)))
  `(progn
     ;; Ensure the effect happens both at compile time, and at load
     ;; time, when not at the top level.
     (eval-when (:compile-toplevel :load-toplevel)
       ,@body)
     (eval-when (:execute)
       (load-time-value
        (progn ,@body t)))
     t))
