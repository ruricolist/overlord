(cl:defpackage #:overlord/util
  (:use :cl :alexandria :serapeum :trivial-file-size)
  (:import-from :overlord/types
    :case-mode :file-pathname :tame-pathname
    :error*)
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
  (:import-from :quri :url-encode)
  (:import-from :bit-smasher :octets->hex)
  (:import-from #:local-time
    #:nsec-of
    #:timestamp-to-universal)
  (:export
   #:compare
   #:locate-dominating-file
   #:quoted-symbol?
   #:find-external-symbol
   #:coerce-case
   #:eval*
   #:dx-sxhash
   #:ensure-pathnamef
   #:read-file-form
   #:write-form-as-file
   #:write-file-if-changed
   #:copy-file-if-changed
   #:call/temp-file-pathnames
   #:call/temp-file-pathname
   #:withf
   #:lessf
   #:with-absolute-package-names
   #:resolve-package
   #:file-mtime
   #:propagate-side-effect
   #:url-encode
   #:byte-array-to-hex-string
   #:version-major-version
   #:timestamp-diff))
(cl:in-package #:overlord/util)

(define-modify-macro withf (&rest item-or-tuple) with
  "Modify macro for augmenting an Fset map or set.")

(define-modify-macro lessf (&rest item-or-tuple) less
  "Modify macro for removing from an Fset map or set.")

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
       (declare (optimize (speed 3) (safety 1) (debug 0)
                          (compilation-speed 0)))
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

(defun rename-by-copying (tmp dest)
  (copy-file tmp dest :if-to-exists :rename-and-delete))

(defun call/temp-file-pathname (dest fn)
  (call/temp-file-pathnames
   (list dest)
   (lambda (outs)
     (funcall fn (only-elt outs)))))

(defun call/temp-file-pathnames (dests fn)
  (let* ((dests (coerce dests 'list))
         (start-times
           (mapcar #'file-write-date dests))
         (start-sizes
           (mapcar #'file-size-in-octets dests))
         (ok nil)
         (tmps
           (loop for nil in dests
                 collect (with-temporary-file (:pathname p :keep t)
                           p))))
    (unwind-protect
         (progn
           (funcall fn tmps)
           ;; Check that the destinations have not been written to.
           (loop for dest in dests
                 for start-time in start-times
                 for start-size in start-sizes
                 for end-time = (file-write-date dest)
                 for end-size = (file-size-in-octets dest)
                 unless (and (eql start-time end-time)
                             (eql start-size end-size))
                   do (error* "Destination file ~a has been written to directly."
                              dest))
           (loop for tmp in tmps
                 for dest in dests
                 do (ensure-directories-exist dest)
                 if (equal (pathname-device tmp)
                           (pathname-device dest))
                   do (handler-case
                          (rename-file-overwriting-target tmp dest)
                        (error ()
                          (rename-by-copying tmp dest)))
                 else do (rename-by-copying tmp dest))
           (setq ok t))
      (unless ok
        (mapc #'delete-file-if-exists tmps)))))

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

Return T if the file was written to, NIL otherwise."
  (check-type file pathname)
  (etypecase (assure vector data)
    (string
     (write-file-if-changed
      (string-to-octets data :encoding encoding)
      file))
    ((and vector (not octet-vector))
     (write-file-if-changed
      (coerce data 'octet-vector)
      file))
    (octet-vector
     (cond ((not (file-exists-p file))
            (replace-file-atomically data file)
            t)
           ((existing-file-unchanged? data file :buffer-size buffer-size)
            nil)
           (t
            (replace-file-atomically data file)
            t)))))

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
  ;; Evaluate it right now, unless we're at the top level (to avoid
  ;; warnings about repeated definitions).
  (unless (null env)
    (eval `(progn ,@body)))
  `(progn
     ;; Ensure the effect happens both at the top level.
     (eval-when (:compile-toplevel :load-toplevel)
       ,@body)
     ;; Ensure the effect happens at load time when not at the top
     ;; level.
     (eval-when (:execute)
       (load-time-value
        (progn ,@body t)))
     t))

(defun byte-array-to-hex-string (ba)
  (octets->hex ba))

(defun version-major-version (version)
  (etypecase version
    (null nil)
    ((integer 0 *) version)
    (string
     (let ((version
             (if (string^= "v" version)
                 (subseq version 1)
                 version)))
       (assure (integer 0 *)
         (parse-integer version
                        :junk-allowed t
                        :radix 10))))))

(defun timestamp-diff (end start)
  "Return the difference between END and START, two timestamps, in
nanoseconds."
  (let* ((s1 (timestamp-to-universal start))
         (s2 (timestamp-to-universal end))
         (ns1 (nsec-of start))
         (ns2 (nsec-of end))
         (ns1 (+ ns1 (* s1 (expt 10 9))))
         (ns2 (+ ns2 (* s2 (expt 10 9)))))
    (- ns2 ns1)))
