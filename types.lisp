(defpackage :overlord/types
  (:use :cl :alexandria :serapeum :uiop/pathname)
  (:import-from :uiop/stream :default-temporary-directory)
  (:import-from :uiop :getcwd)
  (:import-from :trivia :match :let-match1 :ematch
    :multiple-value-ematch)
  (:import-from :fset :compare :compare-slots
    :define-cross-type-compare-methods)
  (:export
   ;; Conditions.
   #:overlord-condition
   #:overlord-error
   #:overlord-warning
   #:overlord-error-target
   #:error*
   #:warning*
   #:cerror*
   ;; General types.
   #:db-version
   #:list-of
   #:check-list-of
   #:plist
   #:universal-time
   #:pathname-designator
   #:package-designator
   #:case-mode
   #:hash-code
   #:delayed-symbol
   #:delay-symbol
   #:maybe-delay-symbol
   #:force-symbol
   ;; Pathname types.
   #:wild-pathname
   #:tame-pathname
   #:absolute-pathname
   #:relative-pathname
   #:directory-pathname
   #:file-pathname
   #:physical-pathname
   #:temporary-file
   #:hash-code
   ;; Symbols
   #:cl-symbol
   ))

(in-package :overlord/types)


;;; Conditions.

(define-condition overlord-condition (condition) ())
(define-condition overlord-error (overlord-condition simple-error) ())
(define-condition overlord-warning (overlord-condition simple-warning) ())

(defgeneric overlord-error-target (error))

(defun print-current-dir (&optional (stream t))
  "Print the current directory to STREAM.
If the value of `*default-pathname-defaults*' and a call to
`uiop:getcwd' differ, then print them both."
  (let ((dpd *default-pathname-defaults*)
        (cwd (getcwd)))
    (format stream "~2&Working dir: ~s" cwd)
    (unless (pathname-equal dpd cwd)
      (format stream "~%~s: ~s"
              '*default-pathname-defaults*
              *default-pathname-defaults*))))

(defmethod print-object :after ((x overlord-condition) stream)
  (unless *print-escape*
    (print-current-dir stream)))

(defun error* (message &rest args)
  (error 'overlord-error
         :format-control message
         :format-arguments args))

(defun cerror* (cont message &rest args)
  (cerror cont
          'overlord-error
          :format-control message
          :format-arguments args))

(defun warn* (message &rest args)
  (warn 'overlord-warning
        :format-control message
        :format-arguments args))


;;; General types.

(deftype db-version ()
  '(integer 1 *))

(deftype universal-time ()
  '(integer 0 *))

(deftype pathname-designator ()
  '(or string pathname))

(deftype package-designator ()
  '(or string-designator package))

(deftype list-without-nil ()
  `(and list (satisfies list-without-nil?)))

(defsubst list-without-nil? (list)
  (declare (inline memq))
  (not (memq nil list)))

(deftype list-of (a)
  ;; We don't check that every element is of type A (that could be
  ;; expensive) but, if `null' is not a subtype of A, then we do check
  ;; that `nil' is not present in the list. It is not sound, but it is
  ;; useful.
  (if (subtypep 'null a)
      ;; XXX Not, of course, recursive, but still catches many
      ;; mistakes.
      `(or null (cons ,a list))
      `(and list (satisfies list-without-nil?))))

(defun check-list-of* (list item-type)
  (unless (and (listp list)
               (every (of-type item-type) list))
    (error 'type-error
           :datum list
           :expected-type `(list-of ,item-type))))

(defmacro check-list-of (list item-type)
  `(check-list-of* ,list ',item-type))

(deftype plist ()
  '(and list (satisfies plist?)))

(defloop plist? (list)
  (declare (optimize speed (debug 0)))
  (match list
    (() t)
    ((list* (and _ (type symbol)) _ list)
     (plist? list))
    (otherwise nil)))

(deftype case-mode ()
  "Possible values for a readtable's case mode."
  '(member :upcase :downcase :preserve :invert))

(deftype hash-code ()
  '(integer 0 #.most-positive-fixnum))

(defconstructor delayed-symbol
  (package-name string)
  (symbol-name string))

(defun delay-symbol (symbol)
  (assure delayed-symbol
    (match symbol
      ((delayed-symbol) symbol)
      ((and _ (type symbol))
       (let ((package-name
               (~> symbol
                   symbol-package
                   package-name))
             (symbol-name (symbol-name symbol)))
         (delayed-symbol package-name symbol-name)))
      (otherwise symbol))))

(defun force-symbol (delay)
  (match delay
    ((delayed-symbol package-name symbol-name) delay
     (if-let (package (find-package package-name))
       (receive (symbol status) (find-symbol symbol-name package)
         (if (null status)
             (error* "Cannot force symbol: no such symbol as ~a::~a"
                     package-name
                     symbol-name)
             symbol))
       (error* "Cannot force symbol: no such package as ~a" package-name)))
    (otherwise delay)))

(defun delayed-symbol= (ds1 ds2)
  (multiple-value-ematch (values ds1 ds2)
    (((delayed-symbol p1 s1)
      (delayed-symbol p2 s2))
     (and (equal p1 p2)
          (equal s1 s2)))))

(defmethod compare ((ds1 delayed-symbol) (ds2 delayed-symbol))
  (fset:compare-slots ds1 ds2
                      #'delayed-symbol-package-name
                      #'delayed-symbol-symbol-name))

(define-cross-type-compare-methods delayed-symbol)

(defun maybe-delay-symbol (symbol)
  (cond ((not (symbolp symbol))
         symbol)
        ((built-in-symbol? symbol)
         symbol)
        (t (delay-symbol symbol))))

(defun built-in-symbol? (symbol)
  (and (symbolp symbol)
       (or (keywordp symbol)
           (cl-symbol-p symbol)
           (when-let ((package (symbol-package symbol)))
             (eq package (find-package :overlord/target))))))


;;; Pathname types.

(deftype wild-pathname ()
  "A pathname with wild components."
  '(and pathname (satisfies wild-pathname-p)))

(deftype tame-pathname ()
  "A pathname without wild components."
  '(or directory-pathname
    (and pathname (not (satisfies wild-pathname-p)))))

(deftype absolute-pathname ()
  '(and pathname (satisfies absolute-pathname-p)))

(deftype relative-pathname ()
  '(and pathname (satisfies relative-pathname-p)))

(deftype directory-pathname ()
  '(and pathname (satisfies directory-pathname-p)))

(deftype file-pathname ()
  '(and pathname (satisfies file-pathname-p)))

;;; logical-pathname is defined in CL.

(deftype physical-pathname ()
  '(and pathname (not (satisfies logical-pathname-p))))

(defun temporary-file? (file)
  (subpathp file (default-temporary-directory)))

(deftype temporary-file ()
  '(and pathname (satisfies temporary-file?)))


;;; Symbol types.

(defun cl-symbol-p (x)
  (and (symbolp x)
       (eql (symbol-package x)
            (find-package :cl))))

(deftype cl-symbol ()
  '(and symbol
    (not keyword)
    (satisfies cl-symbol-p)))
