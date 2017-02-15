(defpackage :overlord/types
  (:use :cl :alexandria :serapeum :uiop/pathname)
  (:import-from :uiop/stream :default-temporary-directory)
  (:export
   ;; Conditions.
   #:overlord-condition
   #:overlord-error
   #:overlord-warning
   #:error*
   #:warning*
   #:cerror*
   ;; General types.
   #:list-of
   #:plist
   #:universal-time
   #:pathname-designator
   #:package-designator
   #:case-mode
   #:hash-code
   ;; Pathname types.
   #:wild-pathname
   #:tame-pathname
   #:absolute-pathname
   #:relative-pathname
   #:directory-pathname
   #:file-pathname
   #:physical-pathname
   #:temporary-file
   ;; Imports and exports.
   #:bindable-symbol
   #:export-alias
   #:import-alias
   #:non-keyword
   #:function-spec
   #:macro-spec
   #:binding
   #:export-spec
   #:definable-symbol
   #:var-alias
   #:function-alias
   #:macro-alias
   #:binding-spec
   #:binding-designator))

(in-package :overlord/types)


;;; Conditions.

(define-condition overlord-condition (condition) ())
(define-condition overlord-error (overlord-condition simple-error) ())
(define-condition overlord-warning (overlord-condition simple-warning) ())

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

(deftype universal-time ()
  '(integer 0 *))

(deftype pathname-designator ()
  '(or string pathname))

(deftype package-designator ()
  '(or string-designator package))

(deftype list-of (a)
  ;; XXX Not, of course, recursive, but still catches most mistakes.
  `(or null (cons ,a list)))

(deftype plist ()
  ;; Would it be worth it to check for even length?
  '(or null (cons symbol (cons t list))))

(deftype case-mode ()
  "Possible values for a readtable's case mode."
  '(member :upcase :downcase :preserve :invert))

(deftype hash-code ()
  '(integer 0 #.most-positive-fixnum))


;;; Pathname types.

(deftype wild-pathname ()
  "A pathname with wild components."
  '(and pathname (satisfies wild-pathname-p)))

(deftype tame-pathname ()
  "A pathname without wild components."
  '(and pathname (not (satisfies wild-pathname-p))))

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


;;; Imports and exports.

(defconst cl-constants
  (collecting
    (do-external-symbols (s :cl)
      (when (constantp s)
        (collect s)))))

(defun cl-symbol? (x)
  (and (symbolp x)
       (eql (symbol-package x)
            (find-package :cl))))

(deftype cl-symbol ()
  '(and symbol
    (not keyword)
    (satisfies cl-symbol?)))

(deftype bindable-symbol ()
  "To a rough approximation, a symbol that can/should be bound."
  '(and symbol
    (not (member nil t function quote))
    (not keyword)))

(deftype definable-symbol ()
  "To a rough approximation, a symbol that can/should be given a definition."
  '(and symbol
    (not (satisfies constantp))
    (not keyword)                       ;works for functions, though.
    (not cl-symbol)))

(deftype non-keyword ()
  `(and symbol
        (not keyword)
        ;; XXX Too slow.
        #+ () (not (satisfies constantp))
        (not (member ,@cl-constants))
        ;; This would just be confusing.
        (not (member quote function))))

(deftype function-spec ()
  '(tuple 'function symbol))

(deftype macro-spec ()
  '(tuple 'macro-function symbol))

(deftype binding ()
  '(or non-keyword function-spec macro-spec))

;;; Exports.

(deftype export-alias ()
  '(and symbol (not (member t nil function quote))))

(deftype export-spec ()
  '(or non-keyword
    function-spec
    macro-spec
    (tuple non-keyword export-alias)
    (tuple function-spec export-alias)
    (tuple macro-spec export-alias)))

;;; Imports.

(deftype var-alias () 'bindable-symbol)
(deftype function-alias () '(tuple 'function bindable-symbol))
(deftype macro-alias () '(tuple 'macro-function bindable-symbol))
(deftype import-alias () '(or var-alias function-alias macro-alias))

(deftype binding-spec ()
  '(or (member :all :all-as-functions) list))

(deftype binding-designator ()
  '(or atom
    function-alias
    macro-alias
    (tuple symbol import-alias)
    (tuple (tuple 'function symbol) import-alias)
    (tuple (tuple 'macro-function symbol) import-alias)))
