(defpackage :overlord/types
  (:use :cl :alexandria :serapeum :uiop/pathname)
  (:import-from :uiop/stream :default-temporary-directory)
  (:import-from :trivia :match :_)
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
   #:check-list-of
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
   #:import-alias
   #:bindable-symbol
   #:export-alias
   #:var-spec
   #:function-spec
   #:macro-spec
   #:binding-spec
   #:export-spec
   #:var-alias
   #:function-alias
   #:macro-alias
   #:definable-symbol
   #:binding-designator
   #:canonical-binding
   #:non-keyword))

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

(defun plist? (list)
  (nlet plist? (list)
    (match list
      (() t)
      ((list* (and _ (type symbol)) _ list)
       (plist? list))
      (otherwise nil))))

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

(deftype var-spec ()
  'non-keyword)

(deftype function-spec ()
  '(tuple 'function bindable-symbol))

(deftype macro-spec ()
  '(tuple 'macro-function bindable-symbol))

;;; Exports.

(deftype export-alias ()
  '(and symbol (not (member t nil function quote))))

(deftype export-spec ()
  '(or var-spec
    function-spec
    macro-spec
    (tuple var-spec :as export-alias)
    (tuple function-spec :as export-alias)
    (tuple macro-spec :as export-alias)))

;;; Imports.

(deftype var-alias () 'bindable-symbol)
(deftype function-alias () '(tuple 'function bindable-symbol))
(deftype macro-alias () '(tuple 'macro-function bindable-symbol))
(deftype import-alias () '(or var-alias function-alias macro-alias))

(deftype binding-spec ()
  '(or (member :all :all-as-functions)
    (tuple :import-set list)
    list))

(deftype canonical-binding ()
  '(tuple keyword import-alias))

(deftype binding-designator ()
  '(or
    var-spec
    function-spec
    macro-spec
    (tuple symbol :as import-alias)))
