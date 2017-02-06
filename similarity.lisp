(defpackage #:overlord/similarity
  (:use #:cl #:alexandria #:serapeum)
  (:export #:similar?)
  (:documentation "Implementation of the algorithm from 3.2.4.2.2 for
  determining whether objects are similar."))

(in-package #:overlord/similarity)

(define-do-macro do-hash ((k v ht &optional return) &body body)
  `(maphash (lambda (,k ,v)
              ,@body)
            ,ht))

(defun flatten-array (s)
  (make-array (array-total-size s)
              :displaced-to s))

(defun literally-similar? (s c)
  (handler-case
      (equal (write-to-string s :readably t)
             (write-to-string c :readably t))
    (print-not-readable () nil)))

(defun load-forms-similar? (s c &optional env)
  (or
   ;; SBCL's backquote implementation necessitates this.
   (and (typep s 'structure-object)
        (typep c 'structure-object)
        (literally-similar? s c))
   (handler-case
       (mvlet ((load1 init1 (make-load-form s env))
               (load2 init2 (make-load-form c env)))
         (and (similar? load1 load2)
              (similar? init1 init2)))
     (error () nil))))

(defun same-type? (x y)
  (type= (type-of x)
         (type-of y)))

(defgeneric similar? (s c)
  (:documentation "Are S and C similar according to the definition in CLHS 3.2.4.2.2?")
  ;; Default method.
  (:method ((s t) (c t))
    nil)
  (:method ((s number) (c number))
    ;; Two numbers S and C are similar if they are of the same
    ;; type and represent the same mathematical value.
    (and (same-type? s c)
         (= s c)))
  (:method ((s character) (c character))
    ;; Two simple characters S and C are similar if they have similar
    ;; code attributes.
    (= (char-code s) (char-code c)))
  (:method ((s symbol) (c symbol))
    (let ((p1 (symbol-package s))
          (p2 (symbol-package c)))
      (cond
        ;; Two apparently uninterned symbols S and C are similar if
        ;; their names are similar.
        ((nor p1 p2) (string= s c))
        ((and p1 p2) (eql s c))
        (t nil))))
  (:method ((s package) (c package))
    (similar? (package-name s) (package-name c)))
  (:method ((s random-state) (c random-state))
    (if (subtypep 'random-state 'structure-object)
        (equalp s c)
        (or
         ;; On Clozure, for example, random-state is not a subtype of
         ;; structure-object, but it still gets special handling from
         ;; equalp.
         (equalp s c)
         ;; Everything I can easily test. I also test MCKL, but it
         ;; doesn't extend equalp.
         #-(or sbcl ccl clisp ecl gcl)
         (literally-similar? s c))))
  (:method ((s cons) (c cons))
    (and (similar? (car s) (car c))
         (similar? (cdr s) (cdr c))))
  (:method ((s array) (c array))
    (if (and (= (array-rank s) 1)
             (= (array-rank c) 1))
        (and (similar? (length s) (length c))
             (similar? (array-element-type s)
                       (array-element-type c))
             (every #'similar? s c))
        (and (similar? (array-rank s)
                       (array-rank c))
             (similar? (array-dimensions s)
                       (array-dimensions c))
             (similar? (array-element-type s)
                       (array-element-type c))
             (every #'similar?
                    (flatten-array s)
                    (flatten-array c)))))
  (:method ((s hash-table) (c hash-table))
    (and (eql (hash-table-test s)
              (hash-table-test c))
         (flet ((hash-subset? (x y)
                  (do-hash (k1 v1 x t)
                    (multiple-value-bind (v2 v2?)
                        (gethash k1 y)
                      (unless (and v2? (similar? v1 v2))
                        (return nil))))))
           (and (hash-subset? s c)
                (hash-subset? c s)))))
  (:method ((s pathname) (c pathname))
    (equal s c))
  (:method ((s standard-object) (c standard-object))
    (load-forms-similar? s c))
  (:method ((s structure-object) (c structure-object))
    (load-forms-similar? s c)))
