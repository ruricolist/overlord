(uiop/package:define-package :overlord-tests
    (:use :fiveam)
  (:mix :overlord/shadows :serapeum :alexandria)
  (:import-from :overlord :with-imports :require-as)
  (:import-from :overlord/impl :target-timestamp)
  (:import-from :local-time :now)
  (:import-from :uiop :absolute-pathname-p)
  ;; Languages.
  (:import-from :overlord/demo/js)
  (:import-from :overlord/lang/sweet-exp)
  (:import-from :overlord/lang/s-exp)
  (:import-from :core-lisp)
  (:export :run))
(in-package :overlord-tests)

(overlord:set-package-base (asdf:system-relative-pathname :overlord ""))


;;; Suite.

(def-suite overlord)
(in-suite overlord)


;;; Running tests.
(defun run-overlord-tests ()
  (let ((overlord:*base* (asdf:system-relative-pathname :overlord ""))
        (fiveam:*on-error* :debug))
    (run! 'overlord)))

;;; Internal use.
(defun debug-test (test)
  (let ((overlord:*base* (asdf:system-relative-pathname :overlord ""))
        (fiveam:*on-error* :debug))
    (run! test)))


;;; Utilities.

(defun resolve-file (file)
  (uiop:native-namestring
   (if (absolute-pathname-p file)
       file
       (asdf:system-relative-pathname :overlord file))))

(defun touch-file (file)
  (lret ((file-string (resolve-file file)))
    (uiop:run-program `("touch" ,file-string))))

(defun touch (&rest targets)
  (flet ((touch (target)
           (if (typep target '(or pathname string))
               (touch-file target)
               (setf (target-timestamp target) (now)))))
    (mapcar #'touch targets)))

;;; Does the utility work?
(test touch-test
  (let ((file (resolve-file "tests/touch-test")))
    (is (< (file-write-date file)
           (progn
             (touch file)
             (file-write-date file))))))

(defmacro disable (&body body)
  `(comment ,@body))


;;; Definition form tests.

(overlord:define-constant
    +literal-string-file+ #p"tests/literal.txt")

(overlord:defconst/deps +literal-string+
    (read-file-into-string +literal-string-file+)
  (:depends-on '+literal-string-file+)
  (:depends-on +literal-string-file+))

(overlord:defvar/deps *literal-string*
    (read-file-into-string +literal-string-file+)
  (:depends-on '+literal-string-file+)
  (:depends-on +literal-string-file+))

(test const/deps
  (sleep 1)
  (local
    (def original #.+literal-string+)
    (touch +literal-string-file+)
    (overlord:build '+literal-string+)
    (is (not (eq original +literal-string+)))))

(test var/deps
  (local
    (def string1 *literal-string*)
    (is (stringp string1))

    (touch +literal-string-file+)
    (overlord:build '*literal-string*)
    (def string2 *literal-string*)
    (is (stringp string2))
    (is (not (eq string1 string2)))

    (touch '+literal-string-file+)
    (overlord:build '*literal-string*)
    (def string3 *literal-string*)
    (is (stringp string3))
    (is (not (eq string2 string3)))))


;;; Basic language tests.

;;; JS demo.

(test js-demo
  (touch "demo/demo1.js")
  (is
   (equal "moooooooooooo"
          (overlord:with-imports (demo1 :from "demo/demo1.js" :binding (#'moo))
            (moo 5)))))

;;; Meta-languages.

(test s-exp
  (touch "tests/s-exp-test.sexp")
  (is (= 42
         (overlord:with-imports (answer :from "tests/s-exp-test.sexp")
           answer))))

(test sweet-exp
  (touch "tests/factorial.lsp")
  (is
   (= 2432902008176640000
      (overlord:with-imports (factorializer :from "tests/factorial.lsp" :binding (#'fact))
        (fact 20)))))
