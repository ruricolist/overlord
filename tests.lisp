(uiop/package:define-package :overlord-tests
    (:use :fiveam :overlord/import-set)
  (:mix :overlord/shadows :serapeum :alexandria)
  (:import-from :overlord :with-imports :require-as
    :with-import-default :require-default)
  (:import-from :overlord/target :target-timestamp)
  (:import-from :overlord/types :overlord-error)
  (:import-from :overlord/asdf
    :asdf-system-relative-pathname)
  (:import-from :local-time :now)
  (:import-from :uiop
    :native-namestring
    :file-exists-p
    :absolute-pathname-p
    :os-windows-p
    :run-program)
  ;; Languages.
  (:import-from :overlord/demo/js)
  (:import-from :overlord/lang/sweet-exp)
  (:import-from :overlord/lang/s-exp)
  (:import-from :core-lisp)
  (:export :run))
(in-package :overlord-tests)

(overlord:set-package-base (asdf-system-relative-pathname :overlord ""))

(defun nap (&optional (n 1))
  "Sleep until the universal time counter ticks over."
  (format t "~&zzz...")
  (loop with start = (get-universal-time)
        until (< start (get-universal-time))
        do (sleep n))
  (format t "~%"))


;;; Suite.

(def-suite overlord)
(in-suite overlord)


(defun call/temp-db (fn)
  (let* (;; Use a random fasl version so we can be reasonably sure
         ;; everything is being compiled clean.
         (version (random most-positive-fixnum))
         (overlord/specials:*db-version* version)
         (overlord/db::*db* (overlord/db::reload-db)))
    (unwind-protect
         (funcall fn)
      (when (equal (overlord/specials:db-version) version)
        (overlord/db:delete-versioned-db)))))

(defmacro with-temp-db ((&key) &body body)
  (with-thunk (body)
    `(call/temp-db ,body)))

;;; Running tests.
(defun run-overlord-tests ()
  (let ((overlord:*base* (asdf-system-relative-pathname :overlord ""))
        (fiveam:*on-error* :debug)))
  (format t "~&First run (1/2)~%")
  (with-temp-db ()
    (run! 'overlord))
  (format t "~&Resetting global state for second run...~%")
  (nap 2)
  (format t "~&Second run (2/2)~%")
  (overlord/global-state:reset-global-state)
  (with-temp-db ()
    (run! 'overlord)))

;;; Internal use.
(defun debug-test (test)
  (let ((overlord:*base* (asdf-system-relative-pathname :overlord ""))
        (fiveam:*on-error* :debug)
        (fiveam:*on-failure* :debug))
    (run! test)))


;;; Utilities.

;;; Force reload whenever run.
(defmacro with-imports* ((mod &rest args &key from as &allow-other-keys) &body body)
  `(progn
     (require-as ,as ,from)
     (with-imports (,mod ,@args)
       ,@body)))

(defun resolve-file (file)
  (native-namestring
   (if (absolute-pathname-p file)
       file
       (asdf-system-relative-pathname :overlord file))))

(defun touch-file (file)
  (lret ((file-string (resolve-file file)))
    (assert (file-exists-p file-string))
    (if (os-windows-p)
        (run-program
         (fmt "powershell (ls \"~a\").LastWriteTime = Get-Date"
              (native-namestring file-string)))
        (run-program `("touch" ,file-string)))))

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

(overlord:defconfig +literal-string-file+ #p"tests/literal.txt")

(overlord:define-target-config +literal-string+
    (read-file-into-string +literal-string-file+)
  (:depends-on '+literal-string-file+)
  (:depends-on +literal-string-file+))

(overlord:define-target-var *literal-string*
    (read-file-into-string +literal-string-file+)
  (:depends-on '+literal-string-file+)
  (:depends-on +literal-string-file+))

(test config/deps
  (nap 1)
  (local
    (def original +literal-string+)
    (touch +literal-string-file+)
    (overlord:build '+literal-string+)
    (is (not (eq original +literal-string+)))))

(test var/deps
  (local
    (def string1 *literal-string*)
    (is (stringp string1))

    (nap 1)

    (touch +literal-string-file+)
    (overlord:build '*literal-string*)
    (def string2 *literal-string*)
    (is (stringp string2))
    (is (not (eq string1 string2)))

    (nap 1)

    (touch '+literal-string-file+)
    (overlord:build '*literal-string*)
    (def string3 *literal-string*)
    (is (stringp string3))
    (is (not (eq string2 string3)))))


;;; Basic language tests.

;;; JS demo.

(test js-demo
  (is
   (equal "moooooooooooo"
          (with-imports* (demo1 :from "demo/demo1.js" :binding (#'moo))
            (moo 5)))))

;;; Meta-languages.

(test s-exp
  (is (= 42
         (with-import-default (answer :from "tests/s-exp-test.sexp")
           answer))))

(test sweet-exp
  (is
   (= 2432902008176640000
      (with-imports* (factorializer :from "tests/factorial.lsp" :binding (#'fact))
        (fact 20)))))

(test import-default-as-function
  (is (= 2432902008176640000
         (with-import-default (#'fact :from "tests/import-as-function.lsp")
           (fact 20)))))


;;; Prefixes and renaming.

(test (party :compile-at :run-time)
  ;; Reproduces an example from the R6RS spec.
  (with-imports* (party :from "tests/party/party.lisp" :binding :all-as-functions)
    (let ((p (make-party)))
      (is (equal (pop! p) "Boom! 108"))
      (push! p (push* (make 5 5) 1))
      (is (equal (pop! p) "Boom! 24")))))

(test (grid :compile-at :run-time)
  ;; Reproduces an example from the R7RS spec.
  (with-imports* (main :from "tests/grid/main.lisp" :binding ((run :as #'run*)))
    (let* ((sep #\Page)
           (s (with-output-to-string (*standard-output*)
                (run* sep))))
      (is (= (count sep s) 80))
      (let ((frames (split-sequence sep s :remove-empty-subseqs t)))
        (is (= (length frames) 80))
        (is-true (notany #'equal frames (rest frames)))))))


;;; Import as package.

(test import-as-package
  (let ((pkg :overlord-test/as-package))
    (when (find-package pkg)
      (delete-package pkg))
    (eval `(overlord:import-as-package ,pkg
             :from "tests/islisp/exports.lsp"
             :as :core-lisp
             :binding (x #'y (macro-function z))))
    (is-true (find-package pkg))
    (is (equal '(:var :fn :macro)
               (eval `(list ,(find-symbol (string 'x) pkg)
                            (,(find-symbol (string 'y) pkg))
                            (,(find-symbol (string 'z) pkg))))))))


;;; Import sets.

(def-suite import-set :in overlord)

(in-suite import-set)

(test all
  (is (null (expand-import-set :all (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set :all (constantly '(:x)))))

  (is (import-set=
       '(#'x)
       (expand-import-set :all-as-functions
                          (constantly '(:x))))))

(test only
  (is (null
       (expand-import-set '(:only :all)
                          (constantly nil))))

  (is (null
       (expand-import-set '(:only :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(x)
       (expand-import-set '(:only :all x)
                          (constantly '(:x :y :z)))))

  (is (import-set=
       '(#'x)
       (expand-import-set '(:only :all-as-functions #'x)
                          (constantly '(:x :y :z)))))

  ;; Id not present.
  (signals error
    (expand-import-set '(:only :all x)
                       (constantly nil)))

  (signals error
    (expand-import-set '(:only :all x)
                       (constantly '(:a))))

  ;; Importing a var as a function.
  (signals error
    (expand-import-set '(:only :all-as-functions x)
                       (constantly '(:x :y :z))))

  ;; Importing a function as a var.
  (signals error
    (expand-import-set '(:only :all #'x)
                       (constantly '(:x :y :z)))))

(test except
  (is (null
       (expand-import-set '(:except :all)
                          (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set '(:except :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(y z)
       (expand-import-set '(:except :all x)
                          (constantly '(:x :y :z)))))

  (is (import-set=
       '(#'y #'z)
       (expand-import-set '(:except :all-as-functions #'x)
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:except :all x)
                       (constantly nil)))

  (signals error
    (expand-import-set '(:except :all x)
                       (constantly :a))))

(test prefix
  (is (null
       (expand-import-set '(:prefix :all my-)
                          (constantly nil))))

  (is (import-set= '(my-x my-y my-z)
                   (expand-import-set '(:prefix :all my-)
                                      (constantly '(:x :y :z)))))

  (is (import-set= '(nilx nily nilz)
                   (expand-import-set '(:prefix :all nil)
                                      (constantly '(:x :y :z)))))

  (is (import-set= '(#'my-x #'my-y #'my-z)
                   (expand-import-set '(:prefix :all-as-functions my-)
                                      (constantly '(:x :y :z))))))

(test drop-prefix
  (is (null
       (expand-import-set '(:drop-prefix :all my-)
                          (constantly nil))))

  (is (import-set=
       '(x y z)
       (expand-import-set '(:drop-prefix :all my-)
                          (constantly '(:my-x :my-y :my-z)))))

  (is (import-set=
       '(x y z)
       (expand-import-set '(:drop-prefix :all nil)
                          (constantly '(:nilx :nily :nilz)))))

  (is (import-set=
       '(#'x #'y #'z)
       (expand-import-set '(:drop-prefix :all-as-functions my-)
                          (constantly '(:my-x :my-y :my-z))))))

(test rename
  (is (null
       (expand-import-set '(:rename :all)
                          (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set '(:rename :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(a b c)
       (expand-import-set '(:rename :all (x a) (y b) (z c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:rename :all-as-functions (x a) (y b) (z c))
                       (constantly '(:x :y :z))))

  (is (import-set=
       '(#'a #'b #'c)
       (expand-import-set '(:rename :all-as-functions
                            (#'x #'a) (#'y #'b) (#'z #'c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:rename :all
                         (#'x #'a) (#'y #'b) (#'z #'c))
                       (constantly '(:x :y :z)))))

(test alias
  (is (null
       (expand-import-set '(:alias :all)
                          (constantly nil))))

  (is (import-set=
       '(x)
       (expand-import-set '(:alias :all)
                          (constantly '(:x)))))

  (is (import-set=
       '(x y z a b c)
       (expand-import-set '(:alias :all (x a) (y b) (z c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:alias :all-as-functions (x a) (y b) (z c))
                       (constantly '(:x :y :z))))

  (is (import-set=
       '(#'a #'b #'c #'x #'y #'z)
       (expand-import-set '(:alias :all-as-functions
                            (#'x #'a) (#'y #'b) (#'z #'c))
                          (constantly '(:x :y :z)))))

  (signals error
    (expand-import-set '(:alias :all
                         (#'x #'a) (#'y #'b) (#'z #'c))
                       (constantly '(:x :y :z)))))



;;; Core Lisp.

(def-suite islisp :in overlord)

(in-suite islisp)

(test hello-islisp
  (is (equal "hello world"
             (with-imports* (m :from "tests/islisp/islisp.lsp" :binding (hello))
               hello))))

(test islisp-dont-be-shadowed
  (is (equal '(:right :right :right)
             (with-imports* (m :from "tests/islisp/dont-be-shadowed.lsp"
                               :binding (syms (xyz :as #'expand-xyz)))
               (destructuring-bind (x y z) syms
                 (eval
                  `(let ((,x :wrong)) (declare (ignorable ,x))
                     (flet ((,y () :wrong)) (declare (ignore #',y))
                       (macrolet ((,z () :wrong))
                         ,(expand-xyz nil nil))))))))))

(test islisp-imports
  (is (equal '(:var :fn :macro)
             (require-default "tests/islisp/imports.lsp"))))

(test islisp-auto-alias
  (is (equal '(0 1)
             (require-default "tests/islisp/shadowing.lsp"))))

(test islisp-hygiene
  (touch #1="tests/islisp/hygiene.lsp")
  ;; Not the desired results, just the ones we expect.
  (handler-bind ((warning #'muffle-warning))
    (is (equal '(4 6 :ERROR 4 16 :ERROR)
               (require-default #1#)))))

(test islisp-globals-can-close
  "Test that globals defined with `defglobal' close over themselves."
  (with-imports* (m :from "tests/islisp/globals-can-close.lsp" :binding (x))
    (is (eql x (funcall x)))))

(test islisp-phasing
  "Test that state is not preserved across rebuilds."
  (require-as :core-lisp #1="tests/islisp/phasing.lsp")
  (with-imports* (m :from #1# :binding (#'inc-count))
    (is (= (inc-count) 0))))



;;; Sanity checks.

(test db-exists
  (is-true (file-exists-p (overlord/db::log-file-path))))
