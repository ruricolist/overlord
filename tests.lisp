(uiop/package:define-package :overlord/tests
    (:use :fiveam)
  (:mix :cl :serapeum :alexandria)
  (:import-from :overlord :with-imports :require-as
    :with-import-default :require-default
    :depends-on
    :cmd :$cmd)
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
  (:export :run-overlord-tests :with-temp-db
           :touch))
(in-package :overlord/tests)

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
        ;; Busy-wait until we can actually delete the temp db (we may
        ;; still be writing to it, regardless of `finish-output').
        (loop (ignore-errors
               (overlord/db:delete-versioned-db)
               (return))
              (sleep 1))))))

(defmacro with-temp-db ((&key) &body body)
  (with-thunk (body)
    `(call/temp-db ,body)))

;;; Running tests.
(defun run-overlord-tests ()
  (let ((overlord:*base* (asdf-system-relative-pathname :overlord ""))
        (fiveam:*on-error* :debug)))
  (with-temp-db ()
    (run! 'overlord)))

;;; Internal use.
(defun debug-test (test)
  (let ((overlord:*base* (asdf-system-relative-pathname :overlord ""))
        (fiveam:*on-error* :debug)
        (fiveam:*on-failure* :debug))
    (run! test)))


;;; Utilities.

(defun resolve-file (file)
  (native-namestring
   (if (absolute-pathname-p file)
       file
       (uiop:merge-pathnames* file (overlord:base)))))

(defun touch-file (file)
  (lret ((file-string (resolve-file file)))
    (if (file-exists-p file-string)
        (if (os-windows-p)
            (run-program
             (fmt "powershell (ls \"~a\").LastWriteTime = Get-Date"
                  (native-namestring file-string)))
            (run-program `("touch" ,file-string)))
        (prog1 (open file-string :direction :probe
                                 :if-does-not-exist :create)
          (assert (file-exists-p file-string))))))

(defun touch (&rest targets)
  (flet ((touch (target)
           (if (typep target '(or pathname string))
               (touch-file target)
               (setf (target-timestamp target) (now)))))
    (mapcar #'touch targets)))

;;; Does the utility work?
(test touch-test
  ;; This is more complicated than you might expect, since I want it
  ;; to be possible to run the test suite simultaneously in more than
  ;; one Lisp instance.
  (let ((file
          (ensure-directories-exist
           (resolve-file
            (make-pathname
             :name "touch-test"
             :directory `(:relative
                          "tests"
                          "tmp"
                          ,(uiop/os:implementation-identifier)))))))
    (unless (file-exists-p file)
      (touch file)
      (nap 1))
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
  (depends-on '+literal-string-file+)
  (depends-on +literal-string-file+))

(overlord:define-target-var *literal-string*
    (read-file-into-string +literal-string-file+)
  (depends-on '+literal-string-file+)
  (depends-on +literal-string-file+))

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

(test force-config
  "Check that forcing doesn't change the timestamp of a config."
  (let ((sym (intern "+HELLO+" :overlord/tests)))
    (eval `(overlord:defconfig ,sym "hello"))
    (unwind-protect
         (let ((stamp (overlord:target-stamp sym)))
           (overlord:build sym :force t)
           (is (eql stamp (overlord:target-stamp sym))))
      (unintern sym))))

(test unbound-config
  "Do the right thing if a config somehow ends up unbound."
  (if (eql :sbcl uiop:*implementation-type*)
      (skip "Can't make configs unbound on SBCL.")
      (let ((sym (intern "+GOODBYE+" :overlord/tests)))
        (eval `(overlord:defconfig ,sym "goodbye"))
        (unwind-protect
             (let ((stamp (overlord:target-stamp sym)))
               (makunbound sym)
               (overlord:build sym)
               (is (eql stamp (overlord:target-stamp sym))))
          (unintern sym)))))


;;; Temporary pathnames.

(defun mktemp ()
  (uiop:with-temporary-file (:pathname d :keep t)
    d))

(test temp-pathname-edit-dest
  (let ((dest (mktemp)))
    (signals overlord-error
      (overlord/util:call/temp-file-pathname
       dest (lambda (out)
              (declare (ignore out))
              (write-string-into-file "hello" dest
                                      :if-exists :supersede))))
    (delete-file dest)))

(test temp-pathname
  (let ((dest (mktemp)))
    (overlord/util:call/temp-file-pathname
     dest (lambda (out)
            (write-string-into-file "hello" out
                                    :if-exists :supersede)))
    (is (equal "hello" (read-file-into-string dest)))
    (delete-file dest)))


;;; Multiple file stamps.

(test multiple-file-stamp
  (let* ((temps (loop repeat 3 collect (mktemp)))
         (stamp (overlord/target::multiple-file-stamp temps)))
    (is (stringp stamp))
    (loop for s in '("x" "y" "z")
          for temp in temps
          do (write-string-into-file s temp :if-exists :supersede))
    (is (not (equal stamp
                    (overlord/target::multiple-file-stamp temps))))
    (mapc #'delete-file temps)))


;;; Tests for external programs.

(test filename-starts-with-dash
  (signals error
    (eval '(cmd "ls" #p"-file"))))

(test unix-cmd
  (if (uiop:os-unix-p)
      (progn
        (is (equal* "hello"
                    ($cmd "echo hello")
                    ($cmd '("echo" "hello"))
                    ($cmd "echo" #p"hello")
                    ($cmd '("echo" #p "hello"))))
        (let ((file (asdf-system-relative-pathname :overlord "tests/literal.txt")))
          (is (equal (read-file-into-string file)
                     ($cmd "cat" file)))))
      (skip "Not on Unix")))


;;; Sanity checks.

(test db-exists
  (let ((path (overlord/db::log-file-path)))
    (is-true (file-exists-p (overlord/db::log-file-path))
             "DB log does not exist: ~a" path)))
