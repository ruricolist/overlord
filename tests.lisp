(uiop/package:define-package :overlord/tests
    (:use :fiveam)
  (:mix :cl :serapeum :alexandria)
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


;;; Sanity checks.

(test db-exists
  (let ((path (overlord/db::log-file-path)))
    (is-true (file-exists-p (overlord/db::log-file-path))
             "DB log does not exist: ~a" path)))
