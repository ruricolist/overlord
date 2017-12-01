(defpackage #:overlord/scripts
  (:use #:cl :command-line-arguments :serapeum)
  (:import-from #:overlord #:build #:run)
  (:import-from #:overlord/safer-read
    #:safer-read-from-string)
  (:import-from #:overlord/specials #:*cli*)
  (:import-from :alexandria :read-file-into-string)
  (:import-from :uiop :quit)
  (:export #:main))
(in-package #:overlord/scripts)

(defvar *verbose* nil)

(def cli-spec
  '((("verbose" #\v) :type boolean :optional t :documentation "be verbose")
    (("help" #\h #\?) :type boolean :optional t :documentation "be helpful")
    (("version" #\V) :type boolean :optional t :documentation "print version")
    (("system") :type string :optional t :documentation "name of ASDF system")
    (("package") :type string :optional t :documentation "name of Lisp package")))

(defun die (control &rest args)
  (format t "Overlord: ~?~%" control args)
  (quit -1))

(defun out (control &rest args)
  (when *verbose*
    (format t "Overlord: ~?~%" control args)))

(defun maybe-muffle-warning (w)
  (unless *verbose*
    (muffle-warning w)))

(defun print-version ()
  (princ (read-version)))

(defun read-version ()
  (read-file-into-string (version-file)))

(defun version-file ()
  (asdf:system-relative-pathname :overlord "version.sexp"))

(defun main (argv)
  (handler-case
      (progn
        (handle-command-line
         cli-spec
         'handle-args
         :command-line argv
         :name "Overlord"
         :positional-arity 2
         :rest-arity nil)
        (quit 0))
    (error (e)
      (print e)
      (quit -1))))

(defun handle-args (cmd target &key ((:verbose *verbose*) nil)
                                    help version
                                    ((:system system-name) nil)
                                    ((:package package-name) nil)
                    &aux (*cli* t))
  (check-type target string)
  (setf target (string-invert-case target))
  (when (find #\: target)
    ;; dsetq?
    (setf (values package-name target)
          (values-list
           (assure (tuple string string)
             (split-sequence #\: target :count 2)))))
  (setf package-name (or package-name system-name))
  (handler-bind ((warning #'maybe-muffle-warning))
    (cond (help
           (show-option-help cli-spec :sort-names t)
           (quit))
          (version
           (print-version)
           (quit))
          ((equal cmd "build")
           (let ((target (safer-read-from-string target)))
             (build target)
             (out "Built ~a.~%" target)))
          ((equal cmd "run")
           (multiple-value-bind (target system-name package)
               (run (list package-name target) system-name)
             (out "Built target ~s in system ~s (package ~s).~%"
                  target system-name package)))
          (t (die "Invalid cmd: ~a~%" cmd)))))
