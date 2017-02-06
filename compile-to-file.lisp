(defpackage #:overlord/compile-to-file
  (:use #:cl #:alexandria #:serapeum #:overlord/specials)
  (:export #:compile-to-file #:load-as-module :fasl-ext :module))
(in-package #:overlord/compile-to-file)

(defconst no-module "no-module")

(defun module? (x)
  (not (eq x no-module)))

(deftype module ()
  '(not (satisfies module?)))

(defconst fasl-ext
  (pathname-type
   (compile-file-pathname
    "foo.lisp"))
  "The extension of a fasl in this Lisp.")

;;; http://kpreid.livejournal.com/14713.html
(def universal-file (asdf:system-relative-pathname :overlord "universal.lisp"))

(defun compile-to-file (program output-file &key top-level)
  "Compile PROGRAM to a fasl."
  (let ((*program*
          (if top-level
              program
              `(setq *module* ,program))))
    (compile-file universal-file
                  :output-file output-file
                  :external-format :utf-8)))

(defun load-as-module (file)
  "Load FILE and return whatever it assigns to `*module*'."
  (let ((*module* no-module))
    (load file :external-format :utf-8)
    (if (module? *module*)
        *module*
        (error "No module in ~a" file))))
