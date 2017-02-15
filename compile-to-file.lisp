(defpackage #:overlord/compile-to-file
  (:use #:cl #:alexandria #:serapeum #:overlord/specials)
  (:import-from #:overlord/types #:absolute-pathname)
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

(defun compile-to-file (program output-file
                        &key top-level source
                        &aux (namestring (namestring source)))
  "Compile PROGRAM to a fasl."
  (check-type source absolute-pathname)
  (let ((*program*
          (if top-level
              program
              `(setq *module* ,program))))
    ;; TODO The following is cribbed from the sources of Slime and Sly.
    (with-compilation-unit (:allow-other-keys t
                            ;; SBCL
                            :source-namestring namestring)
      (compile-file universal-file
                    :allow-other-keys t
                    :output-file output-file
                    :external-format :utf-8
                    ;; CCL.
                    :compile-file-original-truename source
                    ;; ECL.
                    :source-truename source
                    ;; Clasp.
                    :source-debug-namestring namestring))))

(defun load-as-module (file)
  "Load FILE and return whatever it assigns to `*module*'."
  (let ((*module* no-module))
    (load file :external-format :utf-8)
    (if (module? *module*)
        *module*
        (error "No module in ~a" file))))
