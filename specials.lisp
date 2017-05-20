(in-package #:cl-user)

(defpackage #:overlord/specials
  (:use #:cl :overlord/types)
  (:import-from :serapeum :defvar-unbound :assure)
  (:export #:*base*
           #:*target*
           #:ensure-absolute
           #:*input*
           #:*output*
           #:*module*
           #:*program*
           #:*program-preamble*
           #:*deps*
           #:*language*
           #:*phase*
           #:*source*))
(in-package #:overlord/specials)

(defvar-unbound *base* "The current base.")
(defvar-unbound *input* "Input of a pattern.")
(defvar-unbound *output* "Output of a pattern.")
(defvar-unbound *deps* "Dependencies being collected.")
(defvar-unbound *source* "Source file being compiled.")

(declaim (type absolute-pathname *base* *input* *output* *source*))

(defun ensure-absolute (path)
  "If PATH is relative, merge it with *BASE*."
  (assure absolute-pathname
    (etypecase path
      (absolute-pathname path)
      (relative-pathname
       (if (boundp '*base*)
           (let ((base (uiop:pathname-directory-pathname *base*)))
             (merge-pathnames path base))
           (error* "Can't make absolute: ~a" path))))))

(defvar-unbound *module* "The module being returned.")
(defvar-unbound *program* "The program to be compiled.")

(defvar-unbound *program-preamble*
  "A preamble to the program to be compiled.

This would be something like a package declaration, that the reader
has to see before the other forms.")

(defvar-unbound *language* "The name (symbol) of the current language.")
(declaim (type symbol *language*))
