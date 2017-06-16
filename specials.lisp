(in-package #:cl-user)

(defpackage #:overlord/specials
  (:use #:cl :overlord/types)
  (:import-from :serapeum :defvar-unbound :assure)
  (:export #:*base*
           #:*cli*
           #:*target*
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

(defvar *cli* nil)
(declaim (type boolean *cli*))

(defvar-unbound *module* "The module being returned.")
(defvar-unbound *program* "The program to be compiled.")

(defvar-unbound *program-preamble*
  "A preamble to the program to be compiled.

This would be something like a package declaration, that the reader
has to see before the other forms.")

(defvar-unbound *language* "The name (symbol) of the current language.")
(declaim (type symbol *language*))
