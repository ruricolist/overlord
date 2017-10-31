(in-package #:cl-user)

(defpackage #:overlord/specials
  (:use #:cl :overlord/types)
  (:import-from :serapeum :defvar-unbound :assure)
  (:export #:*base*
           #:*cli*
           #:*input*
           #:*output*
           #:*module*
           #:*program*
           #:*program-preamble*
           #:*deps*
           #:*language*
           #:*phase*
           #:*source*
           #:*depth*
           #:*fasl-version*
           #:*1* #:*2* #:*3*))
(in-package #:overlord/specials)

(defvar-unbound *base* "The current base.")
(defvar-unbound *input* "Input of a pattern.")
(defvar-unbound *output* "Output of a pattern.")
(defvar-unbound *deps* "Dependencies being collected.")
(defvar-unbound *source* "Source file being compiled.")

(defvar-unbound *1* "For a file, the target being built.")

(defvar-unbound *2* "For a file, the target being built.
If the file is being built from a pattern, this is bound to the name of the file less the extension associated with that pattern.")

(defvar-unbound *3* "The temporary file that will be renamed to the target file.")

(declaim (type (and directory-pathname absolute-pathname) *base*))

(declaim (type absolute-pathname *input* *output* *source*))

(defvar *cli* nil "Are we running on a CLI?")
(declaim (type boolean *cli*))

(defvar-unbound *module* "The module being returned.")
(defvar-unbound *program* "The program to be compiled.")

(defvar-unbound *program-preamble*
  "A preamble to the program to be compiled.

This would be something like a package declaration, that the reader
has to see before the other forms.")

(defvar-unbound *language* "The name (symbol) of the current language.")
(declaim (type symbol *language*))

(defvar *depth* 0)
(declaim (type (integer 0 #.most-positive-fixnum) *depth*))

(defparameter *fasl-version* 16
  "Versioning for fasls.
Incrementing this should be sufficient to invalidate old fasls.")
(declaim (type fasl-version *fasl-version*))
