(defpackage :overlord/cache
  (:use :cl :alexandria :serapeum
    :overlord/types)
  (:import-from :overlord/specials :db-version)
  (:import-from :uiop
    :os-windows-p
    :xdg-cache-home
    :ensure-directory-pathname)
  (:export
   :current-cache-dir
   :make-shadow-tree
   :shadow-tree-translate))
(in-package :overlord/cache)

(defstruct-read-only (shadow-tree (:constructor %make-shadow-tree))
  "First-class shadow trees.

A shadow tree is a hidden directory structure (like that used by ASDF
to store fasls) whose subdirectories recapitulate the filesystem
hierarchy from the top level.

Shadow trees are useful for caching files that depend in some
deterministic way on top-level files."
  (prefix :type list))

(defsubst make-shadow-tree (&key prefix)
  "Make a shadow tree with prefix PREFIX.
PREFIX may be a string or a list of strings."
  (let ((prefix (ensure-list prefix)))
    (assert (every #'stringp prefix))
    (%make-shadow-tree :prefix prefix)))

(defun shadow-tree-translate (tree path)
  "Return a path equivalent to PATH in shadow tree TREE.
PATH must be an absolute path."
  (path-join
   (shadow-tree-root tree)
   (absolute-path-shadow-tree-suffix path)))

(defun shadow-tree-root (shadow-tree)
  "Get the root directory of SHADOW-TREE."
  (assure (and absolute-pathname directory-pathname)
    (let ((prefix (shadow-tree-prefix shadow-tree)))
      (path-join
       (current-cache-dir)
       (make-pathname
        :directory `(:relative ,@prefix))))))

(defun current-cache-dir (&optional (version (db-version)))
  "The current Overlord cache directory.
The path includes the Overlord major version, as well as the Lisp
implementation and version, so if Overlord is updated, or the Lisp
implementation is upgraded, the old cache is automatically
invalidated."
  (ensure-directory-pathname
   (xdg-cache-home "overlord"
                   (fmt "v~a" (assure db-version version))
                   :implementation)))

(defun absolute-path-shadow-tree-suffix (path)
  "Turn PATH, an absolute pathname, into a relative pathname whose
directory components are the same as the directory components of
PATH.

On Windows the suffix includes the device as a directory component."
  (~>> path
       (assure absolute-pathname)
       pathname-directory
       (drop-while #'keywordp)
       (append (list :relative)
               (if-let (device
                        (and (os-windows-p)
                             (pathname-device path)))
                 (ensure-list device)
                 nil))
       (make-pathname
        :defaults path
        :device :unspecific
        :directory)
       (assure relative-pathname)))
