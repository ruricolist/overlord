(defpackage :overlord/version
  (:use :cl :alexandria :serapeum)
  (:import-from :trivia :match :ematch :let-match)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :trivia.ppcre :ppcre)
  (:export
   :version
   :version=
   :version-satisfies?
   :version>
   :version<
   :version=
   :version<=
   :version>=)
  (:documentation "Mostly-trivial parsing and ordering of versions."))
(in-package :overlord/version)

(defconstructor version
  (major wholenum)
  (minor wholenum)
  (patch wholenum))

(defun parse-version (version)
  (match version
    ((and _ (type version))
     version)
    ((and _ (type (integer 0 *)))
     (version version 0 0))
    ;; Disregard leading v (for git tags) and trailing crap. I don't
    ;; think it's worth implementing semver's rules for comparing
    ;; prerelease versions.
    ((ppcre "^v?(\\d+)(?:\\.(\\d+))(?:\\.(\\d+))" major minor patch)
     (version (parse-integer major)
              (if minor (parse-integer minor) 0)
              (if patch (parse-integer patch) 0)))
    (otherwise
     (error "~a is not a valid version designator." version))))

(defun version< (version1 version2)
  (let-match (((version major1 minor1 patch1)
               (parse-version version1))
              ((version major2 minor2 patch2)
               (parse-version version2)))
    (or (< major1 major2)
        (and (= major1 major2)
             (< minor1 minor2)
             (and (= minor1 minor2)
                  (< patch1 patch2))))))

(defun version= (version1 version2)
  (let-match (((version major1 minor1 patch1)
               (parse-version version1))
              ((version major2 minor2 patch2)
               (parse-version version2)))
    (and (= major1 major2)
         (= minor1 minor2)
         (= patch1 patch2))))

(defsubst version> (version1 version2)
  (version< version2 version1))

(defsubst version<= (version1 version2)
  (or (version< version1 version2)
      (version= version1 version2)))

(defsubst version>= (version1 version2)
  (or (version< version2 version1)
      (version= version1 version2)))

(defun version-satisfies? (version spec)
  (let ((version (parse-version version))
        (spec (parse-version spec)))
    (and (= (version-major version)
            (version-major spec))
         (version<= spec version))))
