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

(defunion version-spec
  unversioned
  (version
   (major wholenum)
   (minor wholenum)
   (patch wholenum)))

(defun parse-version (version)
  (typecase version
    (version version)
    ((integer 0 *) (version version 0 0))
    (string (parse-version-string version))
    (otherwise unversioned)))

(defun parse-version-string (string)
  (ematch string
    ;; Disregard leading v (for git tags) and trailing crap. I don't
    ;; think it's worth implementing semver's rules for comparing
    ;; prerelease versions.
    ((ppcre "^v?(\\d+)(?:\\.(\\d+))(?:\\.(\\d+))" major minor patch)
     (version (parse-integer major)
              (if minor (parse-integer minor) 0)
              (if patch (parse-integer patch) 0)))
    (otherwise unversioned)))

(defun version< (version1 version2)
  (let ((v1 (parse-version version1))
        (v2 (parse-version version2)))
    (dispatch-case ((v1 version-spec)
                    (v2 version-spec))
      ((unversioned version-spec) nil)
      ((version-spec unversioned) nil)
      ((version version)
       (let-match (((version major1 minor1 patch1) v1)
                   ((version major2 minor2 patch2) v2))
         (or (< major1 major2)
             (and (= major1 major2)
                  (or (< minor1 minor2)
                      (and (= minor1 minor2)
                           (< patch1 patch2))))))))))

(defun version= (version1 version2)
  (let ((v1 (parse-version version1))
        (v2 (parse-version version2)))
    (dispatch-case ((v1 version-spec)
                    (v2 version-spec))
      ((unversioned version-spec) nil)
      ((version-spec unversioned) nil)
      ((version version)
       (let-match (((version major1 minor1 patch1) v1)
                   ((version major2 minor2 patch2) v2))
         (and (= major1 major2)
              (= minor1 minor2)
              (= patch1 patch2)))))))

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
    (dispatch-case ((version version-spec)
                    (spec version-spec))
      ;; The absence of a version does not satisfy a version.
      ((unversioned version) nil)
      ;; The absence of a version is the absence of constraints;
      ;; anything satisfies it.
      ((version-spec unversioned) t)
      ((version version)
       (and (= (version-major version)
               (version-major spec))
            (version<= spec version))))))
