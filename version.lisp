(defpackage :overlord/version
  (:use :cl :alexandria :serapeum)
  (:import-from :trivia :match :ematch :let-match)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :trivia.ppcre :ppcre)
  (:export
   :version
   :versionp
   :make-version
   :version=
   :version-compatible?
   :version>
   :version<
   :version=
   :version<=
   :version>=)
  (:documentation "Mostly-trivial parsing and ordering of versions."))
(in-package :overlord/version)

(defunit unversioned)

(defstruct-read-only (version
                      (:predicate versionp))
  (major :type wholenum)
  (minor 0 :type wholenum)
  (patch 0 :type wholenum))

(defmethod print-object ((self version) stream)
  (if (not *print-escape*)
      (format stream "~a.~a.~a"
              (version-major self)
              (version-minor self)
              (version-patch self))
      (call-next-method)))

(deftype version-spec ()
  '(or unversioned version))

(defun version (version)
  (typecase version
    (version version)
    ((integer 0 *) (make-version :major version))
    (string (string->version version))
    (otherwise unversioned)))

(defun string->version (string)
  (ematch string
    ;; Disregard leading v (for git tags) and trailing crap. I don't
    ;; think it's worth implementing semver's rules for comparing
    ;; prerelease versions.
    ((ppcre "^v?(\\d+)(?:\\.(\\d+))(?:\\.(\\d+))" major minor patch)
     (make-version :major (parse-integer major)
                   :minor (if minor (parse-integer minor) 0)
                   :patch (if patch (parse-integer patch) 0)))
    (otherwise unversioned)))

(defun compare-versions (version1 version2)
  (nest
   (let ((v1 (version version1))
         (v2 (version version2))))
   (with-accessors ((major1 version-major)
                    (minor1 version-minor)
                    (patch1 version-patch))
       v1)
   (with-accessors ((major2 version-major)
                    (minor2 version-minor)
                    (patch2 version-patch))
       v2)
   (dispatch-case ((v1 version-spec)
                   (v2 version-spec))
     ((unversioned unversioned) '=)
     ((unversioned version) '/=)
     ((version unversioned) '/=)
     ((version version)
      (cond ((and (= major1 major2)
                  (= minor1 minor2)
                  (= patch1 patch2))
             '=)
            ((or (< major1 major2)
                 (and (= major1 major2)
                      (or (< minor1 minor2)
                          (and (= minor1 minor2)
                               (< patch1 patch2)))))
             '<)
            (t '>))))))

(defsubst version< (v1 v2)
  (eql '< (compare-versions v1 v2)))

(defsubst version> (v1 v2)
  (eql '> (compare-versions v1 v2)))

(defsubst version= (v1 v2)
  (eql '= (compare-versions v1 v2)))

(defsubst version<= (v1 v2)
  (case (compare-versions v1 v2)
    ((< =) t)))

(defsubst version>= (v1 v2)
  (case (compare-versions v1 v2)
    ((> =) t)))

(defun version-compatible? (version spec)
  (let ((version (version version))
        (spec (version spec)))
    (dispatch-case ((version version-spec)
                    (spec version-spec))
      ;; If a formerly versioned dependency is no longer versioned, it
      ;; should be rebuilt.
      ((unversioned version) nil)
      ;; If a formerly unversioned dependency has acquired a version,
      ;; it should be rebuilt.
      ((version-spec unversioned) nil)
      ((version version)
       (and (= (version-major version)
               (version-major spec))
            (version<= spec version))))))
