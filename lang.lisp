;; (delete-package :overlord/lang)
(cl:defpackage :overlord/lang
  (:use :cl :alexandria :serapeum

    :overlord/redo
    ;; Timestamps.
    :overlord/stamp
    ;; Resettable global state.
    :overlord/global-state
    ;; Types common to the project.
    :overlord/types
    ;; Special variables.
    :overlord/specials
    ;; Module protocol.
    :overlord/module
    ;; Utilities.
    :overlord/util
    ;; How to infer the base for the current package.
    :overlord/base
    ;; The #lang syntax.
    :overlord/hash-lang
    ;; Import sets.
    :overlord/import-set
    ;; Logging.
    :overlord/message
    ;; Freezing.
    :overlord/freeze

    :uiop/filesystem
    :uiop/pathname)
  (:import-from :trivia :let-match1)
  (:shadow :import)
  (:import-from :overlord/compile-to-file
    :compile-to-file :load-as-module :fasl-ext)
  (:import-from :overlord/db
    :db-version-dir)
  (:import-from :uiop
    :xdg-cache-home)
  (:import-from :quri :url-encode)
  (:import-from :overlord/target :module-spec :extension)
  (:export))
(cl:in-package :overlord/lang)


;;; Types.

(deftype lang-name ()
  ;; Keywords can be language names.
  '(and symbol (not (member t nil))))

(deftype hash-lang-name ()
  '(and lang-name (satisfies hash-lang-name?)))

(defun hash-lang-name? (x)
  (and (typep x 'lang-name)
       (valid-lang-name? (string x))))

(deftype lang ()
  '(or package lang-name))


;;; Module cells.

;;; What's a module cell? What we want is a namespace for modules,
;;; indexed by language and source file. The obvious thing would be to
;;; declare a table (with `defvar') store modules there. Basically,
;;; that is what we do. But instead of simply storing the module in
;;; the table, we store an indirection -- a "module cell", a mutable
;;; cell which contains the actual module. Then, using compiler macros
;;; and `load-time-value', we can inject direct references to module
;;; cells into the compiled code. The result is that, for inline
;;; references to modules, no run-time lookup is necessary. This is
;;; key to keeping modules fast while also allowing for modules to be
;;; redefined. And ultimately it is similar to the strategy that Lisp
;;; itself uses to allow functions to be redefined at runtime without
;;; having to recompile all their callers.

(define-global-state *module-cells* (dict)
  "The global table of all module cells.")

(defun list-module-cells ()
  (hash-table-values *module-cells*))

;;; TODO Should this be a structure? But using a class gets us slot
;;; type checking on both SBCL and Clozure. A future optimization, but
;;; we can leave it for now.
(defclass module-cell ()
  ((timestamp
    :type target-timestamp
    :initform never
    :accessor module-cell.timestamp)
   (lang
    :initarg :lang
    :type lang-name
    :reader module-cell.lang)
   (source
    :initarg :source
    :type (and file-pathname tame-pathname)
    :accessor module-cell.source)
   (meta
    :initform nil
    :type plist
    :accessor module-cell.meta
    :documentation "Metadata about the module. This persists even when
the module is reloaded.")
   (module
    :initform nil
    :accessor module-cell.module)
   (lock
    :reader monitor))
  (:documentation "Storage for a module.

Each lang+source combination gets its own module cell with its own
unique identity.

The module itself may be reloaded, but the module cell is interned
forever."))

(defun clear-module-cells ()
  "Delete information not needed at runtime from module cells."
  ;; We don't actually clear the table because there may be cases
  ;; where expansion of compiler macros has been suppressed by
  ;; optimization settings and there is no reference to the module
  ;; cell to keep it from being garbage-collected.
  (maphash (lambda (k mc) (declare (ignore k))
             (with-slots (source timestamp) mc
               (setf source *nil-pathname*
                     timestamp never)))
           *module-cells*))

;;; Compiler macro needs to appear as soon as possible to satisfy
;;; SBCL.
(define-compiler-macro module-cell (&whole call lang path)
  (cond ((packagep lang)
         `(module-cell ,(lang-name lang) ,path))
        ((or (quoted-symbol? lang) (keywordp lang))
         (typecase-of (or string pathname) path
           (string `(module-cell ,lang ,(ensure-pathname path :want-pathname t)))
           (pathname
            (let ((path (resolve-target path (base)))) ;Resolve now, while `*base*' is bound.
              `(load-time-value
                (locally
                    ;; Prevent recursive expansion.
                    (declare (notinline module-cell))
                  ;; We can't use %ensure-module-cell directly,
                  ;; because it doesn't apply the defaults for the
                  ;; language.
                  (module-cell ,lang ,path)))))
           (otherwise call)))
        ((constantp lang)
         (let ((val (eval lang)))
           (if (eql val lang) call
               `(module-cell ,val ,path))))
        (t call)))

(defun module-spec-cell (spec)
  (let-match1 (module-spec lang path) spec
    (module-cell lang path)))

(defun module-cell-spec (cell)
  (with-slots (lang source) cell
    (module-spec lang source)))

(defun module-cell-meta (cell key)
  (synchronized (cell)
    (getf (module-cell.meta cell) key)))

(defun (setf module-cell-meta) (value cell key)
  (synchronized (cell)
    (setf (getf (module-cell.meta cell) key)
          value)))

(defplace module-meta (lang path key)
  (module-cell-meta (module-cell lang path) key))

(define-compiler-macro module-meta (lang path key)
  "Expand the call to module-cell at compile time so it can be
resolved at load time."
  `(module-cell-meta (module-cell ,lang ,path) ,key))

(define-compiler-macro (setf module-meta) (value lang path key)
  `(setf (module-cell-meta (module-cell ,lang ,path) ,key) ,value))

(defmethods module-cell (self lock source lang module)
  (:method initialize-instance :after (self &key)
    ;; Give the lock a name.
    (setf lock (bt:make-lock (fmt "Lock for module ~a" self))))

  (:method module-ref (self name)
    (module-ref* module name))

  (:method module-exports (self)
    (module-exports module))

  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a (~a) (~:[not loaded~;loaded~])"
              source
              lang
              module))))

(defun load-module-into-cell (cell)
  (lret ((module
          (assure (not null)
            (load-module (module-cell.lang cell)
                         (module-cell.source cell)))))
    (setf
     (module-cell.module cell) module
     (module-cell.timestamp cell) (now))))

(defun unload-module (lang source)
  (declare (notinline module-cell))
  (lret ((m (module-cell lang source)))
    (with-slots (timestamp module) m
      (setf timestamp never)
      (nix module))))

(defun %ensure-module-cell (lang path)
  "Get the module cell for LANG and PATH, creating and interning one
if it does not exist."
  (check-type path absolute-pathname)
  (setf path
        (assure pathname
          (or (truename* path)
              (or (progn
                    (redo path)
                    (truename* path))
                  (error "Cannot resolve pathname ~a" path)))))
  (mvlet* ((key (cons lang path))
           (cell cell?
            (gethash key *module-cells*)))
    (if cell? (assure module-cell cell)
        (let ((cell (make 'module-cell :lang lang :source path)))
          (setf (gethash key *module-cells*) cell)))))

(defun module-cell (lang path)
  (let ((path
          (assure absolute-pathname
            (merge-input-defaults lang (ensure-pathname path :want-pathname t))))
        (lang (lang-name lang)))
    (%ensure-module-cell lang path)))

(defun find-module (lang source)
  (module-cell.module (module-cell lang source)))

(define-compiler-macro find-module (lang source)
  `(module-cell.module (module-cell ,lang ,source)))


;;; Lazy-loading modules.

(defun load-module/lazy (lang source)
  (load-module-into-cell/lazy
   (module-cell lang source)))

(define-compiler-macro load-module/lazy (lang path)
  `(load-module-into-cell/lazy (module-cell ,lang ,path)))

(defun load-module-into-cell/lazy (cell)
  (with-slots (module lang source) cell
    ;; "Double-checked locking."
    (or module
        (synchronized (cell)
          (or module
              (progn
                (redo (module-cell-spec cell))
                module))))))


;;; Languages

;;; Note that support for languages follows support for file patterns.
;;; A pattern is an abstract relationship between two files; a
;;; language is an abstract relationship between a file and Lisp
;;; binding.

(defun %require-as (lang source *base* &rest args)
  (ensure-pathnamef source)
  (apply #'dynamic-require-as
         lang
         (merge-pathnames* source *base*)
         args))

(defun dynamic-require-as (lang source &key force)
  (check-type source (and absolute-pathname file-pathname))
  (ensure lang (guess-lang+pos source))
  (setf lang (lang-name lang))
  (when force
    (dynamic-unrequire-as lang source))
  (assure (not module-cell)
    (let ((spec (module-spec lang source)))
      (redo-ifchange spec)
      (~> spec
          module-spec-cell
          module-cell.module))))

(defun %unrequire-as (lang source *base*)
  (dynamic-unrequire-as lang
                        (merge-pathnames* source *base*)))

(defun dynamic-unrequire-as (lang source)
  (check-type source (and absolute-pathname file-pathname))
  (unload-module lang source)
  (values))

(defmacro require-as (lang source)
  "Wrap `%require-as`, resolving the base at macro-expansion time."
  `(%require-as ,lang ,source ,(base)))

(defun require-for-emacs (lang source)
  "Like `dynamic-require-as', but with looser restrictions for easy
interoperation with Emacs."
  (let ((lang
          (resolve-lang lang))
        (source
          (assure absolute-pathname
            (ensure-pathname source))))
    (dynamic-require-as lang source)
    (values)))

(defmacro unrequire-as (lang source)
  "Wrap `%unrequire-as', resolving the base at macro-expansion time."
  `(%unrequire-as ,lang ,source ,(base)))

(defun escape-lang-name (lang-name)
  (check-type lang-name lang-name)
  (url-encode (string lang-name) :encoding :utf-8))

(defun lang-fasl-dir (lang current-dir)
  (assure (and absolute-pathname directory-pathname)
    (let ((lang-string (escape-lang-name lang))
          (suffix
            (make-pathname :directory
                           (cons :relative
                                 (drop-while #'keywordp
                                             (pathname-directory current-dir))))))
      (path-join
       (db-version-dir)
       #p"fasls/"
       (make-pathname :directory `(:relative ,lang-string))
       suffix))))

(defun faslize (lang pathname)
  (etypecase-of lang lang
    (package
     (~> lang
         package-name-keyword
         (faslize pathname)))
    (lang-name
     (path-join (lang-fasl-dir lang pathname)
                (make-pathname :name (pathname-name pathname)
                               :type fasl-ext)))))

(defun fasl? (pathname)
  (let ((type (pathname-type pathname)))
    (and (stringp type)
         (string= type fasl-ext))))

(defun load-module (lang source)
  (ensure-pathnamef source)
  (let ((*base* (pathname-directory-pathname source)))
    (load-fasl-lang lang source)))

(defun module-static-exports (lang source)
  (ensure-static-exports lang source)
  (assure list
    (snarf-static-exports lang source)))

(defun ensure-static-exports (lang source)
  (let ((*base* (pathname-directory-pathname source)))
    (~> lang
        (static-exports-pattern source)
        (pattern-ref source)
        redo-ifchange)))

(defun extract-static-exports (lang source)
  (check-type source absolute-pathname)
  (let ((lang (resolve-lang-package lang)))
    (if-let (sym (find-external-symbol 'static-exports lang))
      ;; If the language exports a function to parse static exports,
      ;; use it.
      (funcall sym source)
      ;; Otherwise, get the list of exports by loading the module at
      ;; compile time.
      (module-exports (dynamic-require-as lang source)))))

(defclass static-exports-pattern (pattern)
  ((lang :initarg :lang)
   (source :initarg :source))
  (:documentation "Pattern to extract static exports from a file."))

(defmethods static-exports-pattern (self lang source)
  (:method pattern.output-defaults (self)
    ;; Bear in mind *input* may have been resolved.
    (path-join (faslize lang source)
               (extension "static-exports")))

  (:method pattern-build (self)
    (save-static-exports lang source)
    (redo-ifchange (fasl-lang-pattern-ref lang source))))

(defun static-exports-pattern (lang source)
  ;; The static export file depends on the fasl.
  (make 'static-exports-pattern :lang lang :source source))

(defun snarf-static-exports (lang source)
  (let ((file (static-exports-file lang source)))
    (assert (file-exists-p file))
    (read-file-form file)))

(defun save-static-exports (lang source)
  (let ((exports (extract-static-exports lang source))
        (file (static-exports-file lang source)))
    (write-form-as-file exports file)))

(def static-exports-extension (extension "static-exports"))

(defun static-exports-file (lang source)
  (merge-pathnames*
   static-exports-extension
   (faslize lang source)))

(defun module-dynamic-exports (lang source)
  (module-exports (dynamic-require-as lang source)))


;;; Languages.

;;; This is a generic function so individual langs can define their
;;; own dependencies in :after methods.

;;; TODO Should this use the progn method combination?
(defgeneric lang-deps (lang source)
  (:method ((lang t) (source t))
    nil)

  (:method ((lang symbol) (source t))
    (lang-deps (resolve-lang-package lang) source)))

(defmethod pattern.input-defaults ((lang symbol))
  (let ((p (resolve-package lang)))
    (if p (pattern.input-defaults p) *nil-pathname*)))

(defmethod pattern.input-defaults ((p package))
  (let ((sym (find-symbol #.(string 'extension) p)))
    (or (and sym (symbol-value sym))
        *nil-pathname*)))

(defmacro define-loader-language (package-name (source) &body (reader &key extension))
  (let* ((pn (string package-name)))
    ;; Sanity check: are we overwriting an existing package?
    (when-let (package (find-package pn))
      (when (package-use-list package)
        (error* "Package already exists with a use list: ~a" package))
      (unless (set-equal (package-exports package)
                         (loader-language-exports)
                         :test #'string=)
        (error* "Package already exists with wrong exports: ~a" package)))
    `(progn
       (defpackage ,pn
         (:use)
         (:export ,@(loader-language-exports)))
       (define-loader-language-1 ,pn (,source)
         ,reader
         :extension ,extension))))

(defmacro define-loader-language-1 (package-name (source) &body (reader &key extension))
  "The part that gets expanded once PACKAGE-NAME exists."
  (let* ((p (find-package package-name))
         (syms (mapcar (op (find-symbol (string _) p))
                       (loader-language-exports)))
         (keyword (package-name-keyword package-name)))
    (destructuring-bind (load read ext script) syms
      `(progn
         (declaim (notinline ,load ,read))
         (eval-always
           (define-script ,script ,reader)
           (defparameter ,ext (extension ,extension))
           (defun ,load (,source)
             ,reader)
           (defun ,read (,source _stream)
             (declare (ignore _stream))
             (list ',load ,source))
           (defmethod lang-deps :after ((self (eql ,keyword)) source)
             (declare (ignore source))
             (redo-ifchange ',script)))))))

(defun load-fasl-lang (lang source)
  (let ((object-file (faslize lang source)))
    (restart-case
        (load-as-module object-file)
      (recompile-object-file ()
        :report "Recompile the object file."
        (delete-file-if-exists object-file)
        (redo (module-spec lang source))
        (load-fasl-lang lang source)))))

(defmethod lang-deps ((lang package) (source cl:pathname))
  (let* ((pat (fasl-lang-pattern lang source))
         (ref (pattern-ref pat source)))
    (redo-ifchange ref)))

(defmethod unbuild-lang-deps ((lang package) (source cl:pathname))
  (delete-file-if-exists (faslize lang source)))

(defun lang-name (lang)
  (assure lang-name
    (etypecase-of (or keyword lang-name package) lang
      (keyword lang)
      (lang-name (make-keyword lang))
      (package (package-name-keyword lang)))))

;;; This can't use `defpattern', for bootstrapping reasons.

(defclass fasl-lang-pattern (pattern)
  ((lang :initarg :lang)
   (source :initarg :source)))

(defmethods fasl-lang-pattern (self lang source)
  (:method pattern.output-defaults (self)
    (faslize lang source))

  (:method pattern-build (self)
    (let* ((*source* *input*)
           (lang (lang-name lang))
           (*language* lang)
           ;; Must be bound here for macros that intern
           ;; symbols.
           (*package* (user-package (resolve-package lang))))
      (compile-to-file
       (wrap-current-module
        (expand-module lang *input*)
        lang *input*)
       (ensure-directories-exist *output*)
       :top-level (package-compile-top-level? lang)
       :source *source*))))

(defun fasl-lang-pattern (lang source)
  (make 'fasl-lang-pattern :lang lang :source source))

(defun fasl-lang-pattern-ref (lang source)
  (pattern-ref (fasl-lang-pattern lang source) source))

(defmacro with-input-from-source ((stream source) &body body)
  "Read from SOURCE, skipping any #lang declaration."
  `(with-input-from-file (,stream ,source :element-type 'character)
     (skip-hash-lang ,stream)
     ,@body))

(def reader-string (string 'read-module))

(def module-string (string 'module-progn))

(def compile-top-level-string (string '*compile-top-level*))

(def loader-language-exports
  (list (string 'load)
        reader-string
        (string 'extension)
        (string 'script)))

;;; Make it a function so it can be used before defined.
(defun loader-language-exports ()
  loader-language-exports)

(defmacro module-progn-in (package &body body &environment env)
  "Resolve a package's expander at macro-expansion time.
Also, ensure that PACKAGE is the current package when BODY is
macro-expanded.

If PACKAGE does not export an expander, `progn' is used instead."
  ;; Is expanding the macro this way useful?
  (let* ((package-expander (package-expander package :errorp nil))
         (module-progn (or package-expander 'progn))
         (form `(,module-progn ,@body)))
    (expand-in-package form package env)))

(defun suffix-package (package suffix)
  "Like `resolve-package' but, if a package exists with the same name,
but ending in SUFFIX, and inheriting from that package, return that
instead."
  (assert (string^= "-" suffix))
  (assure package
    (with-absolute-package-names ()
      (when-let (base-package (resolve-package package))
        (let* ((user-package-name
                 (concatenate 'string
                              (package-name base-package)
                              suffix))
               (user-package (find-package user-package-name)))
          (or (and user-package
                   (find base-package (package-use-list user-package))
                   user-package)
              base-package))))))

(defun user-package (package)
  "Like `resolve-package' but, if a package exists with the same name,
but ending in `-user', and inheriting from that package, return that
instead."
  (suffix-package package "-USER"))

;;; TODO Is this useful?
(defun expand-in-package (form package env)
  (let ((*package* (user-package (resolve-package package))))
    (macroexpand-1 form env)))

(defun cl-read-module (source stream)
  (declare (ignore source))
  (let ((eof "eof"))
    `(progn
       ,@(loop for form = (read stream nil eof)
               until (eq form eof)
               collect form))))

(defun package-compile-top-level? (package)
  (and-let* ((sym (find-symbol compile-top-level-string package))
             ((boundp sym)))
    (symbol-value sym)))

(defun package-reader (package &key (errorp t))
  "Resolve the reader exported by PACKAGE."
  (flet ((error* (&rest args)
           (if errorp
               (apply #'error* args)
               (return-from package-reader nil))))
    (assure (or symbol null)
      (let ((p (resolve-package package)))
        (if (eql p (find-package :cl))
            'cl-read-module
            (receive (sym status) (find-symbol reader-string p)
              (cond ((no sym)
                     ;; There is no symbol.
                     (error* "No reader defined in package ~a" p))
                    ((not (eql status :external))
                     ;; There is a symbol, but it's not external.
                     (error* "Package ~a does not export a reader" p))
                    ((not (fboundp sym))
                     ;; There is an external symbol, but it's not
                     ;; fbound.
                     (error* "No binding for reader in package ~a" p))
                    (t
                     (unless (eql (symbol-package sym) p)
                       (simple-style-warning "Package reader ~a in ~a is inherited from ~a."
                                             sym p (symbol-package sym)))
                     sym))))))))

(defun reintern (s &aux (p *package*))
  (let ((s (string s)))
    (or (find-symbol s p)
        (error "No symbol named ~a in ~s" s p))))

(defmacro reinterning ((&rest names) &body body)
  `(let ,(loop for name in names
               collect `(,name (reintern ',name)))
     ,@body))

(defun package-expander (package &key (errorp t))
  "Resolve the expander exported by PACKAGE."
  (flet ((error* (&rest args)
           (if errorp
               (apply #'error* args)
               (return-from package-expander nil))))
    (assure (or symbol null)
      (let ((p (resolve-package package)))
        (receive (sym status) (find-symbol module-string p)
          (cond ((no sym)
                 (error* "No expander defined in package ~a" p))
                ((not (eql status :external))
                 (error* "Package ~a does not export an expander" p))
                ((not (fboundp sym))
                 (error* "Expander in package ~a is exported but unbound" p))
                ((not (macro-function sym))
                 (error* "Package ~a exports an expander that is not a macro" p))
                (t
                 (unless (eql (symbol-package sym) p)
                   (simple-style-warning "Package expander ~a in ~a is inherited from ~a."
                                         sym p (symbol-package sym)))
                 sym)))))))

(defparameter *file-local-variables*
  '(*package* *readtable*
    ;; These seem like a good idea to me.
    *read-base* *read-default-float-format*
    *file-local-variables*)
  "Variables that should be given fresh rebindings while reading in a
  module.

This should be a superset of the variables bound by CL during calls to
`cl:load'.")

(defun expand-module (package source
                      &aux (file-locals *file-local-variables*))
  (let* ((package (resolve-package package))
         (*language* (lang-name package))
         (source (ensure-pathname source :want-pathname t))
         (*source* source))
    (with-input-from-source (in source)
      (progv file-locals (mapcar #'symbol-value file-locals)
        (let* ((reader (package-reader package))
               (module-form
                 (let ((*package* (user-package package)))
                   (funcall reader source in))))
          module-form)))))

(defun expand-module-for-emacs (lang source)
  (setf lang (resolve-lang lang))
  (values (expand-module lang source)))

(defmacro with-current-module ((lang source) &body body)
  `(macrolet ((current-module-lang () ',lang)
              (current-module-source () ',source)
              (current-module-cell ()
                `(module-cell ',',lang ,',source))
              (current-module ()
                `(find-module ',',lang ,',source))
              (current-module-meta (key)
                `(module-meta ',',lang ,',source ,key)))
     ,@body))

(defun wrap-current-module (form package source)
  (let ((lang (lang-name package)))
    `(with-current-module (,lang ,source)
       ,form)))


;;; #lang syntax.

(defcondition no-such-lang (overlord-error)
  ((lang :initarg :lang :type string-designator
         :reader no-such-lang.lang))
  (:report (lambda (c s)
             (with-slots (lang) c
               (format s "No such language as ~a" lang)))))

(defun load-same-name-system (c)
  (declare (ignore c))
  (invoke-restart 'load-same-name-system))

(defgeneric find-asdf-system (lang)
  (:method ((lang no-such-lang))
    (find-asdf-system (no-such-lang.lang lang)))
  (:method ((lang t))
    (and (not (frozen?))
         (let ((lang (string-downcase lang)))
           (asdf:find-system lang nil)))))

(defun ensure-lang-exists (lang &optional (cont #'identity))
  (check-type lang package-designator)
  (check-type cont function)
  (if (packagep lang) lang
      (let ((pkg (resolve-package lang)))
        (or (and pkg (package-name-keyword pkg))
            (restart-case
                (error 'no-such-lang :lang lang)
              (load-same-name-system ()
                :test find-asdf-system
                :report (lambda (s)
                          (format s "Load the system named ~a and try again" lang))
                (asdf:load-system lang)
                (funcall cont lang)))))))

(defun lookup-hash-lang (name)
  (assure (or null lang-name)
    (let* ((pkg-name (assure (satisfies valid-lang-name?)
                       ;; Set the case as if the string were being
                       ;; read, without using `read`.
                       (coerce-case name))))
      (ensure-lang-exists pkg-name #'lookup-hash-lang))))

(defun guess-lang+pos (file)
  "If FILE has a #lang line, return the lang and the position at which
the #lang declaration ends."
  (receive (lang pos)
      (file-hash-lang file)
    (if (stringp lang)
        (values (lookup-hash-lang lang) pos)
        (values nil 0))))

(defun resolve-lang (lang)
  (assure lang-name
    (etypecase-of (or lang-name string) lang
      (string (lookup-hash-lang lang))
      (lang-name lang))))

(defun resolve-lang-package (lang)
  (assure package
    (resolve-package (resolve-lang lang))))

(defmacro with-meta-language ((path stream) &body body)
  (with-thunk (body path stream)
    `(call/meta-language ,body ,path ,stream)))

(defun call/meta-language (fn path stream)
  (let* ((next-lang (read-lang-name stream))
         (package (resolve-lang-package next-lang))
         (user-package (user-package package))
         (*package* user-package)
         (forms (funcall fn path stream)))
    `(module-progn-in ,(package-name-keyword package)
       ,@forms)))


;;; Importing.

;;; It might seem like this could be moved into a separate file, but
;;; that would requiring exposing too much of the above.

;;; Note that the import macros defined here expand into definition
;;; forms from overlord/shadows rather than from cl proper. (E.g.
;;; `overlord/shadows:defun' rather than `cl:defun'.) This is so
;;; languages that need to handle imports specially (e.g. Core Lisp)
;;; can do so simply by shadowing the relevant definition forms with
;;; `macrolet', instead of having to re-implement everything.

;;; Hopefully most, if not all, of this code will be replaced once we
;;; have a full implementation of import sets.

(define-global-state *always-import-values* nil
  "Flag to control importing behavior.
When this is T, imports should always be values, never bindings.

This is intended to be used when saving an image, where you don't care
about ease of development or debugging, only speed.")
(declaim (type boolean *always-import-values*))

(defcondition bad-macro-import (overlord-error)
  ((name :initarg :name :type symbol
         :documentation "The name of the macro."))
  (:documentation "Invalid attempt to import something as a macro."))

(defcondition module-as-macro (bad-macro-import)
  ()
  (:documentation "Attempt to import a module as a macro.")
  (:report (lambda (c s)
             (with-slots (name) c
               (format s "Cannot import a module as a macro: ~a"
                       name)))))

(defcondition macro-as-value (bad-macro-import)
  ()
  (:documentation "Attempt to import a macro as a value.")
  (:report (lambda (c s)
             (with-slots (name) c
               (format s "Cannot import a macro as a value: ~a."
                       name)))))

(defun expand-binding-spec (spec lang source)
  (setf source (merge-pathnames source (base))
        lang (lang-name lang))
  (flet ((get-static-exports ()
           ;; This doesn't save any work. The static bindings are
           ;; always computed every time we import from a module. But
           ;; we still only want to compute them here if we absolutely
           ;; have to. Why? For friendlier debugging. Doing the check
           ;; here would prevent us from macroexpanding `import' at
           ;; all if there was a problem with the imports, which is
           ;; frustrating. Instead, we push the check down into the
           ;; `check-static-bindings-now' macro.
           (module-static-exports/cache lang source)))
    (etypecase-of binding-spec spec
      ((eql :all)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(,export :as ,sym)))
      ((eql :all-as-functions)
       (loop for export in (get-static-exports)
             for sym = (intern (string export))
             collect `(,export :as #',sym)))
      ((tuple :import-set list)
       (let ((import-set (second spec)))
         (expand-import-set import-set #'get-static-exports)))
      (list spec))))

(defun guess-source (lang alias)
  (~>> (etypecase-of import-alias alias
         (var-alias alias)
         ((or function-alias macro-alias)
          (second alias)))
       string-downcase
       (make-pathname :name)
       (merge-input-defaults lang)))

(defmacro function-wrapper (fn)
  "Global definition for possible shadowing."
  fn)

(define-global-state *claimed-module-names* (make-hash-table :size 1024)
  "Table to track claimed modules, so we can warn if they are
  redefined.")

(def module-name-lock (bt:make-lock "Module name lock"))

(defun claim-module-name (module lang source)
  "Warn if MODULE is already in use with a different LANG and SOURCE."
  (synchronized (module-name-lock)
    (let* ((table *claimed-module-names*)
           (old-value (gethash module table))
           (new-value (list lang source)))
      (when old-value
        (unless (equal old-value new-value)
          (warn "~s was claimed for ~a in ~a" module source lang)))
      (setf (gethash module table) new-value))))

(defun lang+source (lang source module base &optional env)
  (setf source (macroexpand source env)) ;Allow a symbol macro as the source.
  (flet ((resolve-source (source)
           (merge-pathnames* (ensure-pathname source :want-pathname t)
                             base)))
    (cond
      ;; We have the source and the language.
      ((and source lang)
       (values (resolve-lang lang)
               (resolve-source source)))
      ;; We have the source, but not the language.
      (source
       (let ((source (resolve-source source)))
         (values (resolve-lang
                  (or (guess-lang+pos source)
                      (required-argument :as)))
                 source)))
      ;; We have the language, but not the source.
      (lang
       (values (resolve-lang lang)
               (resolve-source
                (or (guess-source lang module)
                    (required-argument :from)))))
      ;; We have neither the language nor the source.
      (t (whichever
          (required-argument :as)
          (required-argument :from))))))

(defun imports-with-module-as-function-not-supported (mod bindings values)
  (when (and (typep mod 'function-alias)
             (or bindings values))
    (error* "~
Binding imports (~a) from a module imported as a function (~a) is not currently supported."
            (append bindings values) mod)))

(defun resolve-import-spec
    (&key lang source bindings values module (base (base)) env prefix)
  (check-type base absolute-pathname)
  (check-type prefix string-designator)
  (mvlet* ((lang source (lang+source lang source module base env))
           (bindings values (bindings+values bindings values
                                             :lang lang
                                             :source source
                                             :prefix prefix)))
    (imports-with-module-as-function-not-supported module bindings values)
    (values lang source bindings values)))

(defmacro import (module &body (&key
                                  ((:as lang))
                                  ((:from source))
                                  ((:binding bindings))
                                  values
                                  prefix
                                  function-wrapper)
                  &environment env)
  "Syntax for importing from modules.

Note you can do (import #'foo ...), and the module will be bound as a function."
  ;; Ensure we have both the lang and the source.
  (receive (lang source bindings values)
      (resolve-import-spec :lang lang
                           :source source
                           :module module
                           :bindings bindings
                           :values values
                           :prefix prefix
                           :env env)
    ;; Warn if MODULE is already in use with an incompatible language
    ;; and source.
    (claim-module-name module lang source)
    (let ((lazy? (null values)))
      `(progn
         ;; Importing modules non-lazily has a speed advantage when
         ;; there are no bindings, but it also makes maintenance more
         ;; complex to have two import forms. For now, just use lazy
         ;; imports; maybe re-enable eager loading later.

         ;; Also: while it happens to be the case that Serapeum's `def'
         ;; expands into a symbol macro definition, so switching between
         ;; lazy and eager imports works, it is *conceptually* weird
         ;; that we can just go ahead and redefine a global lexical as a
         ;; symbol macro.
         (import-module/lazy ,module :as ,lang :from ,source)
         #+ () (,(if lazy? 'import-module/lazy 'import-module)
                ,module :as ,lang :from ,(merge-pathnames* source (base)))
         ;; We push the check down into a separate macro so we can
         ;; inspect overall macroexpansion without side effects.
         (check-static-bindings-now ,lang ,source ,(append bindings values))
         (macrolet ((function-wrapper (fn)
                      ,(if function-wrapper
                           `(list ',function-wrapper fn)
                           'fn)))
           (import-bindings ,module ,@bindings)
           (import-values ,module ,@values))
         (import-task ,module :as ,lang :from ,source :values ,values :lazy ,lazy?)
         ;; Strictly for debuggability.
         (values ',module ',(append bindings values))))))

(defun bindings+values (bindings values &key lang source prefix)
  ;; Avoid redundant calls to module-static-bindings.
  (with-static-exports-cache ()
    (flet ((expand (spec)
             (~> (expand-binding-spec spec lang source)
                 canonicalize-bindings
                 (apply-prefix prefix))))
      (let ((bindings (expand bindings))
            (values (expand values)))
        (if *always-import-values*
            (values nil (append bindings values))
            (values bindings values))))))

(defmacro check-static-bindings-now (lang source bindings)
  "Wrapper around check-static-bindings to force evaluation at compile time.
Can't use eval-when because it has to work for local bindings."
  (check-static-bindings lang source bindings))

(defcondition binding-export-mismatch (overlord-error)
  ((bindings :initarg :bindings :type list)
   (exports :initarg :exports :type list))
  (:report (lambda (c s)
             (with-slots (bindings exports) c
               (format s "Requested bindings do not match exports.~%Bindings: ~s~%Exports: ~s"
                       bindings exports)))))

(defun check-static-bindings (lang source bindings)
  "Check that BINDINGS is free of duplicates. Also, using
`module-static-exports', check that all of the symbols being bound are
actually exported by the module specified by LANG and SOURCE."
  (ensure-lang-exists lang)
  (when bindings
    (when (relative-pathname-p source)
      (setf source (merge-pathnames* source (base))))
    (restart-case
        (progn
          (redo (module-spec lang source))
          (let ((exports
                  (module-static-exports lang source))
                (bindings
                  (mapcar (op (import-keyword (first _)))
                          (canonicalize-bindings bindings))))
            ;; Check for duplicated bindings.
            (unless (set-equal bindings (remove-duplicates bindings))
              (error* "Duplicated bindings."))
            ;; Make sure the bindings match the exports.
            (unless (subsetp bindings exports :test #'string=)
              (error 'binding-export-mismatch
                     :bindings bindings
                     :exports exports))))
      (recompile-object-file ()
        :report "Recompile the object file."
        (let ((object-file (faslize lang source))
              (target (module-spec lang source)))
          (delete-file-if-exists object-file)
          (redo target)
          (check-static-bindings lang source bindings))))))

(defmacro import-module (module &key as from)
  (let ((req-form `(require-as ',as ,from)))
    (etypecase-of import-alias module
      (var-alias
       `(overlord/shadows:def ,module ,req-form))
      (function-alias
       `(overlord/shadows:defalias ,(second module) ,req-form))
      (macro-alias
       (error 'module-as-macro :name (second module))))))

(defmacro import-module/lazy (module &key as from)
  ;; TODO
  (let ((target (module-spec as from))
        (*parents* (or *parents* (list (root-target)))))
    (record-prereq target))
  (let ((lazy-load `(load-module/lazy ',as ,from)))
    (etypecase-of import-alias module
      (var-alias
       `(overlord/shadows:define-symbol-macro ,module ,lazy-load))
      (function-alias
       (let ((fn (second module)))
         `(progn
            (declaim (notinline ,fn))
            (overlord/shadows:defun ,fn (&rest args)
              (apply ,lazy-load args)))))
      (macro-alias
       (error 'module-as-macro :name (second module))))))

(defmacro import-task (module &key as from values lazy)
  (declare (ignorable lazy))
  (let ((task-name
          (etypecase-of import-alias module
            (var-alias module)
            ((or function-alias macro-alias)
             (second module)))))
    `(deftask ,task-name
       (progn
         (require-as ',as ,from)
         ;; Put this back if we ever allow non-lazy loaded modules again.
         #+ () ,(let ((req-form `(require-as ',as ,from)))
                  (if lazy
                      req-form
                      `(setf ,module ,req-form)))
         (update-value-bindings ,module ,@values)))))

(defmacro update-value-bindings (module &body values)
  `(progn
     ,@(collecting
         (dolist (clause values)
           (receive (import alias ref) (import+alias+ref clause module)
             (declare (ignore import))
             (collect
                 (etypecase-of import-alias alias
                   (var-alias `(setf ,alias ,ref))
                   (function-alias
                    `(setf (symbol-function ',(second alias)) ,ref))
                   (macro-alias
                    ;; TODO Why not? It's just setf of macro-function.
                    (error 'macro-as-value :name (second alias))))))))))

(defmacro import-bindings (module &body bindings &environment env)
  `(progn
     ,@(mapcar (op (import-binding _ module env))
               bindings)))

(defmacro import-values (module &body values)
  `(progn
     ,@(mapcar (op (import-value _ module)) values)))

(defun canonicalize-binding (clause)
  (assure canonical-binding
    (if (typep clause 'canonical-binding)
        clause
        (etypecase-of binding-designator clause
          (var-spec
           (list (make-keyword clause) clause))
          (function-alias
           (list (make-keyword (second clause)) clause))
          (macro-alias
           (list (make-keyword (second clause)) clause))
          ((tuple symbol :as import-alias)
           (destructuring-bind (import &key ((:as alias))) clause
             (list (make-keyword import) alias)))))))

(defun canonicalize-bindings (clauses)
  (mapcar #'canonicalize-binding clauses))

(defun apply-prefix (clauses prefix)
  (if (null prefix) clauses
      (flet ((prefix (suffix) (symbolicate prefix suffix)))
        (loop for (import alias) in clauses
              collect (list import
                            (etypecase-of import-alias alias
                              (var-alias (prefix alias))
                              (function-alias `(function ,(prefix (second alias))))
                              (macro-alias `(macro-function ,(prefix (second alias))))))))))

(defun import-binding (clause module &optional env)
  (receive (import alias ref) (import+alias+ref clause module)
    (declare (ignore import))
    (etypecase-of import-alias alias
      (var-alias
       `(overlord/shadows:define-symbol-macro ,alias ,ref))
      (function-alias
       (let ((alias (second alias))
             (exp (macroexpand-1 `(function-wrapper ,ref) env)))
         ;; We used to use dynamic-extent declarations here, but Core
         ;; Lisp binds `args' as a symbol macro, and SBCL, stickler
         ;; that it is, objects to dynamic-extent declarations for
         ;; symbol macros.
         (if (equal exp ref)
             `(progn
                (declaim (notinline ,alias))
                (overlord/shadows:defun ,alias (&rest args)
                  (apply ,ref args)))
             `(progn
                (overlord/shadows:defalias ,alias
                  (function-wrapper
                   (lambda (&rest args)
                    (apply ,ref args))))))))
      (macro-alias
       (let ((alias (second alias)))
         (with-gensyms (whole body env)
           `(overlord/shadows:defmacro ,alias (&whole ,whole &body ,body &environment ,env)
              (declare (ignore ,body))
              (funcall ,ref ,whole ,env))))))))

(defun import-value (clause module)
  (receive (import alias ref) (import+alias+ref clause module)
    (declare (ignore import))
    (etypecase-of import-alias alias
      (var-alias
       `(overlord/shadows:def ,alias ,ref))
      (function-alias
       (let ((alias (second alias)))
         `(overlord/shadows:defalias ,alias
            (function-wrapper ,ref))))
      (macro-alias
       (error 'macro-as-value :name (second alias))))))

(defun import+alias+ref (clause module)
  (destructuring-bind (import alias) (canonicalize-binding clause)
    (let* ((key (import-keyword import))
           (ref `(module-ref* ,module ',key)))
      (values import alias ref))))

(defun import-keyword (import)
  (if (symbolp import)
      (make-keyword import)
      (make-keyword (second import))))

(defmacro import/local (mod &body (&key from as binding values prefix)
                        &environment env)
  (receive (lang source bindings values)
      (resolve-import-spec :lang as
                           :source from
                           :prefix prefix
                           :module mod
                           :bindings binding
                           :values values
                           :env env)
    ;; TODO If we knew that no macros were being imported, we could
    ;; give the module a local binding and not have to look it up
    ;; every time.
    `(progn
       (import-module/lazy ,mod :as ,lang :from ,source)
       (check-static-bindings-now ,lang ,source ,(append bindings values))
       (import-bindings ,mod ,@bindings)
       (import-values ,mod ,@values))))

(defmacro with-imports ((mod &key from as binding values prefix) &body body)
  "A version of `import' with local scope."
  `(local*
     (import/local ,mod
       :from ,from
       :as ,as
       :binding ,binding
       :values ,values
       :prefix ,prefix)
     (progn ,@body)))

(defmacro import-as-package (package-name
                             &body body
                             &key ((:as lang))
                                  ((:from source) (guess-source lang package-name))
                                  ((:binding bindings))
                                  values
                                  prefix
                             &allow-other-keys
                             &environment env)
  "Like `import', but instead of creating bindings in the current
package, create a new package named PACKAGE-NAME which exports all of
the symbols bound in the body of the import form."
  (receive (lang source bindings values)
      (resolve-import-spec :lang lang
                           :source source
                           :bindings bindings
                           :values values
                           :module 'package-module
                           :prefix prefix
                           :env env)
    (declare (ignore source lang))
    (let ((body (list* :binding bindings
                       :values values
                       (remove-from-plist body :prefix :binding :values))))
      `(progn
         (import->defpackage ,package-name ,@body)
         ;; The helper macro must be expanded after package-name has
         ;; been defined.
         (import-as-package-aux ,package-name ,@body)))))

(defmacro import->defpackage (package-name
                              &body (&rest body
                                     &key
                                       ((:binding bindings))
                                       values
                                       &allow-other-keys))
  (declare (ignore body))
  `(defpackage ,package-name
     (:use)
     (:export ,@(nub (loop for (nil alias) in (append bindings values)
                           collect (make-keyword
                                    (etypecase-of import-alias alias
                                      (var-alias alias)
                                      (function-alias (second alias))
                                      (macro-alias (second alias)))))))))

(defmacro import-as-package-aux (package-name &body
                                                (&rest body
                                                 &key ((:binding bindings))
                                                      values
                                                      &allow-other-keys))
  (let ((p (assure package (find-package package-name))))
    (labels ((intern* (sym)
               (intern (string sym) p))
             (intern-spec (spec)
               (loop for (key alias) in spec
                     collect `(,key :as ,(etypecase-of import-alias alias
                                           (var-alias (intern* alias))
                                           (function-alias
                                            (let ((alias (second alias)))
                                              `(function ,(intern* alias))))
                                           (macro-alias
                                            (let ((alias (second alias)))
                                              `(macro-function ,(intern* alias)))))))))
      (let ((module-binding (symbolicate '%module-for-package- (package-name p))))
        `(import ,module-binding
           :binding ,(intern-spec bindings)
           :values  ,(intern-spec values)
           ,@body)))))

(defun subpackage-full-name (child-package-name)
  (let* ((parent-package *package*)
         (parent-package-name (package-name parent-package))
         (child-package-name (string child-package-name))
         (full-package-name
           (fmt "~a.~a" parent-package-name child-package-name)))
    (make-keyword full-package-name)))

(defmacro import-as-subpackage (child-package-name
                                &body body
                                &key
                                  &allow-other-keys)
  `(import-as-package ,(subpackage-full-name child-package-name)
     ,@body))
