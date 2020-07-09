(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum)
  (:shadowing-import-from :serapeum :collecting :summing :in)
  (:import-from :overlord/base :base :current-dir! :with-current-dir)
  (:import-from :overlord/types :list-of :plist :error*)
  (:import-from :overlord/message :*message-stream* :message)
  (:import-from :uiop
                :os-unix-p
                :native-namestring
                :native-namestring
                :os-windows-p :file-exists-p :getenv
                :pathname-directory-pathname)
  (:import-from :trivia :match)
  (:import-from :overlord/build-env :register-proc*)
  (:import-from :shlex)
  (:export
   :cmd :$cmd :cmd?
   :run-program-in-dir
   :run-program-in-dir*
   :with-cmd-dir))
(cl:in-package :overlord/cmd)

(defun can-use-env-c? ()
  (and (os-unix-p)
       (zerop
        (nth-value 2
          (uiop:run-program
           `("env" "-C"
                   ,(native-namestring
                     (user-homedir-pathname))
                   "pwd")
           :ignore-error-status t
           :output nil
           :error-output nil)))))

(defparameter *can-use-env-c*
  (can-use-env-c?))

(defun update-can-use-env-c ()
  (setf *can-use-env-c* (can-use-env-c?)))

(uiop:register-image-restore-hook 'update-can-use-env-c)

(defparameter *keyword-abbrevs*
  ;; >| would be |>\|| in Lisp syntax, not worth it.
  ;; >? for Fish-style noclobber?
  '((:in :directory _)
    (:< :input _)
    ((:> :1>) :output _)
    (:>> :if-output-exists :append :output _)
    (:2> :error-output _)
    (:2>> :if-error-output-exists :append :error-output _)
    ((:&> :>&)
     :output _ :error-output _)
    ((:&>> :>>&)
     :if-error-output-exists :append
     :if-output-exists :append
     :error-output _
     :output _)))

(defun expand-keyword-abbrevs (args)
  (collecting
    (doplist (k v args)
      (if-let (match (assoc k *keyword-abbrevs*
                            :test #'member
                            :key #'ensure-list))
        (apply #'collect (substitute v '_ (rest match)))
        (collect k v)))))

(defmacro with-cmd-dir (dir &body body)
  `(with-current-dir ((pathname ,dir))
     ,@body))

(defmacro define-cmd-variant (name lambda-list &body body)
  (let ((docstring (and (stringp (car body)) (pop body))))
    `(progn
       (defun ,name ,lambda-list
         ,@(unsplice docstring)
         ,@body)
       (define-compiler-macro ,name (cmd &rest args)
         `(locally (declare (notinline ,',name))
            (,',name ,@(simplify-cmd-args (cons cmd args))))))))

(define-cmd-variant $cmd (cmd &rest args)
  "Return the results of CMD as a string, stripping any trailing
newlines, like $(cmd) would in a shell."
  (chomp
   (with-output-to-string (s)
     (apply #'cmd
            cmd
            :output s
            args))))

(define-cmd-variant cmd? (cmd &rest args)
  (let ((exit-code
          (apply #'cmd
                 cmd
                 :ignore-error-status t
                 args)))
    (if (zerop exit-code) t
        (values nil exit-code))))

(define-cmd-variant cmd (cmd &rest args)
  "Run a program.

CMD should be a string naming a program. This command will be run with
its current directory set to the value of `overlord:current-dir!' in a
thread-safe manner.

A list of strings or pathnames is added to the list of arguments.

A string in ARGS is split into a list of tokens using shell-style
tokenization rules. (To protect a string with spaces, either add
quotation marks, or enclose it in a singleton list.)

A pathname in ARGS is translated to a native namestring and passed as
an argument to the command. The native namestring is not permitted to
start with a dash.

A property list is treated as a list of keyword arguments to
`uiop:run-program'. Certain keywords are treated as abbreviations:
e.g. `:>' is an abbreviation for `:output'. Abbreviations can be
compound: e.g. `:>>' affects both `:output' and `:if-exists'.

By default, standard output is sent to `*standard-output*', and error
output is sent to `*message-stream*'.

On Windows, the .exe suffix may be omitted from the name of the
executable."
  (receive (tokens args) (parse-cmd-args (cons cmd args))
    (setf tokens (cons (exe-string (car tokens)) (cdr tokens)))
    (setf args (expand-keyword-abbrevs args))
    (message "$ ~{~a~^ ~}" (mapcar #'shlex:quote tokens))
    (multiple-value-call #'run-program-in-dir*
      tokens
      (values-list args)
      :output *standard-output*
      :error-output *message-stream*)))

(defun simplify-cmd-args (args)
  (nlet rec ((args-in args)
             (args-out '()))
    (match args-in
      ((list)
       (reverse args-out))
      ((list (and _ (type keyword)))
       (error "Dangling keyword argument to cmd."))
      ((list* (and k (type keyword)) v rest)
       (rec rest
            (cons (if (constantp v)
                      `'(,k ,v)
                      `(list ,k ,v))
                  args-out)))
      ((list* (and s (type string)) xs)
       (rec xs
            (cons `(quote (,@(split-cmd s)))
                  args-out)))
      ((list* (and p (type pathname)) xs)
       (rec xs
            (cons (stringify-pathname p)
                  args-out)))
      ((list* x xs)
       (rec xs (cons x args-out))))))

(defun run-program-in-dir* (tokens &rest args)
  "Run a program (with uiop:run-program) in the current base directory."
  (let ((dir (stringify-pathname
              (or (getf args :directory)
                  (current-dir!)))))
    (apply #'run-program-in-dir dir tokens
           (remove-from-plist args :directory))))

(defun run-program-in-dir (dir tokens
                           &rest args
                           &key ignore-error-status
                                (output *standard-output*)
                                (error-output *message-stream*)
                           &allow-other-keys)
  "Run a program (with `uiop:run-program`) with DIR as its working directory."
  (handler-bind ((serious-condition
                   (lambda (e) (declare (ignore e))
                     (finish-output output)
                     (finish-output error-output))))
    (let* ((cmd
             ;; NB The :directory argument to launch-program may end up
             ;; calling `chdir', which is unacceptable.
             (wrap-with-dir dir tokens))
           (proc
             (multiple-value-call #'uiop:launch-program cmd
               (values-list args))))
      (register-proc* proc)
      (let ((abnormal? t))
        (unwind-protect
             (multiple-value-prog1
                 (let ((status (uiop:wait-process proc)))
                   (cond ((zerop status)
                          status)
                         (ignore-error-status
                          status)
                         (t
                          (cerror "IGNORE-ERROR-STATUS"
                                  'uiop:subprocess-error
                                  :command tokens
                                  :code status
                                  :process proc)
                          status)))
               (setf abnormal? nil))
          (when abnormal?
            (uiop:terminate-process proc)))))))

(defun parse-cmd-args (args)
  (nlet rec ((args args)
             (tokens '())
             (plist '()))
    (match args
      ((list)
       (values (nreverse tokens)
               (nreverse plist)))
      ((list* (and arg (type integer)) args)
       (rec (cons (princ-to-string arg)
                  args)
            tokens
            plist))
      ((list* (and arg (type string)) args)
       (rec args
            (nreconc (split-cmd arg) tokens)
            plist))
      ((list* (and arg (type pathname)) args)
       (rec args
            (cons (stringify-pathname arg) tokens)
            plist))
      ((list* (and arg (type plist)) args)
       (rec args
            tokens
            (revappend arg plist)))
      ((list* (and arg (type sequence)) args)
       (rec args
            (nreconc
             (collecting
               (do-each (token arg)
                 (collect (etypecase token
                            (string token)
                            (pathname (stringify-pathname token))))))
             tokens)
            plist))
      ((list (and _ (type keyword)))
       (error "Dangling keyword argument to cmd."))
      ((list* (and k (type keyword)) v args)
       (rec args
            tokens
            (nreconc (list k v) plist)))
      ((list* arg _)
       (error "Can't use ~a as a cmd argument." arg)))))

(defun wrap-with-dir (dir tokens)
  "Wrap TOKENS with the necessary code to run the process in DIR.

The OS-level current directory is per-process, not per thread. Using
`chdir' could lead to race conditions. Instead, we arrange for the new
process to change its own working directory."
  (destructuring-bind (command . args) tokens
    (cond (*can-use-env-c*
           ;; When there is a recent version of GNU env installed, the
           ;; -C switch lets us do Bernstein chaining without spinning
           ;; up a shell.
           `("env" "-C" ,dir ,command ,@args))
          ((not (os-windows-p))
           `("/bin/sh"
             "-c"
             ;; Use Bernstein chaining; change to the directory in $1,
             ;; shift, and exec the rest of the argument array.
             "set -e; CDPATH='' cd -P \"$1\"; shift; exec \"$@\""
             ;; Terminate processing of shell options; everything
             ;; after this is passed through.
             "--"
             ,dir
             ,command
             ,@args))
          ;; This looks weird, but it actually works, because the
          ;; Windows API to start a process is called with a
          ;; string rather than an array. We could just as well
          ;; pass a string, but then we would have to do our own
          ;; escaping.
          (t
           `("cmd"
             "/c"
             ;; Note that /d is required for cd to work across drives.
             "cd" "/d" ,dir
             ;; Ampersand is the command separator.
             "&" ,command ,@args)))))

(defun stringify-pathname (arg)
  (unless (pathnamep arg)
    (return-from stringify-pathname arg))
  (lret ((string
          (let ((string (native-namestring arg)))
            (if (and (os-windows-p)
                     (featurep :ccl)
                     (position #\/ string))
                ;; Work around a CCL bug; issue #103 on GitHub.
                (substitute #\\ #\/ string)
                string))))
    (when (string^= "-" string)
      ;; Should we ignore the unsafe file names if `--' or
      ;; `---' is already present in the list of tokens?
      (cerror "Allow the unsafe file name"
              "File name ~a begins with a dash"
              string))))

(defun exe-string (p)
  (stringify-pathname (exe p)))

(defun split-cmd (cmd)
  ;; NB UIOP expects simple-strings for arguments.
  (mapcar (op (coerce _ '(simple-array character (*))))
          (shlex:split cmd :whitespace-split nil
                           :punctuation-chars t)))
