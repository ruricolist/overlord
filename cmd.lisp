(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum)
  (:shadowing-import-from :serapeum :collecting :summing :in)
  (:import-from :overlord/base :base :current-dir!)
  (:import-from :overlord/types :list-of :plist :error*)
  (:import-from :overlord/message :*message-stream* :message)
  (:import-from :uiop :os-windows-p :file-exists-p :getenv
    :pathname-directory-pathname)
  (:import-from :trivia :match)
  (:export
   :cmd :$cmd
   :run-program-in-dir
   :run-program-in-dir*
   :resolve-executable))
(cl:in-package :overlord/cmd)

(defun $cmd (cmd &rest args)
  "Return the results of CMD as a string, stripping any trailing
newlines, like $(cmd) would in a shell."
  (apply #'cmd
         cmd
         :output '(:string :stripped t)
         args))

(defun cmd (cmd &rest args)
  "Run a program.

CMD should be a string naming a program. This command will be run with
its current directory set to the value of `overlord:current-dir!' in a
thread-safe manner.

A list of strings or pathnames is added to the list of arguments.

A string in ARGS is split into a list of space-separated tokens. (To
protect a string with spaces, enclose it in a singleton list.)

A pathname in ARGS is translated to a native namestring and passed as
an argument to the command. The native namestring is not permitted to
start with a dash.

A property list is treated as a list of arguments to `uiop:run-program'.

By default, standard output is sent to `*standard-output*', and error
output is sent to `*message-stream*`.

On Windows, the .exe suffix may be omitted from the name of the
executable."
  (receive (tokens args) (parse-cmd-args (cons cmd args))
    (setf tokens (cons (exe-string (car tokens)) (cdr tokens)))
    (message "$ ~{~a~^ ~}" tokens)
    (multiple-value-call #'run-program-in-dir*
      tokens
      (values-list args)
      :output t
      :error-output *message-stream*)))

(define-compiler-macro cmd (cmd &rest args)
  "At compile time, make sure the keyword arguments are syntactically
valid."
  (nlet rec ((args-in args)
             (args-out '()))
    (match args-in
      ((list)
       `(locally (declare (notinline cmd))
          (cmd ,cmd ,@(reverse args-out))))
      ((list (and _ (type keyword)))
       (error "Dangling keyword argument to cmd."))
      ((list* (and k (type keyword)) v rest)
       (rec rest
            (cons `(list ,k ,v) args-out)))
      ((list* (and s (type string)) xs)
       (rec xs
            (cons `(quote (,@(tokens s)))
                  args-out)))
      ((list* (and p (type pathname)) xs)
       (rec xs
            (cons (stringify-pathname p)
                  args-out)))
      ((list* x xs)
       (rec xs (cons x args-out))))))

(defun run-program-in-dir* (tokens &rest args)
  "Run a program (with uiop:run-program) in the current base directory."
  (let ((dir (stringify-pathname (current-dir!))))
    (apply #'run-program-in-dir dir tokens args)))

(defun run-program-in-dir (dir tokens &rest args)
  "Run a program (with `uiop:run-program`) with DIR as its working directory."
  (let ((tokens (wrap-with-dir dir tokens)))
    (apply #'uiop:run-program tokens args)))

(defun parse-cmd-args (args)
  (nlet rec ((args args)
             (tokens '())
             (plist '()))
    (match args
      ((list)
       (values (nreverse tokens)
               (nreverse plist)))
      ((list* (and arg (type string)) args)
       (rec args
            (nreconc (tokens arg) tokens)
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
    (if (not (os-windows-p))
        `("/bin/sh"
          "-c"
          ;; Use Bernstein chaining; change to the directory in $1,
          ;; shift, and exec the rest of the argument array. (If only
          ;; there were a standard tool to do this, along the lines of
          ;; chroot or su, so we wouldn't have to spin up a shell.
          ;; Hopefully your distro uses something lighter than bash
          ;; for /bin/sh.)
          "set -e; env CDPATH=. cd -P $1; shift; exec \"$@\""
          ;; Terminate processing of shell options; everything
          ;; after this is passed through.
          "--"
          ,dir
          ,command
          ,@args)
        ;; This looks weird, but it actually works, because the
        ;; Windows API to start a process is called with a
        ;; string rather than an array. We could just as well
        ;; pass a string, but then we would have to do our own
        ;; escaping.
        `("cmd"
          "/c"
          "cd" ,dir
          ;; Ampersand is the command separator.
          "&" ,command ,@args))))

(defun stringify-pathname (arg)
  (unless (pathnamep arg)
    (return-from stringify-pathname arg))
  (lret ((string
          (let ((string (uiop:native-namestring arg)))
            (if (and (os-windows-p)
                     (or #+ccl t)
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

(defun exe (p)
  (let* ((p (pathname p))
         (type (pathname-type p)))
    (if (and (os-windows-p)
             (no type))
        (make-pathname :type "exe"
                       :defaults p)
        p)))

(defun exe-string (p)
  (stringify-pathname (exe p)))

(defconst pathsep
  (if (os-windows-p) #\; #\:))

(defun $path ()
  (mapcar #'uiop:pathname-directory-pathname
          ;; Neither Windows nor POSIX supports escaping the separator
          ;; in $PATH.
          (split-sequence pathsep
                          (getenv "PATH")
                          :remove-empty-subseqs t)))

(defun resolve-executable (p)
  (let* ((p (exe p))
         (name (pathname-name p))
         (type (pathname-type p)))
    (loop for dir in ($path)
          for pathname = (make-pathname :name name
                                        :type type
                                        :defaults dir)
          when (file-exists-p pathname)
            do (return pathname))))
