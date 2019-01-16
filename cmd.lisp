(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum)
  (:shadowing-import-from :serapeum :collecting :summing :in)
  (:import-from :overlord/base :base :current-dir!)
  (:import-from :overlord/types :list-of :plist :error*)
  (:import-from :overlord/message :*message-stream*)
  (:import-from :uiop
    :os-windows-p :os-unix-p
    :run-program
    :native-namestring)
  (:export
   :*command-shell*
   :run-program-in-dir
   :run-program-in-current-dir
   :cmd))
(cl:in-package :overlord/cmd)

;;; Running commands in a specific dir.

(defparameter *cmd-shell*
  "/bin/sh"
  "The POSIX shell to use for running programs in a specific dir.

This is /bin/sh by default; if your system uses a heavy shell as
/bin/sh (like Bash) you might get better performance when launching
lots of processes by changing this to point to a lighter shell (e.g.
/bin/dash).")

(defun wrap-command-with-dir (dir command)
  "Wrap COMMAND with the necessary code to run the process in a new directory.

The OS-level current directory is per-process, not per thread. Using
`chdir' could lead to race conditions. Instead, we arrange for the new
process to change its own working directory."
  (check-type command list)
  (cond ((os-unix-p)
         `(,*cmd-shell*
           "-c"
           ;; Use Bernstein chaining; change to the directory in
           ;; $1, shift, and exec the rest of the argument array.
           ;; (If only there were a standard tool to do this,
           ;; along the lines of chroot or su, so we wouldn't
           ;; have to spin up a shell. Hopefully your distro uses
           ;; something lighter than bash for /bin/sh.)
           "cd $1; shift; exec \"$@\""
           ;; Terminate processing of shell options; everything
           ;; after this is passed through.
           "--"
           ,dir
           ,@command))
        ;; This looks weird, but it actually works, because the
        ;; Windows API to start a process is called with a string
        ;; rather than an array. We could just as well pass a string,
        ;; but then we would have to do our own escaping.
        ((os-windows-p)
         `("cmd"
           "/c"
           "cd" ,dir
           ;; Ampersand is the command separator.
           "&" ,@command))
        (t (error* "This OS does not support ~s"
                   'run-program-in-dir))))

(defun run-program-in-dir (dir command &rest keys &key &allow-other-keys)
  (let ((dir (stringify-pathname dir)))
    (apply #'run-program
           (wrap-command-with-dir dir command)
           keys)))

(defun run-program-in-current-dir (command &rest keys &key &allow-other-keys)
  (apply #'run-program-in-dir
         (current-dir!)
         command
         keys))

;;; The `cmd' DSL.

(defun parse-cmd-args (args)
  (multiple-value-bind (tokens plist)
      (mvfold (lambda (tokens plist arg)
                (typecase arg
                  (string
                   (nreconc (tokens arg) tokens))
                  (pathname
                   (cons (stringify-pathname arg) tokens))
                  (plist
                   (revappend arg plist))
                  (sequence
                   (nreconc (collecting
                              (do-each (token arg)
                                (collect (etypecase token
                                           (string token)
                                           (pathname (stringify-pathname token))))))
                            tokens))
                  (t (error "Can't use ~a as a cmd argument." arg))))
              args '() '())
    (values (nreverse tokens)
            (nreverse plist))))

(defun stringify-pathname (arg)
  (lret ((string (native-namestring arg)))
    (when (string^= "-" string)
      ;; Should we ignore the unsafe file names if `--' or
      ;; `---' is already present in the list of tokens?
      (cerror "Allow the unsafe file name"
              "File name ~a begins with a dash"
              string))))

(defun maybe-strip-carriage-returns (string)
  (if #.(os-windows-p)
      (string-replace-all #.(coerce (list #\Return #\Newline) 'string)
                          string
                          #.(string #\Newline))
      string))

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

A property list is treated as a list of arguments to `uiop:run-program'."
  (multiple-value-bind (command keys) (parse-cmd-args (cons cmd args))
    (multiple-value-call #'run-program-in-dir
      command
      (values-list keys)
      :output t
      :error-output *message-stream*)))
