(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum)
  (:shadowing-import-from :serapeum :collecting :summing :in)
  (:import-from :overlord/base :base :current-dir!)
  (:import-from :overlord/types :list-of :plist :error*)
  (:import-from :overlord/message :*message-stream*)
  (:import-from :uiop :os-windows-p)
  (:export :cmd))
(cl:in-package :overlord/cmd)

(defun parse-cmd-args (args)
  (multiple-value-bind (tokens plist)
      (mvfold (lambda (tokens plist arg)
                (typecase arg
                  (string
                   (values (nreconc (tokens arg) tokens)
                           plist))
                  (pathname
                   (values (cons (stringify-pathname arg) tokens)
                           plist))
                  (plist
                   (values tokens
                           (revappend arg plist)))
                  (sequence
                   (values
                    (nreconc
                     (collecting
                       (do-each (token arg)
                         (collect (etypecase token
                                    (string token)
                                    (pathname (stringify-pathname token))))))
                     tokens)
                    plist))
                  (t (error "Can't use ~a as a cmd argument." arg))))
              args '() '())
    (values (nreverse tokens)
            (nreverse plist))))

(defun stringify-pathname (arg)
  (lret ((string (uiop:native-namestring arg)))
    (when (string^= "-" string)
      ;; Should we ignore the unsafe file names if `--' or
      ;; `---' is already present in the list of tokens?
      (cerror "Allow the unsafe file name"
              "File name ~a begins with a dash"
              string))))

(defun wrap-with-current-dir (tokens)
  "Wrap TOKENS with the necessary code to run the process in a new directory.

The OS-level current directory is per-process, not per thread. Using
`chdir' could lead to race conditions. Instead, we arrange for the new
process to change its own working directory."
  (destructuring-bind (command . args) tokens
    (let ((dir (stringify-pathname (current-dir!))))
      (if (not (os-windows-p))
          `("/bin/sh"
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
            "&" ,command ,@args)))))

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
  (multiple-value-bind (tokens plist) (parse-cmd-args (cons cmd args))
    (multiple-value-call #'uiop:run-program
      (wrap-with-current-dir tokens)
      (values-list plist)
      :output t
      :error-output *message-stream*)))
