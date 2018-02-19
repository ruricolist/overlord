(cl:defpackage :overlord/cmd
  (:use :cl :alexandria :serapeum)
  (:shadowing-import-from :serapeum :collecting :summing :in)
  (:import-from :overlord/specials :use-threads-p)
  (:import-from :overlord/base :base :current-dir!)
  (:import-from :overlord/types :list-of :plist :error*)
  (:import-from :overlord/message :*message-stream*)
  (:import-from :uiop :os-windows-p)
  (:export :cmd))
(cl:in-package :overlord/cmd)

(defun parse-cmd-args (args)
  (let ((tokens (queue))
        (plist (queue)))
    (dolist (arg args)
      (typecase arg
        (string
         (qconc tokens (tokens arg)))
        (pathname
         (enq (stringify-pathname arg) tokens))
        (plist
         (qappend plist arg))
        ((list-of string)
         (qappend tokens arg))
        (t (error "Can't use ~a as a cmd argument." arg))))
    (values (qlist tokens)
            (qlist plist))))

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
  (if (not (use-threads-p)) tokens
      (destructuring-bind (command . args) tokens
        (let ((dir (stringify-pathname (current-dir!))))
          (if (not (os-windows-p))
              `("/bin/sh"
                "-c"
                ;; Use Bernstein chaining; change to the directory in
                ;; $1, shift, and exec the rest of the argument array.
                ;; (If only there were a standard tool to do this, along
                ;; the lines of chroot or su, so we wouldn't have to
                ;; spin up a shell.)
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
                "&" ,command ,@args))))))

(defun cmd (&rest args)
  (multiple-value-bind (tokens plist) (parse-cmd-args args)
    (multiple-value-call #'uiop:run-program
      (wrap-with-current-dir tokens)
      (values-list plist)
      :output t
      :error-output *message-stream*)))
