(defpackage :overlord/message
  (:use :cl :alexandria :serapeum
    :overlord/global-state)
  (:import-from :overlord/types :error*)
  (:import-from :cl-ppcre :all-matches-as-strings)
  (:shadowing-import-from :cl-ppcre :scan)
  (:export
   :overlord-message
   :message
   :*message-stream*))
(in-package :overlord/message)

(define-global-state *message-stream*
    (make-synonym-stream '*error-output*)
  "The stream printed to by the default message handler.")

(defun message (control &rest args)
  (let* ((stream *message-stream*)
         (control
           (if (and (stringp control)
                    (string$= "." control))
               (string-right-trim "." control)
               control)))
    (prog1 (format stream "~&[overlord] ~?~%" control args)
      (force-output stream))))

(define-compiler-macro message (&whole call format-control &rest format-arguments)
  (if (not (stringp format-control)) call
      (progn
        (sanity-check-message-args format-control format-arguments)
        (let ((format-control (string-right-trim "." format-control)))
          `(message (formatter ,format-control) ,@format-arguments)))))

(defun sanity-check-message-args (format-control format-arguments)
  "Do some basic sanity-checking with format-control and format-arguments."
  (when (stringp format-control)
    (when-let (required (guess-arg-count format-control))
      (let ((provided (length format-arguments)))
        (unless (= required provided)
          (error* "Message format string requires ~d argument~:p, but ~d ~:*~[were~;was~:;were~] provided."
                  required
                  provided))))))

(defun guess-arg-count (format-string)
  "When possible, Guess the number of arguments required by FORMAT-STRING."
  (let ((directives (extract-directives format-string)))
    (when (every (op (scan "~[a-zA-Z]" _)) directives)
      (length directives))))

(defun extract-directives (format-string)
  (all-matches-as-strings "(~.)" format-string))
