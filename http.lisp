;;; http.lisp -- update a downloaded file
(defpackage #:overlord/http
  (:use #:cl #:alexandria #:serapeum
    #:overlord/types
    #:overlord/global-state)
  (:import-from #:cl-strftime #:format-time)
  (:import-from #:drakma #:http-request)
  (:import-from #:overlord/base #:ensure-absolute)
  (:import-from #:serapeum #:make-octet-vector)
  (:export #:update-file-from-url
           #:ensure-file-from-url
           #:go-offline
           #:go-online
           #:*connection-timeout*
           #:online-only))

;;; This is an (optional) facility for updating a file from a URL.
;;; When you call (update-file-from-url FILE URL), we make a request
;;; to URL with an If-Modified-Since header derived from FILE's mtime.
;;; If the resource at the URL is newer than FILE, then we download
;;; the resource and replace FILE with it.

(in-package #:overlord/http)

(define-global-state *offline* nil
  "Are we offline?")
(declaim (type boolean *offline*))

(defparameter *connection-timeout* 5)

(defun call/online (thunk &key error)
  (if *offline*
      (if error
          (restart-case
              (error* "Can't do that offline.")
            (go-online ()
              :report "Go back online."
              (go-online)
              (call/online thunk)))
          nil)
      (restart-case
          (funcall thunk)
        (go-offline ()
          :report "Continue as if offline."
          (go-offline)))))

(defmacro online-only ((&key error) &body body)
  (with-thunk (body)
    `(call/online ,body :error ,error)))

(defun http-request/binary (url &rest args)
  (apply #'http-request url
         :force-binary t
         :connection-timeout *connection-timeout*
         args))

(defun update-file-from-url (file url)
  (lret ((file
          (ensure-absolute file)))
    (if (not (uiop:file-exists-p file))
        (if *offline*
            (error* "Offline: cannot retrieve ~a" file)
            (multiple-value-bind (body status) (http-request/binary url)
              (when (= status 200)
                (write-byte-vector-into-file body file))))
        (online-only ()
                     (let ((fwd (file-write-date file)))
                       (multiple-value-bind (body status)
                           (http-request/binary
                            url
                            :additional-headers
                            `((:if-modified-since . ,(format-mtime fwd))))
                         (when (= status 200)
                           (unless (vector= body (read-file-into-byte-vector file))
                             (write-byte-vector-into-file body file :if-exists :supersede)))))))))

(defun ensure-file-from-url (file url)
  "Unlike `update-file-from-url' this does not preserve URL's
timestamp."
  (lret ((file (ensure-absolute file)))
    (unless (uiop:file-exists-p file)
      (online-only ()
                   (multiple-value-bind (body status) (http-request/binary url)
                     (if (= status 200)
                         (write-byte-vector-into-file body file)
                         (error* "Could not fetch ~a: code ~a" url status)))))))

(defun go-offline ()
  (setf *offline* t))

(defun go-online ()
  (setf *offline* nil))

(defun format-mtime (mtime)
  (format-time nil :http mtime))

