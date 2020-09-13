(uiop:define-package :overlord/cmd
  (:use :cl :alexandria :serapeum :cmd/hooks)
  (:use-reexport :cmd)
  (:import-from :overlord/message :*message-stream* :message)
  (:import-from :overlord/build-env :register-proc*))
(cl:in-package :overlord/cmd)

(add-hook '*message-hook* #'message)

(add-hook '*proc-hook* #'register-proc*)
