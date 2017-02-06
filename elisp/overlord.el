;;; overlord.el -*- lexical-binding: t -*-

(defvar overlord-lang-hist ())

(defvar overlord-lang nil
  "The Overlord language to use for this buffer.
Must be the name of a CL package.")
(make-variable-buffer-local 'overlord-lang)
(put 'overlord-lang 'safe-local-variable 'stringp)

(defvar overlord-module-name-hist ())

(eval-when-compile
  (cl-defmacro sl-case (&body (&key (sly '(error "Not yet implemented"))
                                    (slime '(error "Not yet implemented"))))
    `(cond ((featurep 'sly)
            ,sly)
           ((featurep 'slime)
            ,slime)
           (t (overlord-give-up)))))

(declare-function sly-read-package-name "sly")
(declare-function slime-read-package-name "slime")

(declare-function sly-to-lisp-filename "sly")
(declare-function slime-to-lisp-filename "slime")

(declare-function sly-eval-with-transcript "sly")
(declare-function slime-eval-with-transcript "slime")

(declare-function sly-eval-describe "sly")
(declare-function slime-eval-describe "slime")

(defun overlord-hash-lang ()
  (save-excursion
    (goto-char (point-min))
    ;; A stricter implementation would only skip comments.
    (when (re-search-forward "^#lang +\\([-/_+a-zA-Z0-9]+\\)" nil t)
      (match-string-no-properties 1))))

(defun overlord-read-lang ()
  (completing-read "Language: "
                   overlord-lang-hist
                   nil nil nil 'overlord-lang-hist))

(defun overlord-lang ()
  (or overlord-lang
      (overlord-hash-lang)
      (overlord-read-lang)))

(defun overlord-read-module-name ()
  (intern
   (completing-read "Module name: "
                    overlord-module-name-hist
                    nil nil nil 'overlord-module-name-hist)))

(defun overlord-give-up ()
  (cl-assert (and (not (featurep 'sly))
                  (not (featurep 'slime))))
  (error "No SLIME, no Sly, I give up."))

(defun overlord-eval (form)
  (sl-case
   :sly (sly-eval-with-transcript form)
   :slime (slime-eval-with-transcript form)))

(defun overlord-lisp-file-name (file)
  (sl-case :sly (sly-to-lisp-filename file)
           :slime (slime-to-lisp-filename file)))

(defun overlord-compile-file (file lang)
  (interactive (list buffer-file-name (overlord-lang)))
  (let ((file (overlord-lisp-file-name file)))
    (overlord-eval
     `(overlord:require-for-emacs ,lang ,file))))

(defun overlord-buffer-file-name ()
  (overlord-lisp-file-name
   (buffer-file-name)))

(defun overlord-switch-repl ()
  (interactive)
  (sl-case
   :sly (call-interactively 'sly-mrepl)
   :slime (call-interactively 'slime-switch-to-output-buffer)))

(defun overlord--import (file lang binding)
  (let ((file (overlord-lisp-file-name file))
        (module-name (overlord-read-module-name)))
    (overlord-eval
     `(cl:progn (overlord:import ,module-name
                              :from ,file
                              :as ,lang
                              :binding ,binding)
                ;; TODO Display a list of imports.
             ,file))))

(defun overlord-import-vars (file lang)
  (interactive (list buffer-file-name (overlord-lang)))
  (overlord--import file lang ':all))

(defun overlord-import-fns (file lang)
  (interactive (list buffer-file-name (overlord-lang)))
  (overlord--import file lang ':all-as-functions))

(defun overlord-import-module (file lang)
  (interactive (list buffer-file-name (overlord-lang)))
  (overlord--import file lang nil))

(defun overlord-expand-module ()
  (interactive)
  (save-some-buffers)
  (let* ((lang (overlord-lang))
         (file (overlord-lisp-file-name buffer-file-name))
         (expr `(overlord:expand-module-for-emacs ,lang ,file)))
    ;; TODO Prettier output (more like `sly-expand-1').
    (sl-case
     :sly (sly-eval-describe
           `(slynk:pprint-eval ,(prin1-to-string expr)))
     :slime (slime-eval-describe
             `(swank::swank-pprint
               (cl:list ,(prin1-to-string expr)))))))

(defun maybe-activate-overlord ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "#lang ")
      (overlord-mode 1))))

;;; It could be prog-mode-hook, but then we would be conflicting with
;;; Racket.
(add-hook 'lisp-mode-hook 'maybe-activate-overlord)

(defvar overlord-mode-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-k") 'overlord-compile-file)
    (define-key m (kbd "C-c C-z") 'overlord-switch-repl)
    (define-key m (kbd "C-c C-i C-v") 'overlord-import-vars)
    (define-key m (kbd "C-c C-i C-f") 'overlord-import-fns)
    (define-key m (kbd "C-c C-i C-m") 'overlord-import-module)
    (define-key m (kbd "C-c C-m") 'overlord-expand-module)
    m))

(define-minor-mode overlord-mode
  "Minor mode for Overlord."
  :lighter " Overlord"
  :keymap overlord-mode-keymap)

(provide 'overlord-mode)
