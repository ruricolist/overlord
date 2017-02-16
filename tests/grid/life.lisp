#lang overlord/simple-module

(:import grid :from "grid.lisp"
  :binding :all-as-functions)
(:export #'life)

(defun life-count (grid i j)
  (flet ((cnt (i j)
           (if (ref grid i j) 1 0)))
    (+ (cnt (- i 1) (- j 1))
       (cnt (- i 1) j)
       (cnt (- i 1) (+ j 1))
       (cnt i (- j 1))
       (cnt i (+ j 1))
       (cnt (+ i 1) (- j 1))
       (cnt (+ i 1) j)
       (cnt (+ i 1) (+ j 1)))))

(defun life-alive? (grid i j)
  (case (life-count grid i j)
    (3 t)
    (2 (ref grid i j))
    (t nil)))

(defun life-print (grid &optional (separator #\Page))
  (write-string (string separator))
  (each grid
        (lambda (i j v) (declare (ignore i))
          (format t "~:[ ~;*~]" v)
          (when (= j (- (cols grid) 1))
            (terpri)))))

(defun life (grid iterations &optional (separator #\Page))
  (loop repeat iterations
        for grid0 = grid then grid1
        and grid1 = (make (rows grid) (cols grid))
              then grid0
        do (each grid0
                 (lambda (j k v) (declare (ignore v))
                   (let ((a (life-alive? grid0 j k)))
                     (set! grid1 j k a))))
           (life-print grid1 separator)))
