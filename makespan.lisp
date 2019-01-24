(defpackage :overlord/makespan
  (:use :cl :alexandria :serapeum)
  (:export :minimize-makespan
   :optimal-machine-count))
(in-package :overlord/makespan)

;;; Minimize the makespan of a set of tasks.

;;; Bear in mind that minimizing a makespan is an NP-hard problem.
;;; Fortunately, there is an algorithm, the LPT algorithm, that gives
;;; theoretically good results, and even better results in practice.

;;; Our terminology differs slightly from that usually used in
;;; discussions of scheduling algorithms; what are usually called
;;; jobs, we will call tasks, to avoid confusion with the use of "job"
;;; in the rest of Overlord.

(defstruct machine
  (total-time 0 :type (integer 0 *))
  (task-queue (queue) :type queue))

(defconstructor task
  (task t)
  (time (integer 0 *)))

(defun machine-add-task (machine task)
  (incf (machine-total-time machine)
        (task-time task))
  (enq task (machine-task-queue machine))
  machine)

(defun machine-tasks (machine)
  (qlist (machine-task-queue machine)))

(defun minimize-makespan (machine-count targets build-times)
  "Given MACHINE-COUNT, a sequence of TARGETS, and a sequence of
BUILD-TIMES for each target, distribute TARGETS into batches, at most
one batch per machine (but possibly less), in such a way as to
minimize their makespan -- the time until the last machine is done
with the last task."
  (assert (length= targets build-times))
  (let ((tasks (map 'vector #'task targets build-times)))
    (lpt-schedule machine-count tasks)))

(defun optimal-machine-count (build-times)
  (if (emptyp build-times) 0
      (let ((max (extremum build-times #'>)))
        (if (zerop max) (length build-times)
            (let ((sum (reduce #'+ build-times)))
              (ceiling (/ sum max)))))))

(defun lpt-schedule (machine-count tasks)
  "Implement the Longest Processing Time algorithm.
MACHINE-COUNT should be an integer.

Times should be given as integers."
  (check-type machine-count (integer 1 *))
  (let ((tasks (sort-new tasks #'> :key #'task-time))
        (heap (make-heap :size machine-count
                         :test #'<
                         :key #'machine-total-time)))
    (loop repeat machine-count do
      (heap-insert heap (make-machine)))
    (do-each (task tasks)
      (let* ((first-finished (heap-extract-maximum heap))
             (machine (machine-add-task first-finished task)))
        (heap-insert heap machine)))
    (let* ((machines (reverse (heap-extract-all heap)))
           (batches (map 'list #'machine-tasks machines)))
      (loop for batch in batches
            collect (map 'list #'task-task batch)))))
