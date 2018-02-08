;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(require 'seq)

(defconst TIME-ZERO '(0 0 0 0))

(defun stopwatch-construct ()
  (stopwatch--construct))

(defun stopwatch--construct (&optional laps lap-at-last-stop last-start-time)
  (list (cons 'finished-laps (or laps ()))
        (cons 'lap-time-at-last-stop (or lap-at-last-stop TIME-ZERO))
        (cons 'last-start-time last-start-time)))

(defun stopwatch-start (stopwatch)
  (let ((start-time (current-time))
        (laps (stopwatch--get-finished-laps stopwatch))
        (lap-at-last-stop (stopwatch--get-lap-time-at-last-stop stopwatch)))
  (stopwatch--construct laps lap-at-last-stop start-time)))

(defun stopwatch-stop (stopwatch)
  (if (not (stopwatch--running-p stopwatch))
      stopwatch
    (let* ((end-time (current-time))
           (lap-at-last-stop (stopwatch--calculate-last-lap stopwatch end-time))
           (laps (stopwatch--get-finished-laps stopwatch)))
      (stopwatch--construct laps lap-at-last-stop))))

(defun stopwatch-end-lap (stopwatch)
  (if (not (stopwatch--running-p stopwatch))
      stopwatch
    (let* ((end-time (current-time))
           (last-lap-time (stopwatch--calculate-last-lap stopwatch end-time))
           (laps (stopwatch--get-finished-laps stopwatch)))
      (stopwatch--construct (cons last-lap-time laps) TIME-ZERO end-time))))

(defun stopwatch--calculate-last-lap (stopwatch end-time)
  (let* ((last-start (stopwatch--get-last-start-time stopwatch))
         (time-since-last-start (time-subtract end-time last-start))
         (lap-time-at-last-stop (stopwatch--get-lap-time-at-last-stop stopwatch)))
    (time-add lap-time-at-last-stop time-since-last-start)))

(defun stopwatch-get-all-laps (stopwatch)
  (let* ((stopped-watch (stopwatch-stop stopwatch))
         (finished-laps (stopwatch--get-finished-laps stopped-watch))
         (last-lap (stopwatch--get-lap-time-at-last-stop stopped-watch)))
    (cons last-lap finished-laps)))

(defun stopwatch-get-total-seconds (stopwatch)
  (let* ((stopped-watch (stopwatch-stop stopwatch))
         (total-time (stopwatch-total-time stopped-watch)))
    (stopwatch-round-down-to-seconds total-time)))

(defun stopwatch-total-time (stopwatch)
  (let* ((stopped-watch (stopwatch-stop stopwatch))
         (finished-laps (stopwatch--get-finished-laps stopped-watch))
         (total-of-finished-laps (seq-reduce 'time-add finished-laps TIME-ZERO))
         (last-lap (stopwatch--get-lap-time-at-last-stop stopped-watch)))
    (time-add last-lap total-of-finished-laps)))

(defun stopwatch-fraction-to-next-second (stopwatch)
  (let* ((lap-at-last-stop (stopwatch--get-lap-time-at-last-stop stopwatch))
         (fraction-from-last-second
          (time-to-seconds (stopwatch-zero-seconds-parts lap-at-last-stop))))
    (- 1 fraction-from-last-second)))

(defun stopwatch-fraction-to-next-tenth (stopwatch)
  (let* ((lap-at-last-stop (stopwatch--get-lap-time-at-last-stop stopwatch))
         (fraction-from-last-tenth
          (time-to-seconds (stopwatch-mod-tenths-of-a-second lap-at-last-stop))))
    (- 0.1 fraction-from-last-tenth)))

(defalias 'stopwatch--running-p 'stopwatch--get-last-start-time)

(defun stopwatch--get-finished-laps (stopwatch)
  (cdr (assoc 'finished-laps stopwatch)))

(defun stopwatch--get-last-start-time (stopwatch)
  (cdr (assoc 'last-start-time stopwatch)))

(defun stopwatch--set-last-start-time (stopwatch time)
  (setf (cdr (assoc 'last-start-time stopwatch)) time))

(defun stopwatch--get-lap-time-at-last-stop (stopwatch)
  (cdr (assoc 'lap-time-at-last-stop stopwatch)))

;;; stopwatch-model.el ends here
