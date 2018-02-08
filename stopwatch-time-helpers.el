;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(defun stopwatch-zero-seconds-parts (time)
  (list 0 0 (stopwatch-microsec time) (stopwatch-picosec time)))

(defun stopwatch-mod-tenths-of-a-second (time)
  (list 0 0 (% (stopwatch-microsec time) 100000) (stopwatch-picosec time)))

(defun stopwatch-round-down-to-seconds (time)
  (+ (* (stopwatch-sec-high time) (lsh 1 16)) (stopwatch-sec-low time)))

(defun stopwatch-tenths-of-a-second (time)
  (/ (stopwatch-microsec time) 100000))

(defun stopwatch-sec-high (time)
  (nth 0 time))

(defun stopwatch-sec-low (time)
  (nth 1 time))

(defun stopwatch-microsec (time)
  (nth 2 time))

(defun stopwatch-picosec (time)
  (nth 3 time))

;;; stopwatch-time-helpers.el ends here
