;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(defun zero-seconds-parts (time)
  (list 0 0 (microsec time) (picosec time)))

(defun mod-tenths-of-a-second (time)
  (list 0 0 (% (microsec time) 100000) (picosec time)))

(defun round-down-to-seconds (time)
  (+ (* (sec-high time) (lsh 1 16)) (sec-low time)))

(defun tenths-of-a-second (time)
  (/ (microsec time) 100000))

(defun sec-high (time)
  (nth 0 time))

(defun sec-low (time)
  (nth 1 time))

(defun microsec (time)
  (nth 2 time))

(defun picosec (time)
  (nth 3 time))

;;; stopwatch-time-helpers.el ends here
