;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(defconst SECONDS-IN-A-MINUTE 60)
(defconst SECONDS-IN-AN-HOUR 3600)
(defconst MINUTES-IN-AN-HOUR 60)

(defconst RUNNING-INSTRUCTIONS
  "Press 'q' to quit, 's' to stop stopwatch, 'l' to finish a lap")
(defconst STOPPED-INSTRUCTIONS
  "Press 'q' to quit, 's' to start stopwatch, 'r' to reset")

(defface ultra-bold
  '((default . (:weight ultra-bold)))
  "Ultra-bold face for the stopwatch seconds display")

(defface ultra-light
  '((default . (:weight ultra-light)))
  "Ultra-light face for the stopwatch instructions display")

(defun display-stopwatch (stopwatch-is-running total-time laps buffer window-width)
  (with-current-buffer buffer
    (erase-buffer)
    (if stopwatch-is-running
        (insert RUNNING-INSTRUCTIONS)
      (insert STOPPED-INSTRUCTIONS))
    (add-text-properties 1 (point-max) '(face ultra-light))
    (insert "\n\n")
    (let* ((total-seconds (round-down-to-seconds total-time))
           (watchface-string (to-watchface-string total-seconds))
           (ascii-art-lines (ascii-artify watchface-string))
           (seconds-display-start (point-max)))
      (insert (centre-justify-lines ascii-art-lines buffer window-width))
      (let ((seconds-display-end (point-max)))
         (add-text-properties seconds-display-start
                              seconds-display-end
                              '(face ultra-bold)))
      )
    (insert (format " . %d" (tenths-of-a-second total-time)))
    (insert "\n\n\n")
    (let* ((stringified-laps (seq-map 'to-watchface-string-precise laps))
           (labelled-laps (prepend-lap-numbers stringified-laps))
           (laps-display (centre-justify-lines labelled-laps buffer window-width)))
      (insert laps-display))))

(defun centre-justify-lines (lines buffer window-width)
  (mapconcat (lambda (line) (centre-justify-line line buffer window-width)) lines "\n"))

(defun centre-justify-line (line buffer window-width)
  (let* ((line-width (with-current-buffer buffer (string-width line)))
         (indent (/ (- window-width line-width) 2)))
    (concat (make-string indent ?\s) line)))

(defun prepend-lap-numbers (laps)
  (let* ((lap-number (length laps))
         (prepend-lap-number (lambda (lap)
                               ;; The extra spaces make the alignment a little prettier.
                               ;; It's hacky, but works for now.
                               (let* ((label (format "     Lap %-2d " lap-number))
                                      (padded-lap (format "%6s" lap))
                                      (lap-with-number (concat label padded-lap)))
                                 (setq lap-number (1- lap-number))
                                 lap-with-number))))
    (mapcar prepend-lap-number laps)))

(defun to-watchface-string (elapsed-seconds)
  (let* ((hours (hours-from-seconds elapsed-seconds))
         (minutes (minutes-from-seconds elapsed-seconds))
         (seconds (% elapsed-seconds SECONDS-IN-A-MINUTE)))
    (cond ((> hours 0) (format "%d:%02d:%02d" hours minutes seconds))
          ((> minutes 0) (format "%d:%02d" minutes seconds))
          (t (format "%d" seconds)))))

(defun to-watchface-string-precise (elapsed-time)
  (let* ((total-seconds (round-down-to-seconds elapsed-time))
         (hours-minutes-seconds (to-watchface-string total-seconds))
         (sub-seconds (format ".%d" (tenths-of-a-second elapsed-time))))
    (concat hours-minutes-seconds sub-seconds)))

(defun minutes-from-seconds (seconds)
  (let ((total-minutes (/ seconds SECONDS-IN-A-MINUTE)))
    (% total-minutes MINUTES-IN-AN-HOUR)))

(defun hours-from-seconds (seconds)
  (/ seconds SECONDS-IN-AN-HOUR))

(defun ascii-artify (watchface-string)
  (let* ((ascii-art-digits (ascii-art-list watchface-string))
         (line-numbers (number-sequence 0 (1- ASCII-ART-NUM-ROWS))))
    (mapcar (lambda (n) (line-n n ascii-art-digits)) line-numbers)))

(defun line-n (n ascii-art-list)
  (mapconcat (lambda (list) (nth n list)) ascii-art-list ""))

(defun ascii-art-list (watchface-string)
  (mapcar
   (lambda (c) (cdr (assoc c RAW-ASCII-ART-NUMBERS)))
   watchface-string))

;;; stopwatch-draw.el ends here
