;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(defconst SECONDS-IN-A-MINUTE 60)
(defconst SECONDS-IN-AN-HOUR 3600)
(defconst MINUTES-IN-AN-HOUR 60)

(defconst STOPWATCH-RUNNING-INSTRUCTIONS
  "Press 'q' to quit, 's' to stop stopwatch, 'l' to finish a lap")
(defconst STOPWATCH-STOPPED-INSTRUCTIONS
  "Press 'q' to quit, 's' to start stopwatch, 'r' to reset")

(defface stopwatch-ultra-bold
  '((default . (:weight ultra-bold)))
  "Ultra-bold face for the stopwatch seconds display")

(defface stopwatch-ultra-light
  '((default . (:weight ultra-light)))
  "Ultra-light face for the stopwatch instructions display")

(defun stopwatch-display (stopwatch-is-running total-time laps buffer window-width)
  (with-current-buffer buffer
    (erase-buffer)
    (if stopwatch-is-running
        (insert STOPWATCH-RUNNING-INSTRUCTIONS)
      (insert STOPWATCH-STOPPED-INSTRUCTIONS))
    (add-text-properties 1 (point-max) '(face stopwatch-ultra-light))
    (insert "\n\n")
    (let* ((total-seconds (stopwatch-round-down-to-seconds total-time))
           (watchface-string (stopwatch-to-watchface-string total-seconds))
           (ascii-art-lines (stopwatch-ascii-artify watchface-string))
           (seconds-display-start (point-max)))
      (insert (stopwatch-centre-justify-lines ascii-art-lines
                                              buffer
                                              window-width))
      (let ((seconds-display-end (point-max)))
         (add-text-properties seconds-display-start
                              seconds-display-end
                              '(face stopwatch-ultra-bold)))
      )
    (insert (format " . %d" (stopwatch-tenths-of-a-second total-time)))
    (insert "\n\n\n")
    (let* ((stringified-laps (seq-map 'stopwatch-to-watchface-string-precise
                                      laps))
           (labelled-laps (stopwatch-prepend-lap-numbers stringified-laps))
           (laps-display (stopwatch-centre-justify-lines labelled-laps
                                                         buffer
                                                         window-width)))
      (insert laps-display))))

(defun stopwatch-centre-justify-lines (lines buffer window-width)
  (mapconcat (lambda (line) (stopwatch-centre-justify-line line buffer window-width))
             lines
             "\n"))

(defun stopwatch-centre-justify-line (line buffer window-width)
  (let* ((line-width (with-current-buffer buffer (string-width line)))
         (indent (/ (- window-width line-width) 2)))
    (concat (make-string indent ?\s) line)))

(defun stopwatch-prepend-lap-numbers (laps)
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

(defun stopwatch-to-watchface-string (elapsed-seconds)
  (let* ((hours (hours-from-seconds elapsed-seconds))
         (minutes (minutes-from-seconds elapsed-seconds))
         (seconds (% elapsed-seconds SECONDS-IN-A-MINUTE)))
    (cond ((> hours 0) (format "%d:%02d:%02d" hours minutes seconds))
          ((> minutes 0) (format "%d:%02d" minutes seconds))
          (t (format "%d" seconds)))))

(defun stopwatch-to-watchface-string-precise (elapsed-time)
  (let* ((total-seconds (stopwatch-round-down-to-seconds elapsed-time))
         (hours-minutes-seconds (stopwatch-to-watchface-string total-seconds))
         (sub-seconds (format ".%d"
                              (stopwatch-tenths-of-a-second elapsed-time))))
    (concat hours-minutes-seconds sub-seconds)))

(defun minutes-from-seconds (seconds)
  (let ((total-minutes (/ seconds SECONDS-IN-A-MINUTE)))
    (% total-minutes MINUTES-IN-AN-HOUR)))

(defun hours-from-seconds (seconds)
  (/ seconds SECONDS-IN-AN-HOUR))

(defun stopwatch-ascii-artify (watchface-string)
  (let* ((ascii-art-digits (stopwatch-ascii-art-list watchface-string))
         (line-numbers (number-sequence 0 (1- STOPWATCH-ASCII-ART-NUM-ROWS))))
    (mapcar (lambda (n) (stopwatch-line-n n ascii-art-digits)) line-numbers)))

(defun stopwatch-line-n (n ascii-art-list)
  (mapconcat (lambda (list) (nth n list)) ascii-art-list ""))

(defun stopwatch-ascii-art-list (watchface-string)
  (mapcar
   (lambda (c) (cdr (assoc c STOPWATCH-ASCII-ART-NUMBERS)))
   watchface-string))

;;; stopwatch-draw.el ends here
