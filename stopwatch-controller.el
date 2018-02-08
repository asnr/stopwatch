;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(defconst FLOAT-TOLERANCE 1e-6)

(defun buffers= (buffer other-buffer)
  (string= (buffer-name buffer) (buffer-name other-buffer)))

(defun stopwatch-controller-construct (stopwatch-buffer)
  (list (cons 'buffer stopwatch-buffer)
        (cons 'original-window-width nil)
        (cons 'timer nil)
        (cons 'model nil)
        (cons 'is-running nil)))

(defun stopwatch-controller-start-or-stop-stopwatch (controller)
  (if (stopwatch-controller--running-p controller)
      (stopwatch-controller--stop-stopwatch controller)
    (stopwatch-controller--start-stopwatch controller))
  ;; Update displayed instructions
  (stopwatch-controller--display controller))

(defun stopwatch-controller-end-lap (controller)
  (when (stopwatch-controller--running-p controller)
    (let* ((stopwatch (stopwatch-controller--get-model controller))
           (stopwatch-with-lap (stopwatch-end-lap stopwatch)))
      (stopwatch-controller--set-model controller stopwatch-with-lap)
      (stopwatch-controller--display controller))))

(defun stopwatch-controller-reset-stopwatch (controller)
  (when (not (stopwatch-controller--running-p controller))
    (stopwatch-controller--set-model controller (stopwatch-construct))
    (stopwatch-controller--display controller)))

(defun stopwatch-controller-start (controller)
  (stopwatch-controller--set-buffer-mode controller)
  (stopwatch-controller--set-as-buffer-local-variable controller)
  (stopwatch-controller--set-model controller (stopwatch-construct))
  (stopwatch-controller--attach-finalise-hook controller)
  (stopwatch-controller--init-buffer-and-open-window controller))

(defun stopwatch-controller--set-buffer-mode (controller)
  (with-current-buffer (stopwatch-controller--get-buffer controller)
    (stopwatch-mode)))

(defun stopwatch-controller--set-as-buffer-local-variable (controller)
  (with-current-buffer (stopwatch-controller--get-buffer controller)
    (setq-local buffer-local-stopwatch-controller controller)))

(defun stopwatch-controller--start-stopwatch (controller)
  (let ((stopwatch (stopwatch-controller--get-model controller)))
    (setq stopwatch (stopwatch-start stopwatch))
    (stopwatch-controller--set-model controller stopwatch)
    (let* ((update-display (lambda () (stopwatch-controller--display controller)))
           (fraction-to-next-tenth-raw (stopwatch-fraction-to-next-tenth stopwatch))
           ;; After fraction-to-next-second we're going to quiz the model for
           ;; the number of elapsed seconds. Add tolerance so that floating
           ;; point inaccuracies don't lead us to ask the model before the next
           ;; second has elapsed.
           (fraction-to-next-tenth (+ fraction-to-next-tenth-raw FLOAT-TOLERANCE))
           (one-tenth-intervals 0.1)
           (timer (run-at-time fraction-to-next-tenth
                               one-tenth-intervals
                               update-display)))
      (stopwatch-controller--set-is-running controller t)
      (stopwatch-controller--set-timer controller timer))))

(defun stopwatch-controller--stop-stopwatch (controller)
  (let ((stopwatch (stopwatch-controller--get-model controller)))
    (stopwatch-controller--set-model controller (stopwatch-stop stopwatch))
    (cancel-timer (stopwatch-controller--get-timer controller))
    (stopwatch-controller--set-is-running controller nil)))

(defun stopwatch-controller--display (controller)
  (let* ((model (stopwatch-controller--get-model controller))
         (stopped-watch (stopwatch-stop model))
         (total-time (stopwatch-total-time stopped-watch))
         (laps (stopwatch-get-all-laps stopped-watch))
         (buffer (stopwatch-controller--get-buffer controller))
         (is-running (stopwatch-controller--running-p controller))
         (window-width (stopwatch-controller--get-original-window-width controller)))
    (stopwatch-display is-running total-time laps buffer window-width)))

(defun stopwatch-controller--attach-finalise-hook (controller)
  (add-hook 'kill-buffer-hook
            (lambda () (stopwatch-controller--finalise controller))))

(defun stopwatch-controller--finalise (controller)
  (when (buffers= (current-buffer) (stopwatch-controller--get-buffer controller))
    (let ((timer (stopwatch-controller--get-timer controller)))
      (when timer
        (cancel-timer timer)))))

(defun stopwatch-controller--init-buffer-and-open-window (controller)
  ;; We will use split-window to create the stopwatch window, so the
  ;; stopwatch-window width will be equal to width of the current window.
  (stopwatch-controller--set-original-window-width controller (window-body-width))
  (stopwatch-controller--display controller)
  (let ((window (split-window)))
    (set-window-buffer window (stopwatch-controller--get-buffer controller))
    (select-window window)))

(defalias 'stopwatch-controller--running-p 'stopwatch-controller--get-is-running)

(defun stopwatch-controller--get-is-running (controller)
  (cdr (assoc 'is-running controller)))

(defun stopwatch-controller--set-is-running (controller is-running)
  (setf (cdr (assoc 'is-running controller)) is-running))

(defun stopwatch-controller--get-timer (controller)
  (cdr (assoc 'timer controller)))

(defun stopwatch-controller--set-timer (controller timer)
  (setf (cdr (assoc 'timer controller)) timer))

(defun stopwatch-controller--get-model (controller)
  (cdr (assoc 'model controller)))

(defun stopwatch-controller--set-model (controller model)
  (setf (cdr (assoc 'model controller)) model))

(defun stopwatch-controller--get-buffer (controller)
  (cdr (assoc 'buffer controller)))

(defun stopwatch-controller--get-original-window-width (controller)
  (cdr (assoc 'original-window-width controller)))

(defun stopwatch-controller--set-original-window-width (controller original-window-width)
  (setf (cdr (assoc 'original-window-width controller)) original-window-width))

;;; stopwatch-controller.el ends here
