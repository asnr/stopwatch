;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(defconst FLOAT-TOLERANCE 1e-6)

(defun buffers= (buffer other-buffer)
  (string= (buffer-name buffer) (buffer-name other-buffer)))

(defun controller-construct (stopwatch-buffer)
  (list (cons 'buffer stopwatch-buffer)
        (cons 'original-window-width nil)
        (cons 'timer nil)
        (cons 'model nil)
        (cons 'is-running nil)))

(defun controller-start-or-stop-stopwatch (controller)
  (if (controller--running-p controller)
      (controller--stop-stopwatch controller)
    (controller--start-stopwatch controller))
  ;; Update displayed instructions
  (controller--display controller))

(defun controller-end-lap (controller)
  (when (controller--running-p controller)
    (let* ((stopwatch (controller--get-model controller))
           (stopwatch-with-lap (stopwatch-end-lap stopwatch)))
      (controller--set-model controller stopwatch-with-lap)
      (controller--display controller))))

(defun controller-reset-stopwatch (controller)
  (when (not (controller--running-p controller))
    (controller--set-model controller (stopwatch-construct))
    (controller--display controller)))

(defun controller-start (controller)
  (controller--set-buffer-mode controller)
  (controller--set-as-buffer-local-variable controller)
  (controller--set-model controller (stopwatch-construct))
  (controller--attach-finalise-hook controller)
  (controller--init-buffer-and-open-window controller))

(defun controller--set-buffer-mode (controller)
  (with-current-buffer (controller--get-buffer controller)
    (stopwatch-mode)))

(defun controller--set-as-buffer-local-variable (controller)
  (with-current-buffer (controller--get-buffer controller)
    (setq-local buffer-local-stopwatch-controller controller)))

(defun controller--start-stopwatch (controller)
  (let ((stopwatch (controller--get-model controller)))
    (setq stopwatch (stopwatch-start stopwatch))
    (controller--set-model controller stopwatch)
    (let* ((update-display (lambda () (controller--display controller)))
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
      (controller--set-is-running controller t)
      (controller--set-timer controller timer))))

(defun controller--stop-stopwatch (controller)
  (let ((stopwatch (controller--get-model controller)))
    (controller--set-model controller (stopwatch-stop stopwatch))
    (cancel-timer (controller--get-timer controller))
    (controller--set-is-running controller nil)))

(defun controller--display (controller)
  (let* ((model (controller--get-model controller))
         (stopped-watch (stopwatch-stop model))
         (total-time (stopwatch-total-time stopped-watch))
         (laps (stopwatch-get-all-laps stopped-watch))
         (buffer (controller--get-buffer controller))
         (is-running (controller--running-p controller))
         (window-width (controller--get-original-window-width controller)))
    (display-stopwatch is-running total-time laps buffer window-width)))

(defun controller--attach-finalise-hook (controller)
  (add-hook 'kill-buffer-hook (lambda () (controller--finalise controller))))

(defun controller--finalise (controller)
  (when (buffers= (current-buffer) (controller--get-buffer controller))
    (let ((timer (controller--get-timer controller)))
      (when timer
        (cancel-timer timer)))))

(defun controller--init-buffer-and-open-window (controller)
  ;; We will use split-window to create the stopwatch window, so the
  ;; stopwatch-window width will be equal to width of the current window.
  (controller--set-original-window-width controller (window-body-width))
  (controller--display controller)
  (let ((window (split-window)))
    (set-window-buffer window (controller--get-buffer controller))
    (select-window window)))

(defalias 'controller--running-p 'controller--get-is-running)

(defun controller--get-is-running (controller)
  (cdr (assoc 'is-running controller)))

(defun controller--set-is-running (controller is-running)
  (setf (cdr (assoc 'is-running controller)) is-running))

(defun controller--get-timer (controller)
  (cdr (assoc 'timer controller)))

(defun controller--set-timer (controller timer)
  (setf (cdr (assoc 'timer controller)) timer))

(defun controller--get-model (controller)
  (cdr (assoc 'model controller)))

(defun controller--set-model (controller model)
  (setf (cdr (assoc 'model controller)) model))

(defun controller--get-buffer (controller)
  (cdr (assoc 'buffer controller)))

(defun controller--get-original-window-width (controller)
  (cdr (assoc 'original-window-width controller)))

(defun controller--set-original-window-width (controller original-window-width)
  (setf (cdr (assoc 'original-window-width controller)) original-window-width))

;;; stopwatch-controller.el ends here
