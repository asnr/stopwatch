;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(load-file "./stopwatch-mode.el")
(load-file "./stopwatch-controller.el")
(load-file "./stopwatch-model.el")
(load-file "./stopwatch-ascii-art-numbers.el")
(load-file "./stopwatch-draw.el")
(load-file "./stopwatch-time-helpers.el")

(defun stopwatch ()
  "Open the stopwatch in a new window."
  (interactive)
  (let* ((stopwatch-buffer (generate-new-buffer "stopwatch"))
         (controller (controller-construct stopwatch-buffer)))
    (controller-start controller)))

(provide 'stopwatch)

;;; stopwatch.el ends here
