;;; stopwatch.el --- time yo'self -*- lexical-binding: t; -*-

;; Maintainer: asnr <https://github.com/asnr>
;; Version: 0.0.1
;; URL: https://github.com/asnr/stopwatch

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
  "Open a stopwatch in a new window."
  (interactive)
  (let* ((stopwatch-buffer (generate-new-buffer "stopwatch"))
         (controller (stopwatch-controller-construct stopwatch-buffer)))
    (stopwatch-controller-start controller)))

(provide 'stopwatch)

;;; stopwatch.el ends here
