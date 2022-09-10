;;; stopwatch.el --- time yo'self -*- lexical-binding: t; -*-

;; Maintainer: asnr <https://github.com/asnr>
;; Version: 0.0.1
;; URL: https://github.com/asnr/stopwatch

;;; Commentary:

;; A stopwatch.

;;; Code:

(require 'stopwatch-mode)
(require 'stopwatch-controller)
(require 'stopwatch-model)
(require 'stopwatch-ascii-art-numbers)
(require 'stopwatch-draw)
(require 'stopwatch-time-helpers)

(defun stopwatch ()
  "Open a stopwatch in a new window."
  (interactive)
  (let* ((stopwatch-buffer (generate-new-buffer "stopwatch"))
         (controller (stopwatch-controller-construct stopwatch-buffer)))
    (stopwatch-controller-start controller)))

(provide 'stopwatch)

;;; stopwatch.el ends here
