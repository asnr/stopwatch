;; Stopwatch --- time yo'self -*- lexical-binding: t; -*-

;;; Commentary:

;; A stopwatch.

;;; Code:

(define-derived-mode stopwatch-mode special-mode "Stopwatch"
  "Major mode for stopwatch."
  ;; Should try and make this read only for the user but then how do we update
  ;; The buffer on every tick without throwing an error?
  (read-only-mode 0)
  (buffer-disable-undo))

(define-key stopwatch-mode-map "q" 'kill-buffer-and-window)
(define-key stopwatch-mode-map "s" 'stopwatch-mode-start-or-stop)
(define-key stopwatch-mode-map "l" 'stopwatch-mode-end-lap)
(define-key stopwatch-mode-map "r" 'stopwatch-mode-reset-stopwatch)

(defun stopwatch-mode-start-or-stop ()
  (interactive)
  (stopwatch-controller-start-or-stop-stopwatch buffer-local-stopwatch-controller))

(defun stopwatch-mode-end-lap ()
  (interactive)
  (stopwatch-controller-end-lap buffer-local-stopwatch-controller))

(defun stopwatch-mode-reset-stopwatch ()
  (interactive)
  (stopwatch-controller-reset-stopwatch buffer-local-stopwatch-controller))

;; By default, the evil normal mode keymaps will take precedence over the
;; stopwatch-mode-map and "q" will resolve to evil-record-macro. Make evil use
;; the Emacs state for stopwatch mode.
(when (boundp 'evil-emacs-state-modes)
  (add-to-list 'evil-emacs-state-modes 'stopwatch-mode))

(provide 'stopwatch-mode)

;;; stopwatch-mode.el ends here
