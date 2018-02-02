(require 'ert)
(require 'stopwatch)

(defconst TIME-ZERO '(0 0 0 0))

(defmacro timecop-freeze (time &rest body)
  ;; Careful: current-time is a primitive function. When called from C, the
  ;; advice won't be run.
  (declare (indent 1))
  (let ((result-var (make-symbol "result")))
    `(let ((,result-var nil))
       (advice-add 'current-time
                   :around
                   (lambda (orig-fun) ,time)
                   '((name . timecop-freeze-time)))
       (unwind-protect
           (setq ,result-var (progn ,@body))
         (advice-remove 'current-time 'timecop-freeze-time))
       ,result-var)))

(ert-deftest stopwatch-starts-at-zero ()
  (let ((stopwatch (stopwatch-construct)))
    (should (equal (stopwatch-total-time stopwatch) TIME-ZERO))))

(ert-deftest total-time-returns-time-between-start-and-stop ()
  (let* ((stopwatch (stopwatch-construct))
         (stopwatch (timecop-freeze '(1 1 1 1) (stopwatch-start stopwatch)))
         (total-time (timecop-freeze '(1 2 3 4) (stopwatch-total-time stopwatch))))
     (should (equal total-time '(0 1 2 3)))))
