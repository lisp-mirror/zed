(in-package #:zed)

(u:fn-> update-enabled-traits (jobs) null)
(defun update-enabled-traits (jobs)
  (declare (optimize speed))
  (dolist (job (jobs-enable-traits jobs))
    (destructuring-bind (game-object trait priority) job
      (declare (function priority))
      (let ((new-order (list* trait (game-object-trait-order game-object))))
        (setf (game-object-trait-order game-object) (sort new-order #'< :key priority)))))
  (setf (jobs-enable-traits jobs) nil)
  nil)

(u:fn-> update-disabled-traits (jobs) null)
(defun update-disabled-traits (jobs)
  (declare (optimize speed))
  (dolist (job (jobs-disable-traits jobs))
    (destructuring-bind (game-object . trait) job
      (u:deletef (game-object-trait-order game-object) trait)))
  (setf (jobs-disable-traits jobs) nil)
  nil)
