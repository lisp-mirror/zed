(in-package #:cl-user)

(defpackage #:%zed.jobs
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:gob #:%zed.game-object))
  (:use #:cl))

(in-package #:%zed.jobs)

(defstruct (jobs
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (enable-traits nil :type list)
  (disable-traits nil :type list))

(u:define-printer (jobs stream :type nil)
  (format stream "JOBS"))

(u:fn-> update-enabled-traits (jobs) null)
(defun update-enabled-traits (jobs)
  (declare (optimize speed))
  (dolist (job (enable-traits jobs))
    (destructuring-bind (game-object trait priority) job
      (declare (function priority))
      (let ((new-traits (list* trait (gob::traits game-object))))
        (setf (gob::traits game-object) (sort new-traits #'< :key priority)))))
  (setf (enable-traits jobs) nil)
  nil)

(u:fn-> update-disabled-traits (jobs) null)
(defun update-disabled-traits (jobs)
  (declare (optimize speed))
  (dolist (job (disable-traits jobs))
    (destructuring-bind (game-object . trait) job
      (u:deletef (gob::traits game-object) trait)))
  (setf (disable-traits jobs) nil)
  nil)
