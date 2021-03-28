(in-package #:cl-user)

(defpackage #:%zed.jobs
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:gob #:%zed.game-object)
   (#:tm #:%zed.trait.manager))
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
      (let* ((trait-manager (gob::traits game-object))
             (new-order (list* trait (tm::order trait-manager))))
        (setf (tm::order trait-manager) (sort new-order #'< :key priority)))))
  (setf (enable-traits jobs) nil)
  nil)

(u:fn-> update-disabled-traits (jobs) null)
(defun update-disabled-traits (jobs)
  (declare (optimize speed))
  (dolist (job (disable-traits jobs))
    (destructuring-bind (game-object . trait) job
      (let ((trait-manager (gob::traits game-object)))
        (u:deletef (tm::order trait-manager) trait))))
  (setf (disable-traits jobs) nil)
  nil)
