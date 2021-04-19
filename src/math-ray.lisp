(in-package #:zed.math.ray)

(declaim (inline %ray))
(defstruct (ray
            (:constructor %ray (origin direction))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (origin (p3:point) :type p3:point)
  (direction (v3:vec 0 0 1) :type v3:vec))

(u:fn-> ray (&key (:origin p3:point) (:direction v3:vec)) ray)
(declaim (inline ray))
(defun ray (&key (origin (p3:point)) (direction (v3:vec 0 0 1)))
  (declare (optimize speed))
  (%ray origin (v3:normalize direction)))

(u:fn-> from-points (&key (:from p3:point) (:to p3:point)) ray)
(defun from-points (&key (from (p3:point)) (to (p3:point)))
  (declare (optimize speed))
  (let ((direction (v3:- to from)))
    (declare (dynamic-extent direction))
    (%ray (v3:copy from) (v3:normalize direction))))
