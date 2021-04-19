(in-package #:zed.math.sphere)

(declaim (inline sphere))
(defstruct (sphere
            (:constructor sphere)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (origin (p3:point) :type p3:point)
  (radius 1.0 :type u:f32))

(u:define-printer (sphere stream :type nil)
  (format stream "Sphere"))

(u:fn-> bounding-aabb! (aabb:aabb sphere) aabb:aabb)
(defun bounding-aabb! (aabb sphere)
  (declare (optimize speed))
  (let* ((origin (origin sphere))
         (radius (v3:uniform (radius sphere)))
         (min (v3:- origin radius))
         (max (v3:+ origin radius)))
    (declare (dynamic-extent radius min max))
    (aabb:from-min/max! aabb min max)
    aabb))

(u:fn-> bounding-aabb (sphere) aabb:aabb)
(defun bounding-aabb (sphere)
  (declare (optimize speed))
  (bounding-aabb! (aabb:aabb) sphere))
