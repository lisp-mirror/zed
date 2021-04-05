(in-package #:zed)

(declaim (inline make-collision-volume-sphere))
(defstruct (collision-volume-sphere
            (:include collision-volume
             (type :sphere)
             (mesh-name "sphere"))
            (:predicate nil)
            (:copier nil))
  (radius 1.0 :type u:f32))
