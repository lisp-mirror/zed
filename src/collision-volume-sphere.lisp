(in-package #:zed)

(declaim (inline make-collision-volume-sphere))
(defstruct (collision-volume-sphere
            (:include collision-volume
             (type :sphere)
             (mesh-name "sphere")
             (update-visualization-func #'update-collision-volume-sphere-visualization))
            (:predicate nil)
            (:copier nil))
  (radius 1.0 :type u:f32))

(u:fn-> update-collision-volume-sphere-visualization (collision-volume-sphere game-object) null)
(defun update-collision-volume-sphere-visualization (sphere visual)
  (declare (optimize speed))
  (translate visual (collision-volume-center sphere) :replace-p t)
  (scale visual (v3:uniform (collision-volume-sphere-radius sphere)) :replace-p t)
  nil)
