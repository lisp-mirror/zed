(in-package #:zed)

(declaim (inline make-collision-volume-sphere))
(defstruct (collision-volume-sphere
            (:include collision-volume
             (type :sphere)
             (mesh-name "sphere")
             (update-func #'update-collision-volume-sphere)
             (update-visualization-func #'update-collision-volume-sphere-visualization))
            (:predicate nil)
            (:copier nil))
  (radius 1.0 :type u:f32))

(u:fn-> update-collision-volume-sphere (collision-volume-sphere trait) null)
(defun update-collision-volume-sphere (sphere collider)
  (let ((owner (trait-owner collider)))
    (v3:with-components ((s (get-scale owner :space :world)))
      (setf (collision-volume-sphere-radius sphere) (max sx sy sz)))
    nil))

(u:fn-> update-collision-volume-sphere-visualization (collision-volume-sphere game-object) null)
(defun update-collision-volume-sphere-visualization (sphere owner)
  (declare (optimize speed))
  (translate owner (collision-volume-center sphere) :replace-p t)
  (scale owner (v3:uniform (collision-volume-sphere-radius sphere)) :replace-p t)
  nil)
