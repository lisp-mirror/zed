(in-package #:zed)

(declaim (inline make-collision-volume-sphere))
(defstruct (collision-volume-sphere
            (:include collision-volume
             (type :sphere)
             (mesh-name "sphere")
             (update-func #'update-collision-volume-sphere))
            (:predicate nil)
            (:copier nil))
  (radius 1.0 :type u:f32))
(u:fn-> update-collision-volume-sphere (collision-volume-sphere trait) null)
(defun update-collision-volume-sphere (sphere collider)
  (declare (optimize speed))
  (let ((owner (trait-owner collider)))
    (v3:with-components ((s (get-scale owner :space :world)))
      (transform-point! owner
                        (collision-volume-center sphere)
                        (collision-volume-world-center sphere))
      (setf (collision-volume-sphere-radius sphere) (max sx sy sz)))
    nil))
