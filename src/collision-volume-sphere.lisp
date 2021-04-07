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

(u:fn-> update-broad-phase-sphere (collision-volume-sphere) null)
(defun update-broad-phase-sphere (sphere)
  (declare (optimize speed))
  (let ((world-center (collision-volume-world-center sphere))
        (radius (v3:uniform (collision-volume-sphere-radius sphere))))
    (declare (dynamic-extent radius))
    (v3:-! (collision-volume-broad-phase-min sphere) world-center radius)
    (v3:+! (collision-volume-broad-phase-max sphere) world-center radius)
    nil))

(u:fn-> update-collision-volume-sphere (collision-volume-sphere trait) null)
(defun update-collision-volume-sphere (sphere collider)
  (declare (optimize speed))
  (let ((owner (trait-owner collider)))
    (v3:with-components ((s (get-scale owner :space :world)))
      (transform-point! owner
                        (collision-volume-center sphere)
                        (collision-volume-world-center sphere))
      (setf (collision-volume-sphere-radius sphere) (max sx sy sz)))
    (update-broad-phase-sphere sphere)
    nil))
