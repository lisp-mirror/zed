(in-package #:zed)

(deftype volume-type () '(member :box :sphere))

(defstruct (volume
            (:predicate nil)
            (:copier nil))
  (geometry (obb:obb) :type (or obb:obb sphere:sphere))
  (broad-geometry (aabb:aabb) :type aabb:aabb)
  (collider nil :type trait)
  (layer nil :type symbol)
  (mesh-name "" :type string)
  (contacts (u:dict #'eq) :type hash-table)
  (source nil :type game-object)
  (update-func (constantly nil) :type function))

(u:define-printer (volume stream :type nil)
  (let ((path (game-object-path (trait-owner (volume-collider volume)))))
    (format stream "VOLUME: ~a (~a)" (class-name (class-of (volume-geometry volume))) path)))

(u:fn-> update-volume/box (volume) null)
(defun update-volume/box (box)
  (declare (optimize speed))
  (let* ((collider (volume-collider box))
         (geometry (volume-geometry box))
         (owner (trait-owner collider))
         (min (v3:+ v3:+zero+ #.(v3:uniform -0.5)))
         (max (v3:+ v3:+zero+ #.(v3:uniform 0.5)))
         (transform (get-transform owner :space :world))
         (rotation (obb:rotation geometry)))
    (declare (dynamic-extent min max))
    (transform-point! owner min min)
    (transform-point! owner max max)
    (m4:normalize-rotation! transform transform)
    (m4:rotation-to-mat3! rotation transform)
    (let* ((origin (v3:lerp! (obb:origin geometry) min max 0.5))
           (diagonal (v3:- max origin))
           (x (m3:get-column rotation 0))
           (y (m3:get-column rotation 1))
           (z (m3:get-column rotation 2)))
      (declare (dynamic-extent diagonal x y z))
      (v3:with-components ((s (obb:size geometry)))
        (setf (obb:origin geometry) origin
              sx (v3:dot diagonal x)
              sy (v3:dot diagonal y)
              sz (v3:dot diagonal z)))
      (obb:bounding-aabb! (volume-broad-geometry box) geometry)
      nil)))

(u:fn-> update-volume/sphere (volume) null)
(defun update-volume/sphere (sphere)
  (declare (optimize speed))
  (let* ((collider (volume-collider sphere))
         (geometry (volume-geometry sphere))
         (owner (trait-owner collider)))
    (v3:with-components ((s (get-scale owner :space :world)))
      (transform-point! owner v3:+zero+ (sphere:origin geometry))
      (setf (sphere:radius geometry) (max sx sy sz)))
    (sphere:bounding-aabb! (volume-broad-geometry sphere) geometry)
    nil))

(u:fn-> make-volume/box (trait symbol game-object) volume)
(defun make-volume/box (collider layer source)
  (declare (optimize speed))
  (make-volume :geometry (obb:obb)
               :mesh-name "box"
               :update-func #'update-volume/box
               :collider collider
               :layer layer
               :source source))

(u:fn-> make-volume/sphere (trait symbol game-object) volume)
(defun make-volume/sphere (collider layer source)
  (make-volume :geometry (sphere:sphere)
               :mesh-name "sphere"
               :update-func #'update-volume/sphere
               :collider collider
               :layer layer
               :source source))
