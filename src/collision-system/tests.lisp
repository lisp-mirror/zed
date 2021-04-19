(in-package #:zed)

(defgeneric collide-p (geometry1 geometry2))

(defmethod collide-p ((sphere1 sphere:sphere) (sphere2 sphere:sphere))
  (declare (optimize speed))
  (geo.test:sphere/sphere sphere1 sphere2))

(defmethod collide-p ((sphere sphere:sphere) (obb obb:obb))
  (declare (optimize speed))
  (geo.test:sphere/obb sphere obb))

(defmethod collide-p ((obb obb:obb) (sphere sphere:sphere))
  (declare (optimize speed))
  (geo.test:obb/sphere obb sphere))

(defmethod collide-p ((obb1 obb:obb) (obb2 obb:obb))
  (declare (optimize speed))
  (geo.test:obb/obb obb1 obb2))

(defgeneric raycast (ray geometry))

(defmethod raycast ((ray ray:ray) (sphere sphere:sphere))
  (declare (optimize speed))
  (raycast:ray/sphere ray sphere))

(defmethod raycast ((ray ray:ray) (obb obb:obb))
  (declare (optimize speed))
  (raycast:ray/obb ray obb))
