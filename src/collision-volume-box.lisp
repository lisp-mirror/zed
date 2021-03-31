(in-package #:cl-user)

(defpackage #:%zed.collision.volume.box
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:gob #:%zed.game-object)
   (#:m3 #:zed.math.matrix3)
   (#:m4 #:zed.math.matrix4)
   (#:tfm #:%zed.transform)
   (#:tr #:%zed.trait)
   (#:tr.mesh #:zed.trait.mesh)
   (#:v3 #:zed.math.vector3)
   (#:vol.struct #:%zed.collision.volume.struct))
  (:use #:cl)
  (:shadow
   #:type))

(in-package #:%zed.collision.volume.box)

(declaim (inline %make-box))
(defstruct (box
            (:include vol.struct::volume
             (type :box)
             (mesh-name "box")
             (update-func #'update)
             (update-visualization-func #'update-visualization))
            (:constructor %make-box)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (world-center (v3:zero) :type v3:vec)
  (axes (m3:zero) :type m3:mat)
  (half-extents (v3:zero) :type v3:vec)
  (min-extent (v3:uniform -0.5) :type v3:vec)
  (max-extent (v3:uniform 0.5) :type v3:vec)
  (closest-point-distance (v3:zero) :type v3:vec)
  (closest-point (v3:zero) :type v3:vec)
  (axis-column (v3:zero) :type v3:vec)
  (rotation (m3:id) :type m3:mat)
  (normalized-rotation (m3:id) :type m3:mat)
  (translation (v3:zero) :type v3:vec)
  (diagonal (v3:zero) :type v3:vec)
  (extent-center (v3:zero) :type v3:vec))

(u:fn-> get-closest-point/box-point (box v3:vec) v3:vec)
(defun get-closest-point/box-point (box point)
  (declare (optimize speed))
  (let* ((world-center (world-center box))
         (axes (axes box))
         (d (v3:-! (closest-point-distance box) point world-center))
         (q (v3:copy! (closest-point box) world-center)))
    (dotimes (i 3)
      (let* ((e (aref (half-extents box) i))
             (axis (m3:get-column! (axis-column box) axes i))
             (dist (u:clamp (v3:dot d axis) (- e) e)))
        (v3:scale! axis axis dist)
        (v3:+! q q axis)))
    q))

(u:fn-> make-box-box-rotation (box box) (values m3:mat m3:mat))
(defun make-box-box-rotation (box1 box2)
  (declare (optimize speed))
  (let ((axes1 (axes box1))
        (axis1 (axis-column box1))
        (axes2 (axes box2))
        (axis2 (axis-column box2)))
    (m3:with-components ((a (rotation box1))
                         (b (rotation box2)))
      (psetf a00 (v3:dot (m3:get-column! axis1 axes1 0) (m3:get-column! axis2 axes2 0))
             a10 (v3:dot (m3:get-column! axis1 axes1 1) (m3:get-column! axis2 axes2 0))
             a20 (v3:dot (m3:get-column! axis1 axes1 2) (m3:get-column! axis2 axes2 0))
             a01 (v3:dot (m3:get-column! axis1 axes1 0) (m3:get-column! axis2 axes2 1))
             a11 (v3:dot (m3:get-column! axis1 axes1 1) (m3:get-column! axis2 axes2 1))
             a21 (v3:dot (m3:get-column! axis1 axes1 2) (m3:get-column! axis2 axes2 1))
             a02 (v3:dot (m3:get-column! axis1 axes1 0) (m3:get-column! axis2 axes2 2))
             a12 (v3:dot (m3:get-column! axis1 axes1 1) (m3:get-column! axis2 axes2 2))
             a22 (v3:dot (m3:get-column! axis1 axes1 2) (m3:get-column! axis2 axes2 2)))
      (setf b00 (+ (abs a00) 1e-7)
            b10 (+ (abs a10) 1e-7)
            b20 (+ (abs a20) 1e-7)
            b01 (+ (abs a01) 1e-7)
            b11 (+ (abs a11) 1e-7)
            b21 (+ (abs a21) 1e-7)
            b02 (+ (abs a02) 1e-7)
            b12 (+ (abs a12) 1e-7)
            b22 (+ (abs a22) 1e-7))
      (values a b))))

(u:fn-> make-box-box-translation (box box) v3:vec)
(defun make-box-box-translation (box1 box2)
  (declare (optimize speed))
  (let* ((axes1 (axes box1))
         (axis1 (axis-column box1))
         (world-center1 (world-center box1))
         (world-center2 (world-center box2))
         (translation (v3:-! (translation box1) world-center2 world-center1)))
    (v3:vec (v3:dot translation (m3:get-column! axis1 axes1 0))
            (v3:dot translation (m3:get-column! axis1 axes1 1))
            (v3:dot translation (m3:get-column! axis1 axes1 2)))))

(u:fn-> update (box tr:trait) null)
(defun update (box collider)
  (declare (optimize speed))
  (let* ((owner (tr::owner collider))
         (center (center box))
         (min (tfm::transform-point owner (v3:+ center (min-extent box))))
         (max (tfm::transform-point owner (v3:+ center (max-extent box))))
         (center (v3:lerp! (extent-center box) min max 0.5))
         (axes (m4:rotation-to-mat3
                (m4:normalize-rotation (tfm::get-transform owner :space :world))))
         (axis (axis-column box))
         (diagonal (v3:-! (diagonal box) max center)))
    (setf (world-center box) center
          (axes box) axes
          (half-extents box) (v3:vec (v3:dot diagonal (m3:get-column! axis axes 0))
                                     (v3:dot diagonal (m3:get-column! axis axes 1))
                                     (v3:dot diagonal (m3:get-column! axis axes 2)))))
  nil)

(u:fn-> update-visualization (box) null)
(defun update-visualization (box)
  (declare (optimize speed))
  (let ((game-object (tr::owner (collider box))))
    (tfm::translate game-object (center box) :replace-p t)
    (tfm::scale game-object (v3:- (max-extent box) (min-extent box)) :replace-p t))
  nil)

(u:fn-> make-box (tr:trait) box)
(defun make-box (collider)
  (declare (optimize speed))
  (let ((box (%make-box :collider collider)))
    (u:when-let ((mesh (tr:find-trait (tr::owner collider) 'tr.mesh:mesh)))
      (u:mvlet ((min max (tr.mesh::get-extents mesh)))
        (setf (center box) (v3:scale (v3:+ min max) 0.5)
              (min-extent box) min
              (max-extent box) max)))
    box))