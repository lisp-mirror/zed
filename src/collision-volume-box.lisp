(in-package #:zed)

(declaim (inline make-collision-volume-box))
(defstruct (collision-volume-box
            (:include collision-volume
             (type :box)
             (mesh-name "box")
             (update-func #'update-collision-volume-box)
             (update-visualization-func #'update-collision-volume-box-visualization))
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

(u:fn-> get-closest-point/box-point (collision-volume-box v3:vec) v3:vec)
(defun get-closest-point/box-point (box point)
  (declare (optimize speed))
  (let* ((world-center (collision-volume-box-world-center box))
         (axes (collision-volume-box-axes box))
         (d (v3:-! (collision-volume-box-closest-point-distance box) point world-center))
         (q (v3:copy! (collision-volume-box-closest-point box) world-center)))
    (dotimes (i 3)
      (let* ((e (aref (collision-volume-box-half-extents box) i))
             (axis (m3:get-column! (collision-volume-box-axis-column box) axes i))
             (dist (u:clamp (v3:dot d axis) (- e) e)))
        (v3:scale! axis axis dist)
        (v3:+! q q axis)))
    q))

(u:fn-> make-box-box-rotation (collision-volume-box collision-volume-box) (values m3:mat m3:mat))
(defun make-box-box-rotation (box1 box2)
  (declare (optimize speed))
  (let ((axes1 (collision-volume-box-axes box1))
        (axis1 (collision-volume-box-axis-column box1))
        (axes2 (collision-volume-box-axes box2))
        (axis2 (collision-volume-box-axis-column box2)))
    (m3:with-components ((a (collision-volume-box-rotation box1))
                         (b (collision-volume-box-rotation box2)))
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

(u:fn-> make-box-box-translation (collision-volume-box collision-volume-box) v3:vec)
(defun make-box-box-translation (box1 box2)
  (declare (optimize speed))
  (let* ((axes1 (collision-volume-box-axes box1))
         (axis1 (collision-volume-box-axis-column box1))
         (world-center1 (collision-volume-box-world-center box1))
         (world-center2 (collision-volume-box-world-center box2))
         (translation (v3:-! (collision-volume-box-translation box1) world-center2 world-center1)))
    (v3:vec (v3:dot translation (m3:get-column! axis1 axes1 0))
            (v3:dot translation (m3:get-column! axis1 axes1 1))
            (v3:dot translation (m3:get-column! axis1 axes1 2)))))

(u:fn-> update-collision-volume-box (collision-volume-box trait) null)
(defun update-collision-volume-box (box collider)
  (declare (optimize speed))
  (let* ((owner (trait-owner collider))
         (center (collision-volume-center box))
         (min (transform-point owner (v3:+ center (collision-volume-box-min-extent box))))
         (max (transform-point owner (v3:+ center (collision-volume-box-max-extent box))))
         (center (v3:lerp! (collision-volume-box-extent-center box) min max 0.5))
         (axes (m4:rotation-to-mat3
                (m4:normalize-rotation (get-transform owner :space :world))))
         (axis (collision-volume-box-axis-column box))
         (diagonal (v3:-! (collision-volume-box-diagonal box) max center)))
    (setf (collision-volume-box-world-center box) center
          (collision-volume-box-axes box) axes
          (collision-volume-box-half-extents box) (v3:vec (v3:dot diagonal
                                                                  (m3:get-column! axis axes 0))
                                                          (v3:dot diagonal
                                                                  (m3:get-column! axis axes 1))
                                                          (v3:dot diagonal
                                                                  (m3:get-column! axis axes 2)))))
  nil)

(u:fn-> update-collision-volume-box-visualization (collision-volume-box game-object) null)
(defun update-collision-volume-box-visualization (box owner)
  (declare (optimize speed))
  (translate owner (collision-volume-center box) :replace-p t)
  (scale owner
         (v3:- (collision-volume-box-max-extent box)
               (collision-volume-box-min-extent box))
         :replace-p t)
  nil)
