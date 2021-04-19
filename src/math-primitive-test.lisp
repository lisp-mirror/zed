(in-package #:zed.math.primitive-test)

(u:fn-> sphere/sphere (sphere:sphere sphere:sphere) boolean)
(declaim (inline sphere/sphere))
(defun sphere/sphere (sphere1 sphere2)
  (declare (optimize speed))
  (<= (p3:distance-squared (sphere:origin sphere1) (sphere:origin sphere2))
      (expt (+ (sphere:radius sphere1) (sphere:radius sphere2)) 2)))

(u:fn-> %sphere/obb (sphere:sphere obb:obb) boolean)
(declaim (inline %sphere/obb))
(defun %sphere/obb (sphere obb)
  (declare (optimize speed))
  (let* ((sphere-origin (sphere:origin sphere))
         (closest-point (obb::closest-point obb sphere-origin)))
    (< (p3:distance-squared closest-point sphere-origin)
       (expt (sphere:radius sphere) 2))))

(u:fn-> sphere/obb (sphere:sphere obb:obb) boolean)
(declaim (inline sphere/obb))
(defun sphere/obb (sphere obb)
  (declare (optimize speed))
  (%sphere/obb sphere obb))

(u:fn-> obb/sphere (obb:obb sphere:sphere) boolean)
(declaim (inline obb/sphere))
(defun obb/sphere (obb sphere)
  (declare (optimize speed))
  (%sphere/obb sphere obb))

(u:fn-> obb/obb (obb:obb obb:obb) boolean)
(declaim (inline obb/obb))
(defun obb/obb (obb1 obb2)
  (declare (optimize speed))
  (u:mvlet ((r r-abs (obb::make-obb-obb-rotation obb1 obb2)))
    (m3:with-components ((r r) (ar r-abs))
      (v3:with-components ((tr (obb::make-obb-obb-translation obb1 obb2))
                           (h1 (obb:size obb1))
                           (h2 (obb:size obb2)))
        (not (or (> (abs trx) (+ h1x (* h2x ar00) (* h2y ar01) (* h2z ar02)))
                 (> (abs try) (+ h1y (* h2x ar10) (* h2y ar11) (* h2z ar12)))
                 (> (abs trz) (+ h1z (* h2x ar20) (* h2y ar21) (* h2z ar22)))
                 (> (abs (+ (* trx r00) (* try r10) (* trz r20)))
                    (+ (* h1x ar00) (* h1y ar10) (* h1z ar20) h2x))
                 (> (abs (+ (* trx r01) (* try r11) (* trz r21)))
                    (+ (* h1x ar01) (* h1y ar11) (* h1z ar21) h2y))
                 (> (abs (+ (* trx r02) (* try r12) (* trz r22)))
                    (+ (* h1x ar02) (* h1y ar12) (* h1z ar22) h2z))
                 (> (abs (- (* trz r10) (* try r20)))
                    (+ (* h1y ar20) (* h1z ar10) (* h2y ar02) (* h2z ar01)))
                 (> (abs (- (* trz r11) (* try r21)))
                    (+ (* h1y ar21) (* h1z ar11) (* h2x ar02) (* h2z ar00)))
                 (> (abs (- (* trz r12) (* try r22)))
                    (+ (* h1y ar22) (* h1z ar12) (* h2x ar01) (* h2y ar00)))
                 (> (abs (- (* trx r20) (* trz r00)))
                    (+ (* h1x ar20) (* h1z ar00) (* h2y ar12) (* h2z ar11)))
                 (> (abs (- (* trx r21) (* trz r01)))
                    (+ (* h1x ar21) (* h1z ar01) (* h2x ar12) (* h2z ar10)))
                 (> (abs (- (* trx r22) (* trz r02)))
                    (+ (* h1x ar22) (* h1z ar02) (* h2x ar11) (* h2y ar10)))
                 (> (abs (- (* try r00) (* trx r10)))
                    (+ (* h1x ar10) (* h1y ar00) (* h2y ar22) (* h2z ar21)))
                 (> (abs (- (* try r01) (* trx r11)))
                    (+ (* h1x ar11) (* h1y ar01) (* h2x ar22) (* h2z ar20)))
                 (> (abs (- (* try r02) (* trx r12)))
                    (+ (* h1x ar12) (* h1y ar02) (* h2x ar21) (* h2y ar20)))))))))

(u:fn-> %frustum/aabb (frustum:frustum aabb:aabb) boolean)
(declaim (inline %frustum/aabb))
(defun %frustum/aabb (frustum aabb)
  (declare (optimize speed))
  (let ((min (aabb:min aabb))
        (max (aabb:max aabb))
        (planes (vector (frustum:left frustum)
                        (frustum:right frustum)
                        (frustum:top frustum)
                        (frustum:bottom frustum)
                        (frustum:near frustum)
                        (frustum:far frustum))))
    (declare (dynamic-extent min max planes))
    (v3:with-components ((min- min)
                         (max- max))
      (dotimes (i 6)
        (let ((plane (aref planes i))
              (axis-vertex (v3:zero)))
          (declare (v4:vec plane)
                   (dynamic-extent axis-vertex))
          (v4:with-components ((p plane))
            (v3:with-components ((v axis-vertex))
              (if (minusp px)
                  (setf vx min-x)
                  (setf vx max-x))
              (if (minusp py)
                  (setf vy min-y)
                  (setf vy max-y))
              (if (minusp pz)
                  (setf vz min-z)
                  (setf vz max-z))
              (let ((plane-normal (v3:vec px py pz)))
                (declare (dynamic-extent plane-normal))
                (when (minusp (+ (v3:dot plane-normal v) pw))
                  (return-from %frustum/aabb t)))))))
      nil)))

(u:fn-> frustum/aabb (frustum:frustum aabb:aabb) boolean)
(declaim (inline frustum/aabb))
(defun frustum/aabb (frustum aabb)
  (declare (optimize speed))
  (%frustum/aabb frustum aabb))

(u:fn-> aabb/frustum (aabb:aabb frustum:frustum) boolean)
(declaim (inline aabb/frustum))
(defun aabb/frustum (aabb frustum)
  (declare (optimize speed))
  (%frustum/aabb frustum aabb))
