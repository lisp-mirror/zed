(in-package #:cl-user)

(defpackage #:zed.math.quaternion
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:const #:zed.math.constants)
   (#:m3 #:zed.math.matrix3)
   (#:m4 #:zed.math.matrix4)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:conjugate
   #:length
   #:random)
  (:export
   #:quat
   #:with-components
   #:w
   #:x
   #:y
   #:z
   #:+id+
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
   #:copy!
   #:copy
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:scale!
   #:scale
   #:conjugate!
   #:conjugate
   #:cross!
   #:cross
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:negate!
   #:negate
   #:dot
   #:inverse!
   #:inverse
   #:rotate-euler!
   #:rotate-euler
   #:rotate!
   #:rotate
   #:to-euler!
   #:to-euler
   #:to-mat3!
   #:to-mat3
   #:to-mat4!
   #:to-mat4
   #:from-mat3!
   #:from-mat3
   #:from-mat4!
   #:from-mat4
   #:slerp!
   #:slerp
   #:from-axis-angle!
   #:from-axis-angle
   #:orient!
   #:orient
   #:from-velocity!
   #:from-velocity))

(in-package #:zed.math.quaternion)

(deftype quat () '(u:f32a 4))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  (u:once-only (quat)
    `(symbol-macrolet
         ((,prefix ,quat)
          (,(com::make-accessor-symbol prefix "W") (aref ,quat 0))
          (,(com::make-accessor-symbol prefix "X") (aref ,quat 1))
          (,(com::make-accessor-symbol prefix "Y") (aref ,quat 2))
          (,(com::make-accessor-symbol prefix "Z") (aref ,quat 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(u:fn-> quat (u:f32 u:f32 u:f32 u:f32) quat)
(declaim (inline quat))
(u:eval-always
  (defun quat (w x y z)
    (declare (optimize speed))
    (let ((quat (u:make-f32-array 4)))
      (setf (aref quat 0) w
            (aref quat 1) x
            (aref quat 2) y
            (aref quat 3) z)
      quat)))

(u:fn-> w (quat) u:f32)
(declaim (inline w))
(defun w (quat)
  (declare (optimize speed))
  (aref quat 0))

(u:fn-> (setf w) (u:f32 quat) u:f32)
(declaim (inline (setf w)))
(defun (setf w) (value quat)
  (declare (optimize speed))
  (setf (aref quat 0) value))

(u:fn-> x (quat) u:f32)
(declaim (inline x))
(defun x (quat)
  (declare (optimize speed))
  (aref quat 1))

(u:fn-> (setf x) (u:f32 quat) u:f32)
(declaim (inline (setf x)))
(defun (setf x) (value quat)
  (declare (optimize speed))
  (setf (aref quat 1) value))

(u:fn-> y (quat) u:f32)
(declaim (inline y))
(defun y (quat)
  (declare (optimize speed))
  (aref quat 2))

(u:fn-> (setf y) (u:f32 quat) u:f32)
(declaim (inline (setf y)))
(defun (setf y) (value quat)
  (declare (optimize speed))
  (setf (aref quat 2) value))

(u:fn-> z (quat) u:f32)
(declaim (inline z))
(defun z (quat)
  (declare (optimize speed))
  (aref quat 3))

(u:fn-> (setf z) (u:f32 quat) u:f32)
(declaim (inline (setf z)))
(defun (setf z) (value quat)
  (declare (optimize speed))
  (setf (aref quat 3) value))

(u:define-constant +id+ (quat 1.0 0.0 0.0 0.0) :test #'equalp)

(u:fn-> = (quat quat &key (:rel u:f32) (:abs u:f32)) boolean)
(declaim (inline =))
(defun = (quat1 quat2 &key (rel 1e-7) (abs rel))
  (declare (optimize speed))
  (com::cwcmp 4 (quat1 quat2) (com::= quat1 quat2 rel abs)))

(u:fn-> id! (quat) quat)
(declaim (inline id!))
(defun id! (quat)
  (declare (optimize speed))
  (with-components ((q quat))
    (psetf qw 1.0 qx 0.0 qy 0.0 qz 0.0))
  quat)

(u:fn-> id () quat)
(declaim (inline id))
(defun id ()
  (declare (optimize speed))
  (quat 1.0 0.0 0.0 0.0))

(u:fn-> id-p (quat) boolean)
(declaim (inline id-p))
(defun id-p (quat)
  (declare (optimize speed))
  (= quat +id+))

(u:fn-> random! (quat u:f32 u:f32) quat)
(declaim (inline random!))
(defun random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (com::cwset 4 out nil (cl:+ min (cl:random diff))))
  out)

(u:fn-> random (u:f32 u:f32) quat)
(declaim (inline random))
(defun random (min max)
  (declare (optimize speed))
  (random! (id) min max))

(u:fn-> copy! (quat quat) quat)
(declaim (inline copy!))
(defun copy! (out quat)
  (declare (optimize speed))
  (com::cwset 4 out quat quat)
  out)

(u:fn-> copy (quat) quat)
(declaim (inline copy))
(defun copy (quat)
  (declare (optimize speed))
  (copy! (id) quat))

(u:fn-> +! (quat quat quat) quat)
(declaim (inline +!))
(defun +! (out quat1 quat2)
  (declare (optimize speed))
  (com::cwset 4 out (quat1 quat2) (cl:+ quat1 quat2))
  out)

(u:fn-> + (quat quat) quat)
(declaim (inline +))
(defun + (quat1 quat2)
  (declare (optimize speed))
  (+! (id) quat1 quat2))

(u:fn-> -! (quat quat quat) quat)
(declaim (inline -!))
(defun -! (out quat1 quat2)
  (declare (optimize speed))
  (com::cwset 4 out (quat1 quat2) (cl:- quat1 quat2))
  out)

(u:fn-> - (quat quat) quat)
(declaim (inline -))
(defun - (quat1 quat2)
  (declare (optimize speed))
  (-! (id) quat1 quat2))

(u:fn-> *! (quat quat quat) quat)
(declaim (inline *!))
(defun *! (out quat1 quat2)
  (declare (optimize speed))
  (with-components ((o out) (q1 quat1) (q2 quat2))
    (psetf ow (cl:- (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))
           ox (cl:- (cl:+ (cl:* q1w q2x) (cl:* q1x q2w) (cl:* q1y q2z)) (cl:* q1z q2y))
           oy (cl:- (cl:+ (cl:* q1w q2y) (cl:* q1y q2w) (cl:* q1z q2x)) (cl:* q1x q2z))
           oz (cl:- (cl:+ (cl:* q1w q2z) (cl:* q1z q2w) (cl:* q1x q2y)) (cl:* q1y q2x))))
  out)

(u:fn-> * (quat quat) quat)
(declaim (inline *))
(defun * (quat1 quat2)
  (declare (optimize speed))
  (*! (id) quat1 quat2))

(u:fn-> scale! (quat quat u:f32) quat)
(declaim (inline scale!))
(defun scale! (out quat scalar)
  (declare (optimize speed))
  (with-components ((o out) (v quat))
    (psetf ow (cl:* vw scalar)
           ox (cl:* vx scalar)
           oy (cl:* vy scalar)
           oz (cl:* vz scalar)))
  out)

(u:fn-> scale (quat u:f32) quat)
(declaim (inline scale))
(defun scale (quat scalar)
  (declare (optimize speed))
  (scale! (id) quat scalar))

(u:fn-> conjugate! (quat quat) quat)
(declaim (inline conjugate!))
(defun conjugate! (out quat)
  (declare (optimize speed))
  (with-components ((o out) (q quat))
    (psetf ow qw
           ox (cl:- qx)
           oy (cl:- qy)
           oz (cl:- qz)))
  out)

(u:fn-> conjugate (quat) quat)
(declaim (inline conjugate))
(defun conjugate (quat)
  (declare (optimize speed))
  (conjugate! (id) quat))

(u:fn-> cross! (quat quat quat) quat)
(declaim (inline cross!))
(defun cross! (out quat1 quat2)
  (declare (optimize speed))
  (scale! out (+ (* quat2 (conjugate quat1)) (* quat1 quat2)) 0.5))

(u:fn-> cross (quat quat) quat)
(declaim (inline cross))
(defun cross (quat1 quat2)
  (declare (optimize speed))
  (cross! (id) quat1 quat2))

(u:fn-> length-squared (quat) u:f32)
(declaim (inline length-squared))
(defun length-squared (quat)
  (declare (optimize speed))
  (with-components ((q quat))
    (cl:+ (expt qw 2) (expt qx 2) (expt qy 2) (expt qz 2))))

(u:fn-> length (quat) u:f32)
(declaim (inline length))
(defun length (quat)
  (declare (optimize speed))
  (sqrt (length-squared quat)))

(u:fn-> normalize! (quat quat) quat)
(declaim (inline normalize!))
(defun normalize! (out quat)
  (declare (optimize speed))
  (let ((length (length quat)))
    (unless (zerop length)
      (scale! out quat (/ length))))
  out)

(u:fn-> normalize (quat) quat)
(declaim (inline normalize))
(defun normalize (quat)
  (declare (optimize speed))
  (normalize! (id) quat))

(u:fn-> negate! (quat quat) quat)
(declaim (inline negate!))
(defun negate! (out quat)
  (declare (optimize speed))
  (scale! out quat -1.0))

(u:fn-> negate (quat) quat)
(declaim (inline negate))
(defun negate (quat)
  (declare (optimize speed))
  (negate! (id) quat))

(u:fn-> dot (quat quat) u:f32)
(declaim (inline dot))
(defun dot (quat1 quat2)
  (declare (optimize speed))
  (with-components ((q1 quat1) (q2 quat2))
    (cl:+ (cl:* q1w q2w) (cl:* q1x q2x) (cl:* q1y q2y) (cl:* q1z q2z))))

(u:fn-> inverse! (quat quat) quat)
(declaim (inline inverse!))
(defun inverse! (out quat)
  (declare (optimize speed))
  (conjugate! out quat)
  (scale! out out (/ (length-squared quat)))
  out)

(u:fn-> inverse (quat) quat)
(declaim (inline inverse))
(defun inverse (quat)
  (declare (optimize speed))
  (inverse! (id) quat))

(u:fn-> rotate-euler! (quat quat v3:vec &key (:space keyword)) quat)
(defun rotate-euler! (out quat vec &key (space :local))
  (declare (optimize speed))
  (with-components ((o out))
    (let* ((vx (cl:* (v3:x vec) 0.5))
           (vy (cl:* (v3:y vec) 0.5))
           (vz (cl:* (v3:z vec) 0.5))
           (cx (cos vx))
           (cy (cos vy))
           (cz (cos vz))
           (sx (sin vx))
           (sy (sin vy))
           (sz (sin vz))
           (qw (w quat))
           (qx (x quat))
           (qy (y quat))
           (qz (z quat))
           (rw (cl:- (cl:* cx cy cz) (cl:* sx sy sz)))
           (rx (cl:+ (cl:* sx cy cz) (cl:* cx sy sz)))
           (ry (cl:- (cl:* cx sy cz) (cl:* sx cy sz)))
           (rz (cl:+ (cl:* sx sy cz) (cl:* cx cy sz))))
      (ecase space
        (:local
         (psetf ow (cl:- (cl:* qw rw) (cl:* qx rx) (cl:* qy ry) (cl:* qz rz))
                ox (cl:- (cl:* qw rx) (cl:* qx rw) (cl:* qy rz) (cl:* qz ry))
                oy (cl:- (cl:* qw ry) (cl:* qy rw) (cl:* qz rx) (cl:* qx rz))
                oz (cl:- (cl:* qw rz) (cl:* qz rw) (cl:* qx ry) (cl:* qy rx))))
        (:world
         (psetf ow (cl:- (cl:* rw qw) (cl:* rx qx) (cl:* ry qy) (cl:* rz qz))
                ox (cl:- (cl:* rw qx) (cl:* rx qw) (cl:* ry qz) (cl:* rz qy))
                oy (cl:- (cl:* rw qy) (cl:* ry qw) (cl:* rz qx) (cl:* rx qz))
                oz (cl:- (cl:* rw qz) (cl:* rz qw) (cl:* rx qy) (cl:* ry qx)))))))
  out)

(u:fn-> rotate-euler (quat v3:vec &key (:space keyword)) quat)
(declaim (inline rotate-euler))
(defun rotate-euler (quat vec &key (space :local))
  (declare (optimize speed))
  (rotate-euler! (id) quat vec :space space))

(u:fn-> rotate! (quat quat quat &key (:space keyword)) quat)
(defun rotate! (out quat1 quat2 &key (space :local))
  (declare (optimize speed))
  (ecase space
    (:local (*! out quat1 quat2))
    (:world (*! out quat2 quat1)))
  (normalize! out out))

(u:fn-> rotate (quat quat &key (:space keyword)) quat)
(declaim (inline rotate))
(defun rotate (quat1 quat2 &key (space :local))
  (declare (optimize speed))
  (rotate! (id) quat1 quat2 :space space))

(u:fn-> to-euler! (v3:vec quat) v3:vec)
(declaim (inline to-euler!))
(defun to-euler! (out quat)
  (declare (optimize speed))
  (with-components ((q quat))
    (let* ((sinr-cosp (cl:* 2.0 (cl:+ (cl:* qw qx) (cl:* qy qz))))
           (cosr-cosp (cl:- 1.0 (cl:* 2.0 (cl:+ (cl:* qx qx) (cl:* qy qy)))))
           (roll (atan sinr-cosp cosr-cosp))
           (sinp (cl:* 2.0 (cl:- (cl:* qw qy) (cl:* qz qx))))
           (pitch (if (>= (abs sinp) 1.0)
                      (cl:* const:+pi/2+ (signum sinp))
                      (asin (the (u:f32 -1.0 1.0) sinp))))
           (siny-cosp (cl:* 2.0 (cl:+ (cl:* qw qz) (cl:* qx qy))))
           (cosy-cosp (cl:- 1.0 (cl:* 2.0 (cl:+ (cl:* qy qy) (cl:* qz qz)))))
           (yaw (atan siny-cosp cosy-cosp)))
      (v3:with-components ((o out))
        (psetf ox roll
               oy pitch
               oz yaw))))
  out)

(u:fn-> to-euler (quat) v3:vec)
(declaim (inline to-euler))
(defun to-euler (quat)
  (declare (optimize speed))
  (to-euler! (v3:zero) quat))

(u:fn-> to-mat3! (m3:mat quat) m3:mat)
(declaim (inline to-mat3!))
(defun to-mat3! (out quat)
  (declare (optimize speed))
  (m3:with-components ((o out))
    (let* ((w (w quat))
           (x (x quat))
           (y (y quat))
           (z (z quat))
           (xx (cl:* x x))
           (xy (cl:* x y))
           (xz (cl:* x z))
           (xw (cl:* x w))
           (yy (cl:* y y))
           (yz (cl:* y z))
           (yw (cl:* y w))
           (zz (cl:* z z))
           (zw (cl:* z w)))
      (psetf o00 (cl:- 1.0 (cl:* (cl:+ yy zz) 2.0))
             o10 (cl:* (cl:+ xy zw) 2.0)
             o20 (cl:* (cl:- xz yw) 2.0)
             o01 (cl:* (cl:- xy zw) 2.0)
             o11 (cl:- 1.0 (cl:* (cl:+ xx zz) 2.0))
             o21 (cl:* (cl:+ yz xw) 2.0)
             o02 (cl:* (cl:+ xz yw) 2.0)
             o12 (cl:* (cl:- yz xw) 2.0)
             o22 (cl:- 1.0 (cl:* (cl:+ xx yy) 2.0)))))
  out)

(u:fn-> to-mat3 (quat) m3:mat)
(declaim (inline to-mat3))
(defun to-mat3 (quat)
  (declare (optimize speed))
  (to-mat3! (m3:id) quat))

(u:fn-> to-mat4! (m4:mat quat) m4:mat)
(defun to-mat4! (out quat)
  (declare (optimize speed))
  (m4:with-components ((o out))
    (let* ((w (w quat))
           (x (x quat))
           (y (y quat))
           (z (z quat))
           (xx (cl:* x x))
           (xy (cl:* x y))
           (xz (cl:* x z))
           (xw (cl:* x w))
           (yy (cl:* y y))
           (yz (cl:* y z))
           (yw (cl:* y w))
           (zz (cl:* z z))
           (zw (cl:* z w)))
      (psetf o00 (cl:- 1.0 (cl:* (cl:+ yy zz) 2.0))
             o10 (cl:* (cl:+ xy zw) 2.0)
             o20 (cl:* (cl:- xz yw) 2.0)
             o30 0.0
             o01 (cl:* (cl:- xy zw) 2.0)
             o11 (cl:- 1.0 (cl:* (cl:+ xx zz) 2.0))
             o21 (cl:* (cl:+ yz xw) 2.0)
             o31 0.0
             o02 (cl:* (cl:+ xz yw) 2.0)
             o12 (cl:* (cl:- yz xw) 2.0)
             o22 (cl:- 1.0 (cl:* (cl:+ xx yy) 2.0))
             o32 0.0
             o03 0.0
             o13 0.0
             o23 0.0
             o33 1.0)))
  out)

(u:fn-> to-mat4 (quat) m4:mat)
(declaim (inline to-mat4))
(defun to-mat4 (quat)
  (declare (optimize speed))
  (to-mat4! (m4:id) quat))

(u:fn-> from-mat3! (quat m3:mat) quat)
(defun from-mat3! (out mat)
  (declare (optimize speed))
  (with-components ((o out))
    (m3:with-components ((m mat))
      (psetf ow (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:+ m00 m11 m22 1.0)))) 0.5)
             ox (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:- (cl:+ m00 1.0) m11 m22))))
                      (signum (cl:- m21 m12))
                      0.5)
             oy (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:- (cl:+ (cl:- m00 1.0) m11) m22))))
                      (signum (cl:- m02 m20))
                      0.5)
             oz (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:+ (cl:- 1.0 m00 m11) m22))))
                      (signum (cl:- m10 m01))
                      0.5))))
  out)

(u:fn-> from-mat3 (m3:mat) quat)
(declaim (inline from-mat3))
(defun from-mat3 (mat)
  (declare (optimize speed))
  (from-mat3! (id) mat))

(u:fn-> from-mat4! (quat m4:mat) quat)
(defun from-mat4! (out mat)
  (declare (optimize speed))
  (with-components ((o out))
    (m4:with-components ((m mat))
      (psetf ow (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:+ m00 m11 m22 1.0)))) 0.5)
             ox (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:- (cl:+ m00 1.0) m11 m22))))
                      (signum (cl:- m21 m12))
                      0.5)
             oy (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:- (cl:+ (cl:- m00 1.0) m11) m22))))
                      (signum (cl:- m02 m20))
                      0.5)
             oz (cl:* (sqrt (the (u:f32 0.0) (max 0.0 (cl:+ (cl:- 1.0 m00 m11) m22))))
                      (signum (cl:- m10 m01))
                      0.5))))
  out)

(u:fn-> from-mat4 (m4:mat) quat)
(declaim (inline from-mat4))
(defun from-mat4 (mat)
  (declare (optimize speed))
  (from-mat4! (id) mat))

(u:fn-> slerp! (quat quat quat u:f32) quat)
(defun slerp! (out quat1 quat2 factor)
  (declare (optimize speed))
  (let ((factor factor))
    (with-components ((o out) (q1 quat1) (q2 quat2))
      (let ((dot (dot q1 q2))
            (q2 q2))
        (when (minusp dot)
          (negate! q2 q2)
          (setf dot (cl:- dot)))
        (if (> (abs dot) 0.9995)
            (psetf ow (u:lerp factor q1w q2w)
                   ox (u:lerp factor q1x q2x)
                   oy (u:lerp factor q1y q2y)
                   oz (u:lerp factor q1z q2z))
            (let* ((angle (acos (the (u:f32 -0.9995 0.9995) dot)))
                   (sin-angle (sin angle))
                   (scale1 (/ (sin (cl:* angle (cl:- 1 factor))) sin-angle))
                   (scale2 (/ (sin (cl:* factor angle)) sin-angle)))
              (psetf ow (cl:+ (cl:* q1w scale1) (cl:* q2w scale2))
                     ox (cl:+ (cl:* q1x scale1) (cl:* q2x scale2))
                     oy (cl:+ (cl:* q1y scale1) (cl:* q2y scale2))
                     oz (cl:+ (cl:* q1z scale1) (cl:* q2z scale2))))))))
  out)

(u:fn-> slerp (quat quat u:f32) quat)
(declaim (inline slerp))
(defun slerp (quat1 quat2 factor)
  (declare (optimize speed))
  (slerp! (id) quat1 quat2 factor))

(u:fn-> from-axis-angle! (quat v3:vec u:f32) quat)
(declaim (inline from-axis-angle!))
(defun from-axis-angle! (out axis angle)
  (declare (optimize speed))
  (let* ((half-angle (cl:* angle 0.5))
         (c (cos half-angle))
         (s (sin half-angle)))
    (with-components ((o out))
      (v3:with-components ((v axis))
        (psetf ow c
               ox (cl:* vx s)
               oy (cl:* vy s)
               oz (cl:* vz s)))))
  out)

(u:fn-> from-axis-angle (v3:vec u:f32) quat)
(declaim (inline from-axis-angle))
(defun from-axis-angle (axis angle)
  (declare (optimize speed))
  (from-axis-angle! (id) axis angle))

(u:fn-> orient! (quat keyword &rest (or keyword v3:vec u:f32)) quat)
(defun orient! (out space &rest axes/angles)
  (declare (optimize speed))
  (let ((quat (id)))
    (declare (dynamic-extent quat))
    (loop :for (axis angle) :on axes/angles :by #'cddr
          :for axis-vec = (case axis
                            (:x v3:+right+)
                            (:y v3:+up+)
                            (:z v3:+forward+)
                            (t (v3:normalize! axis axis)))
          :do (from-axis-angle! quat axis-vec angle)
              (ecase space
                (:local (*! out quat out))
                (:world (*! out out quat)))
              (normalize! out out)))
  out)

(u:fn-> orient (keyword &rest (or keyword v3:vec u:f32)) quat)
(declaim (inline orient))
(defun orient (space &rest axes/angles)
  (declare (optimize speed))
  (apply #'orient! (id) space axes/angles))

(u:fn-> from-velocity! (quat v3:vec u:f32) quat)
(declaim (inline from-velocity!))
(defun from-velocity! (out velocity delta)
  (declare (optimize speed))
  (let ((nav (v3:zero)))
    (declare (dynamic-extent nav))
    (v3:normalize! nav velocity)
    (from-axis-angle! out nav (cl:* (v3:length velocity) delta))
    (normalize! out out)))

(u:fn-> from-velocity (v3:vec u:f32) quat)
(declaim (inline from-velocity))
(defun from-velocity (velocity delta)
  (declare (optimize speed))
  (from-velocity! (id) velocity delta))
