(in-package #:cl-user)

(defpackage #:%zed.camera-state
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:const #:zed.math.constants)
   (#:gob #:%zed.game-object)
   (#:m3 #:zed.math.matrix3)
   (#:m4 #:zed.math.matrix4)
   (#:q #:zed.math.quaternion)
   (#:ts #:%zed.transform-state)
   (#:v3 #:zed.math.vector3)
   (#:win #:%zed.window))
  (:use #:cl))

(in-package #:%zed.camera-state)

(deftype mode () '(member :perspective :orthographic :isometric))

(defstruct (state
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (transform nil :type ts::state)
  (window nil :type win::window)
  (view (m4:id) :type m4:mat)
  (projection (m4:id) :type m4:mat))

(u:fn-> %update-projection/orthographic (state u:f32 u:f32 u:f32) null)
(declaim (inline %update-projection/orthographic))
(defun %update-projection/orthographic (state zoom near far)
  (declare (optimize speed))
  (let* ((window (window state))
         (w (/ (win::width window) zoom 2.0))
         (h (/ (win::height window) zoom 2.0)))
    (m4:ortho! (projection state) (- w) w (- h) h near far)
    nil))

(u:fn-> %update-projection/perspective (state u:f32 u:f32 u:f32 u:f32) null)
(declaim (inline %update-projection/perspective))
(defun %update-projection/perspective (state fov-y zoom near far)
  (declare (optimize speed))
  (let ((window (window state)))
    (m4:perspective! (projection state)
                     (/ fov-y zoom)
                     (/ (float (win::width window) 1f0)
                        (float (win::height window) 1f0))
                     near
                     far)
    nil))

(u:fn-> %update-projection/isometric (state u:f32 u:f32 u:f32) null)
(declaim (inline %update-projection/isometric))
(defun %update-projection/isometric (state zoom near far)
  (declare (optimize speed))
  (let ((transform (transform state))
        (rotation #.(q:inverse
                     (q:rotate-euler
                      q:+id+
                      (v3:vec (- (asin (/ (sqrt 3)))) 0.0 const::+pi/4+)))))
    (%update-projection/orthographic state zoom near far)
    (ts::initialize-rotation transform rotation)
    nil))

(u:fn-> update-projection (state mode u:f32 u:f32 u:f32 u:f32) null)
(defun update-projection (state mode fov-y zoom near far)
  (declare (optimize speed))
  (ecase mode
    (:orthographic (%update-projection/orthographic state zoom near far))
    (:perspective (%update-projection/perspective state fov-y zoom near far))
    (:isometric (%update-projection/isometric state zoom near far))))

(u:fn-> update-view (state boolean) null)
(defun update-view (state translate-p)
  (declare (optimize speed))
  (let* ((world-matrix (m4:copy (ts::world-matrix (transform state))))
         (view (view state))
         (eye (m4:get-translation world-matrix))
         (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 world-matrix :z))))
         (up (m4:rotation-axis-to-vec3 world-matrix :y)))
    (m4:look-at! view eye target up)
    (unless translate-p
      (m4:set-translation! view view v3:+zero+))
    nil))
