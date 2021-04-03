(in-package #:zed)

(deftype camera-mode () '(member :perspective :orthographic :isometric))

(defstruct (camera-state
            (:predicate nil)
            (:copier nil))
  (transform nil :type transform-state)
  (window nil :type window)
  (view (m4:id) :type m4:mat)
  (projection (m4:id) :type m4:mat))

(u:fn-> update-camera-projection/orthographic (camera-state u:f32 u:f32 u:f32) null)
(declaim (inline update-camera-projection/orthographic))
(defun update-camera-projection/orthographic (state zoom near far)
  (declare (optimize speed))
  (let* ((window (camera-state-window state))
         (w (/ (window-width window) zoom 2.0))
         (h (/ (window-height window) zoom 2.0)))
    (m4:ortho! (camera-state-projection state) (- w) w (- h) h near far)
    nil))

(u:fn-> update-camera-projection/perspective (camera-state u:f32 u:f32 u:f32 u:f32) null)
(declaim (inline update-camera-projection/perspective))
(defun update-camera-projection/perspective (state fov-y zoom near far)
  (declare (optimize speed))
  (let ((window (camera-state-window state)))
    (m4:perspective! (camera-state-projection state)
                     (/ fov-y zoom)
                     (/ (float (window-width window) 1f0)
                        (float (window-height window) 1f0))
                     near
                     far)
    nil))

(u:fn-> update-camera-projection/isometric (camera-state u:f32 u:f32 u:f32) null)
(declaim (inline update-camera-projection/isometric))
(defun update-camera-projection/isometric (state zoom near far)
  (declare (optimize speed))
  (let ((transform (camera-state-transform state))
        (rotation #.(q:inverse
                     (q:rotate-euler
                      q:+id+
                      (v3:vec (- (asin (/ (sqrt 3)))) 0.0 const::+pi/4+)))))
    (update-camera-projection/orthographic state zoom near far)
    (initialize-rotate-state transform rotation)
    nil))

(u:fn-> update-camera-projection (camera-state camera-mode u:f32 u:f32 u:f32 u:f32) null)
(defun update-camera-projection (state mode fov-y zoom near far)
  (declare (optimize speed))
  (ecase mode
    (:orthographic (update-camera-projection/orthographic state zoom near far))
    (:perspective (update-camera-projection/perspective state fov-y zoom near far))
    (:isometric (update-camera-projection/isometric state zoom near far))))

(u:fn-> update-camera-view (camera-state boolean) null)
(defun update-camera-view (state translate-p)
  (declare (optimize speed))
  (let* ((world-matrix (m4:copy (transform-state-world-matrix (camera-state-transform state))))
         (view (camera-state-view state))
         (eye (m4:get-translation world-matrix))
         (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 world-matrix :z))))
         (up (m4:rotation-axis-to-vec3 world-matrix :y)))
    (m4:look-at! view eye target up)
    (unless translate-p
      (m4:set-translation! view view v3:+zero+))
    nil))
