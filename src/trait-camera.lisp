(in-package #:cl-user)

(defpackage #:%zed.trait.camera
  ;; Third-party aliases
  (:local-nicknames
   (#:const #:origin.constants)
   (#:m4 #:origin.mat4)
   (#:q #:origin.quat)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:ts #:%zed.transform-state)
   (#:trait #:%zed.trait)
   (#:win #:%zed.window))
  (:use #:cl))

(in-package #:%zed.trait.camera)

(deftype mode () '(member :perspective :orthographic :isometric))

(trait::define-internal-trait camera ()
  ((%mode :reader mode
          :inline t
          :type mode
          :initarg :mode
          :initform :perspective)
   (%clip-near :reader clip-near
               :inline t
               :type u:f32
               :initarg :clip-near
               :initform 0.1)
   (%clip-far :reader clip-far
              :inline t
              :type u:f32
              :initarg :clip-far
              :initform 1024.0)
   (%fov-y :accessor fov-y
           :inline t
           :type u:f32
           :initarg :fov-y
           :initform 45.0)
   (%zoom :reader zoom
          :inline t
          :type u:f32
          :initarg :zoom
          :initform 1.0)
   (%translate-view-p :reader translate-view-p
                      :inline t
                      :type boolean
                      :initarg :translate-view-p
                      :initform t)
   (%view :reader view
          :inline t
          :type m4:mat
          :initform (m4:mat 1))
   (%projection :reader projection
                :inline t
                :type m4:mat
                :initform (m4:mat 1)))
  (:setup-hook setup)
  (:update-hook update))

(defun %update-projection/orthographic (camera)
  (let* ((context (trait::context camera))
         (window (ctx::window context))
         (zoom (zoom camera))
         (w (/ (win::width window) zoom 2.0))
         (h (/ (win::height window) zoom 2.0)))
    (m4:ortho! (projection camera) (- w) w (- h) h (clip-near camera) (clip-far camera))))

(defun %update-projection/perspective (camera)
  (let* ((context (trait::context camera))
         (window (ctx::window context)))
    (m4:perspective! (projection camera)
                     (/ (fov-y camera) (zoom camera))
                     (/ (win::width window) (win::height window) 1.0)
                     (clip-near camera)
                     (clip-far camera))))

(defun %update-projection/isometric (camera)
  (let ((transform (gob::transform (trait::owner camera)))
        (rotation (q:inverse
                   (q:rotate-euler
                    q:+id+
                    (v3:vec #.(- (asin (/ (sqrt 3)))) 0 const::pi/4)))))
    (%update-projection/orthographic camera)
    (ts::initialize-rotation transform rotation)))

(defun update-projection (camera)
  (ecase (mode camera)
    (:orthographic (%update-projection/orthographic camera))
    (:perspective (%update-projection/perspective camera))
    (:isometric (%update-projection/isometric camera))))

(defun update-view (camera)
  (let* ((transform (gob::transform (trait::owner camera)))
         (world-matrix (m4:copy (ts::world-matrix transform)))
         (view (view camera))
         (eye (m4:get-translation world-matrix))
         (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 world-matrix :z))))
         (up (m4:rotation-axis-to-vec3 world-matrix :y)))
    (m4:look-at! view eye target up)
    (unless (translate-view-p camera)
      (m4:set-translation! view view v3:+zero+))))

;;; Hooks

(defun setup (camera)
  (setf (fov-y camera) (* (fov-y camera) const:+deg+)))

(defun update (camera)
  (update-view camera)
  (update-projection camera))
