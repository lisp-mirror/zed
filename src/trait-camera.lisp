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
   (#:trs #:%zed.transform-state)
   (#:trait #:%zed.trait)
   (#:win #:%zed.window))
  (:use #:cl))

(in-package #:%zed.trait.camera)

(trait::define-trait camera ()
  ((%mode :reader mode
          :initarg :mode
          :initform :perspective)
   (%clip-near :reader clip-near
               :initarg :clip-near
               :initform 0.1)
   (%clip-far :reader clip-far
              :initarg :clip-far
              :initform 1024.0)
   (%fov-y :reader fov-y
           :initarg :fov-y
           :initform 45.0)
   (%zoom :reader zoom
          :initarg :zoom
          :initform 1.0)
   (%translate-view-p :reader translate-view-p
                      :initarg :translate-view-p
                      :initform t)
   (%view :reader view
          :initform (m4:mat 1))
   (%projection :reader projection
                :initform (m4:mat 1))))

(defun %make-projection/orthographic (camera)
  (let* ((context (trait::context camera))
         (window (ctx::window context))
         (zoom (zoom camera))
         (w (/ (win::width window) zoom 2.0))
         (h (/ (win::height window) zoom 2.0)))
    (m4:ortho! (projection camera) (- w) w (- h) h (clip-near camera) (clip-far camera))))

(defun %make-projection/perspective (camera)
  (let* ((context (trait::context camera))
         (window (ctx::window context)))
    (m4:perspective! (projection camera)
                     (/ (fov-y camera) (zoom camera))
                     (/ (win::width window) (win::height window) 1.0)
                     (clip-near camera)
                     (clip-far camera))))

(defun %make-projection/isometric (camera)
  (let ((transform (gob::transform (trait::owner camera)))
        (rotation (q:inverse
                   (q:rotate-euler
                    q:+id+
                    (v3:vec #.(- (asin (/ (sqrt 3)))) 0 const::pi/4)))))
    (%make-projection/orthographic camera)
    (trs::initialize-rotation transform rotation)))

(defun make-projection (camera)
  (ecase (mode camera)
    (:orthographic (%make-projection/orthographic camera))
    (:perspective (%make-projection/perspective camera))
    (:isometric (%make-projection/isometric camera))))

(defun make-view (camera)
  (let* ((transform (gob::transform (trait::owner camera)))
         (world-matrix (m4:copy (trs::world-matrix transform)))
         (view (view camera))
         (eye (m4:get-translation world-matrix))
         (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 world-matrix :z))))
         (up (m4:rotation-axis-to-vec3 world-matrix :y)))
    (m4:look-at! view eye target up)
    (unless (translate-view-p camera)
      (m4:set-translation! view view v3:+zero+))))
