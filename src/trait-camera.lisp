(in-package #:cl-user)

(defpackage #:zed.trait.camera
  ;; Third-party aliases
  (:local-nicknames
   (#:const #:origin.constants)
   (#:m3 #:origin.mat3)
   (#:m4 #:origin.mat4)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3))
  ;; Internal aliases
  (:local-nicknames
   (#:cam.state #:%zed.camera-state)
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:trait #:%zed.trait)
   (#:ts #:%zed.transform-state))
  (:use #:cl)
  (:export
   #:camera
   #:resolve-normal-matrix))

(in-package #:zed.trait.camera)

(trait::define-internal-trait camera ()
  ((%state :accessor state
           :inline t
           :type cam.state::state
           :initform nil)
   (%mode :reader mode
          :inline t
          :type cam.state::mode
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
                      :initform t))
  (:setup setup)
  (:attach attach)
  (:update update))

(defun make-active (camera)
  (let ((context (trait::context camera)))
    (setf (ctx::active-camera context) camera)))

(u:fn-> resolve-normal-matrix (ctx::context gob::game-object) m3:mat)
(defun resolve-normal-matrix (context game-object)
  (declare (optimize speed))
  (let* ((transform-state (gob::transform game-object))
         (normal-matrix (ts::normal-matrix transform-state)))
    (u:when-let* ((camera (ctx::active-camera context))
                  (state (state camera)))
      (m4:set-translation! normal-matrix (ts::world-matrix transform-state) v3:+zero+)
      (m4:*! normal-matrix (cam.state::view state) normal-matrix)
      (m4:invert! normal-matrix normal-matrix)
      (m4:transpose! normal-matrix normal-matrix))
    (m4:rotation-to-mat3 normal-matrix)))

;;; Hooks

(defun setup (camera)
  (setf (fov-y camera) (* (fov-y camera) const:+deg+))
  (unless (ctx::active-camera (trait::context camera))
    (make-active camera)))

(defun attach (camera)
  (let* ((context (trait::context camera))
         (owner (trait::owner camera))
         (window (ctx::window context))
         (transform (gob::transform owner)))
    (setf (state camera) (cam.state::make-state :transform transform :window window))))

(defun update (camera)
  (let ((state (state camera)))
    (cam.state::update-view state (translate-view-p camera))
    (cam.state::update-projection state
                                  (mode camera)
                                  (fov-y camera)
                                  (zoom camera)
                                  (clip-near camera)
                                  (clip-far camera))))
