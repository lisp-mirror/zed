(in-package #:cl-user)

(defpackage #:zed.trait.camera
  ;; Third-party aliases
  (:local-nicknames
   (#:const #:origin.constants)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:cam.state #:%zed.camera-state)
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:trait #:%zed.trait))
  (:use #:cl)
  (:export
   #:camera))

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
  (:setup-hook setup)
  (:attach-hook attach)
  (:update-hook update))

(defun make-active (camera)
  (let ((context (trait::context camera)))
    (setf (ctx::active-camera context) camera)))

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
