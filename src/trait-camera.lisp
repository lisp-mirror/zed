(in-package #:zed.trait.camera)

(z::define-internal-trait camera ()
  ((%state :accessor state
           :inline t
           :type z::camera-state
           :initform nil)
   (%mode :reader mode
          :inline t
          :type z::camera-mode
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
  (let ((context (z:trait-context camera)))
    (setf (z::context-active-camera context) camera)))

(u:fn-> resolve-normal-matrix (z::context z:game-object) m3:mat)
(defun resolve-normal-matrix (context game-object)
  (declare (optimize speed))
  (let* ((transform-state (z::game-object-transform game-object))
         (normal-matrix (z::transform-state-normal-matrix transform-state)))
    (u:when-let* ((camera (z::context-active-camera context))
                  (state (state camera)))
      (m4:set-translation! normal-matrix
                           (z::transform-state-world-matrix transform-state)
                           v3:+zero+)
      (m4:*! normal-matrix (z::camera-state-view state) normal-matrix)
      (m4:invert! normal-matrix normal-matrix)
      (m4:transpose! normal-matrix normal-matrix))
    (m4:rotation-to-mat3 normal-matrix)))

;;; Hooks

(defun setup (camera)
  (setf (fov-y camera) (* (fov-y camera) const:+deg+))
  (unless (z::context-active-camera (z:trait-context camera))
    (make-active camera)))

(defun attach (camera)
  (let* ((context (z:trait-context camera))
         (owner (z::trait-owner camera))
         (window (z::context-window context))
         (transform (z::game-object-transform owner)))
    (setf (state camera) (z::make-camera-state :transform transform :window window))))

(defun update (camera)
  (let ((state (state camera)))
    (z::update-camera-view state (translate-view-p camera))
    (z::update-camera-projection state
                                 (mode camera)
                                 (fov-y camera)
                                 (zoom camera)
                                 (clip-near camera)
                                 (clip-far camera))))
