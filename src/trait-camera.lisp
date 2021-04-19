(in-package #:zed.trait.camera)

(z::define-internal-trait camera ()
  ((%mode :reader mode
          :inline t
          :type (member :perspective :orthographic :isometric)
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
   (%free-look-p :reader free-look-p
                 :inline t
                 :type boolean
                 :initarg :free-look-p
                 :initform nil)
   (%free-look-state :accessor free-look-state
                     :inline t
                     :type (or z::free-look-state null)
                     :initform nil)
   (%translate-view-p :reader translate-view-p
                      :inline t
                      :type boolean
                      :initarg :translate-view-p
                      :initform t)
   (%view :reader view
          :inline t
          :type m4:mat
          :initform (m4:id))
   (%projection :reader projection
                :inline t
                :type m4:mat
                :initform (m4:id))
   (%frustum :reader frustum
             :inline t
             :type frustum:frustum
             :initform (frustum:make-frustum)))
  (:setup setup)
  (:attach attach)
  (:update update))

(u:fn-> update-projection/orthographic (camera) null)
(declaim (inline update-projection/orthographic))
(defun update-projection/orthographic (camera)
  (declare (optimize speed))
  (let* ((core (z:trait-core camera))
         (window (z::core-window core))
         (zoom (zoom camera))
         (w (/ (z::window-width window) zoom 2.0))
         (h (/ (z::window-height window) zoom 2.0)))
    (m4:ortho! (projection camera) (- w) w (- h) h (clip-near camera) (clip-far camera))
    nil))

(u:fn-> update-projection/perspective (camera) null)
(declaim (inline update-projection/perspective))
(defun update-projection/perspective (camera)
  (declare (optimize speed))
  (let* ((core (z:trait-core camera))
         (window (z::core-window core)))
    (m4:perspective! (projection camera)
                     (/ (fov-y camera) (zoom camera))
                     (/ (float (z::window-width window) 1f0)
                        (float (z::window-height window) 1f0))
                     (clip-near camera)
                     (clip-far camera))
    nil))

(u:fn-> update-projection/isometric (camera) null)
(declaim (inline update-projection/isometric))
(defun update-projection/isometric (camera)
  (declare (optimize speed))
  (let* ((owner (z:trait-owner camera))
         (transform (z::game-object-transform owner))
         (rotation #.(q:inverse
                      (q:rotate-euler
                       q:+id+
                       (v3:vec (- (asin (/ (sqrt 3)))) 0 const::+pi/4+)))))
    (update-projection/orthographic camera)
    (z::initialize-rotate-state transform rotation)
    nil))

(u:fn-> update-projection (camera) null)
(defun update-projection (camera)
  (declare (optimize speed))
  (ecase (mode camera)
    (:orthographic (update-projection/orthographic camera))
    (:perspective (update-projection/perspective camera))
    (:isometric (update-projection/isometric camera))))

(u:fn-> update-view (camera) null)
(defun update-view (camera)
  (declare (optimize speed))
  (let* ((world-matrix (m4:copy (z::get-transform (z:trait-owner camera) :space :world)))
         (view (view camera))
         (eye (m4:get-translation world-matrix))
         (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 world-matrix :z))))
         (up (m4:rotation-axis-to-vec3 world-matrix :y)))
    (declare (dynamic-extent eye target up))
    (m4:look-at! view eye target up)
    (unless (translate-view-p camera)
      (m4:set-translation! view view v3:+zero+))
    (u:when-let ((free-look-state (free-look-state camera)))
      (z::set-initial-free-look-orientation free-look-state world-matrix))
    nil))

(defun make-active (camera)
  (let ((core (z:trait-core camera)))
    (setf (z::core-active-camera core) camera)))

(u:fn-> resolve-normal-matrix (z::core z:game-object) m3:mat)
(defun resolve-normal-matrix (core game-object)
  (declare (optimize speed))
  (let* ((transform-state (z::game-object-transform game-object))
         (normal-matrix (load-time-value (m4:id))))
    (u:when-let ((camera (z::core-active-camera core)))
      (m4:set-translation! normal-matrix
                           (z::transform-state-world-matrix transform-state)
                           v3:+zero+)
      (m4:*! normal-matrix (view camera) normal-matrix)
      (m4:invert! normal-matrix normal-matrix)
      (m4:transpose! normal-matrix normal-matrix))
    (m4:rotation-to-mat3! (z::transform-state-normal-matrix transform-state) normal-matrix)))

;;; Hooks

(u:fn-> setup (camera) null)
(defun setup (camera)
  (declare (optimize speed))
  (setf (fov-y camera) (* (fov-y camera) const:+deg+))
  (unless (z::core-active-camera (z:trait-core camera))
    (make-active camera))
  nil)

(u:fn-> attach (camera) null)
(defun attach (camera)
  (declare (optimize speed))
  (when (free-look-p camera)
    (let ((game-object (z:trait-owner camera)))
      (setf (free-look-state camera) (z::make-free-look-state game-object))
      nil)))

(u:fn-> update (camera) null)
(defun update (camera)
  (declare (optimize speed))
  (let ((core (z:trait-core camera)))
    (u:when-let ((free-look-state (free-look-state camera)))
      (z::update-free-look-state core free-look-state))
    (update-view camera)
    (update-projection camera)
    (frustum:update (frustum camera) (view camera) (projection camera))
    nil))
