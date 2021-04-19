(in-package #:zed.trait.sprite)

(z::define-internal-trait sprite ()
  ((%name :reader name
          :inline t
          :type string
          :initarg :name
          :initform "")
   (%asset :reader asset
           :inline t
           :type list
           :initarg :asset
           :initform nil)
   (%frames :reader frames
            :inline t
            :type u:ub16
            :initarg :frames
            :initform 1)
   (%duration :reader duration
              :inline t
              :type u:f32
              :initarg :duration
              :initform 1.0)
   (%repeat-p :reader repeat-p
              :inline t
              :type boolean
              :initarg :repeat-p
              :initform t)
   (%instances :reader instances
               :inline t
               :type (and (integer 1) fixnum)
               :initarg :instances
               :initform 1)
   (%buffer-spec :reader buffer-spec
                 :inline t
                 :type list
                 :initarg :buffer-spec
                 :initform '(:spritesheet zsl:sprite))
   (%spritesheet :accessor spritesheet
                 :inline t
                 :type z::spritesheet
                 :initform nil)
   (%index :accessor index
           :inline t
           :type u:ub16
           :initform 0)
   (%initial-index :accessor initial-index
                   :inline t
                   :type u:ub16
                   :initform 0)
   (%elapsed :accessor elapsed
             :inline t
             :type u:f32
             :initform 0.0)
   (%pause-p :accessor pause-p
             :inline t
             :type boolean
             :initform nil))
  (:setup setup)
  (:update update)
  (:render render))

;;; Hooks

(u:fn-> setup (sprite) null)
(defun setup (sprite)
  (declare (optimize speed))
  (let* ((core (z:trait-core sprite))
         (spritesheet (z::make-spritesheet core (asset sprite) (buffer-spec sprite))))
    (setf (spritesheet sprite) spritesheet
          (index sprite) (z::find-sprite spritesheet (name sprite))
          (initial-index sprite) (index sprite))
    nil))

(u:fn-> update (sprite) null)
(defun update (sprite)
  (declare (optimize speed))
  (unless (pause-p sprite)
    (let* ((duration (duration sprite))
           (clock (z::core-clock (z:trait-core sprite))))
      (incf (elapsed sprite) (z::get-frame-time clock))
      (if (>= (elapsed sprite) duration)
          (setf (elapsed sprite) 0.0
                (pause-p sprite) (unless (repeat-p sprite) t))
          (let* ((step (/ (elapsed sprite) duration))
                 (min (float (initial-index sprite) 1d0))
                 (max (+ min (frames sprite)))
                 (index (floor (u:clamp (u:lerp step min max) (1- min) max))))
            (setf (index sprite) index)))
      nil)))

(u:fn-> render (sprite) null)
(defun render (sprite)
  (declare (optimize speed))
  (let* ((asset (asset sprite))
         (shader-manager (z::core-shader-manager (z:trait-core sprite)))
         (render-trait (z:find-trait (z:trait-owner sprite) 'tr.ren:render))
         (material (tr.ren::material render-trait)))
    (z::set-uniform material :sprite.index (index sprite))
    (z::with-shader-buffers (shader-manager asset)
      (gl:bind-vertex-array (z::spritesheet-vao (spritesheet sprite)))
      (gl:draw-arrays-instanced :triangle-strip 0 4 (instances sprite))
      (gl:bind-vertex-array 0))
    nil))
