(in-package #:cl-user)

(defpackage #:zed.trait.sprite
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:clock #:%zed.clock)
   (#:ctx #:%zed.context)
   (#:mat #:%zed.material)
   (#:mat.def #:%zed.material.definition)
   (#:sbs #:%zed.shader-buffer-state)
   (#:ss #:%zed.spritesheet)
   (#:tr.ren #:zed.trait.render)
   (#:trait #:%zed.trait)
   (#:uni #:%zed.material.uniform))
  (:use #:cl)
  (:export
   #:sprite))

(in-package #:zed.trait.sprite)

(trait::define-internal-trait sprite ()
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
                 :initform '(:spritesheet umbra.sprite:sprite))
   (%spritesheet :accessor spritesheet
                 :inline t
                 :type ss::spritesheet
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
  (:setup-hook setup)
  (:update-hook update)
  (:pre-render-hook pre-render)
  (:render-hook render))

;;; Hooks

(u:fn-> setup (sprite) null)
(defun setup (sprite)
  (declare (optimize speed))
  (let* ((context (trait::context sprite))
         (spritesheet (ss::make-spritesheet context (asset sprite) (buffer-spec sprite))))
    (setf (spritesheet sprite) spritesheet
          (index sprite) (ss::find spritesheet (name sprite))
          (initial-index sprite) (index sprite))
    nil))

(u:fn-> update (sprite) null)
(defun update (sprite)
  (declare (optimize speed))
  (unless (pause-p sprite)
    (let* ((duration (duration sprite))
           (clock (ctx::clock (trait::context sprite))))
      (incf (elapsed sprite) (float (clock::frame-time clock) 1f0))
      (if (>= (elapsed sprite) duration)
          (setf (elapsed sprite) 0.0
                (pause-p sprite) (unless (repeat-p sprite) t))
          (let* ((step (/ (elapsed sprite) duration))
                 (min (float (initial-index sprite) 1d0))
                 (max (+ min (frames sprite)))
                 (index (floor (u:clamp (u:lerp step min max) (1- min) max))))
            (setf (index sprite) index)))
      nil)))

(defun pre-render (sprite)
  (let* ((render-trait (trait::find-trait (trait::owner sprite) 'tr.ren::render))
         (material (tr.ren::material render-trait)))
    (mat::set-uniforms material :sprite.index (index sprite))))

(u:fn-> render (sprite) null)
(defun render (sprite)
  (declare (optimize speed))
  (let ((asset (asset sprite))
        (buffer-state (ctx::shader-buffer-state (trait::context sprite))))
    (sbs::with-buffers (buffer-state asset)
      (gl:bind-vertex-array (ss::vao (spritesheet sprite)))
      (gl:draw-arrays-instanced :triangle-strip 0 4 (instances sprite))
      (gl:bind-vertex-array 0))
    nil))
