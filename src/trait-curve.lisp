(in-package #:zed.trait.curve)

(z::define-internal-trait curve ()
  ((%name :reader name
          :inline t
          :type symbol
          :initarg :name
          :initform nil)
   (%divisions :reader divisions
               :inline t
               :type u:positive-fixnum
               :initarg :divisions
               :initform 100)
   (%even-spacing-p :reader even-spacing-p
                    :inline t
                    :type boolean
                    :initarg :even-spacing-p
                    :initform nil)
   (%visible-p :reader visible-p
               :inline t
               :type boolean
               :initarg :visible-p
               :initform nil)
   (%flip :reader flip
          :inline t
          :type (member nil :x :y)
          :initarg :flip
          :initform nil)
   (%segments :accessor segments
              :inline t
              :type list
              :initarg :segments
              :initform nil)
   (%data :accessor data
          :inline t
          :type (or curve:curve null)
          :initarg :data
          :initform nil))
  (:attach attach)
  (:update update))

(z:define-material curve ()
  (:shader zsl:default
   :uniforms (:color (v3:vec 0 0.4 0.7)
              :opacity 1)
   :features (:line-width 3
              :depth-mode :lequal)))

(z:define-geometry-layout point ()
  (:data (:format :interleaved)
         (position :type float :count 3)))

(z:define-geometry line-segments ()
  (:layout point
   :vertex-count 2
   :primitive :lines))

(defun evaluate (curve parameter)
  (let ((game-object (z:trait-owner curve)))
    (v3:+ (v3:* (curve:evaluate (data curve) parameter :even-spacing-p (even-spacing-p curve))
                (z:get-scale game-object))
          (z:get-translation game-object))))

(defun rescale (curve &key viewport padding)
  (let* ((core (z:trait-core curve))
         (game-object (z:trait-owner curve))
         (viewport-size (z::get-viewport-size core viewport)))
    (v2:with-components ((v (v2:+ viewport-size (or padding (v2:zero)))))
      (z:scale game-object (v3:vec vx vy 0) :replace-p t))))

(defun flip-points (curve)
  (let* ((data (u:href z::=curves= (name curve)))
         (axis (flip curve))
         (invert (case axis
                   (:x (v3:vec -1 1 1))
                   (:y (v3:vec 1 -1 1))
                   (t (v3:uniform 1.0)))))
    (mapcar
     (lambda (x)
       (v3:* x invert))
     (if (eq axis :y)
         (reverse (z::curve-data-points data))
         (z::curve-data-points data)))))

(defun make-data (curve)
  (u:if-let ((name (name curve)))
    (u:if-let ((data (u:href z::=curves= name)))
      (let* ((data (curve:make-curve (flip-points curve)
                                     :divisions (divisions curve)))
             (even-spacing-p (even-spacing-p curve))
             (segments (curve:collect-segments data
                                               (divisions curve)
                                               :even-spacing-p even-spacing-p)))
        (setf (data curve) data
              (segments curve) segments))
      (error "Curve ~s not defined." name))
    (error "Curve name not specified for trait: ~s." curve)))

(defun make-segments (curve)
  (u:when-let ((data (data curve)))
    (curve:collect-segments data (divisions curve) :even-spacing-p (even-spacing-p curve))))

(defun initialize-visualization (curve)
  (when (visible-p curve)
    (let ((core (z:trait-core curve))
          (game-object (z:trait-owner curve)))
      (when (or (z:find-trait game-object 'tr.geo:geometry)
                (z:find-trait game-object 'tr.ren:render))
        (error "Curve is ~s is marked as visible, but its game object must not have a geometry or ~
                render trait attached."
               curve))
      (let ((geometry (z:make-trait core 'tr.geo:geometry :name 'line-segment))
            (render (z:make-trait core 'tr.ren:render :material 'curve)))
        (z:attach-trait game-object geometry)
        (z:attach-trait game-object render)))))

;;; entity hooks

(u:fn-> attach (curve) null)
(defun attach (curve)
  (declare (optimize speed))
  (make-data curve)
  (initialize-visualization curve)
  nil)

(u:fn-> update (curve) null)
(defun update (curve)
  (declare (optimize speed))
  (when (visible-p curve)
    (let* ((game-object (z:trait-owner curve))
           (geometry (z:find-trait game-object 'tr.geo:geometry)))
      (z::update-geometry (tr.geo::resource geometry) :data (segments curve))
      nil)))
