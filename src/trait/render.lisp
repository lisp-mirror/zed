(in-package #:zed.trait.render)

(z::define-internal-trait render ()
  ((%material-name :reader material-name
                   :inline t
                   :type symbol
                   :initarg :material
                   :initform nil)
   (%material :accessor material
              :inline t
              :type (or z::material null)
              :initform nil)
   (%layer :reader layer
           :inline t
           :type u:b16
           :initarg :layer
           :initform 0)
   (%culled-p :accessor culled-p
              :inline t
              :type boolean
              :initform nil)
   (%viewport-name :reader viewport-name
                   :inline t
                   :type symbol
                   :initarg :viewport-name
                   :initform :default)
   (%viewport :accessor viewport
              :inline t
              :type z::viewport
              :initform nil))
  (:setup setup)
  (:attach attach)
  (:detach detach)
  (:render render))

(u:fn-> draw-order-tree-sort (render render) boolean)
(defun draw-order-tree-sort (x y)
  (declare (optimize speed))
  (< (+ (ash (layer x) 16) (z::game-object-depth (z:trait-owner x)))
     (+ (ash (layer y) 16) (z::game-object-depth (z:trait-owner y)))))

(u:fn-> render-game-object (render) null)
(defun render-game-object (render-trait)
  (declare (optimize speed))
  (let* ((core (z:trait-core render-trait))
         (owner (z:trait-owner render-trait))
         (material (material render-trait))
         (func (z::material-data-render-func (z::material-data material))))
    (z::configure-viewport (viewport render-trait))
    (z::with-debug-group (format nil "Game Object: ~a" (z::game-object-label owner))
      (funcall func core owner material))
    nil))

(u:fn-> render-frame (z::core) null)
(defun render-frame (core)
  (declare (optimize speed))
  (let ((draw-order (z::core-draw-order core)))
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer :depth-buffer)
    (u:when-let ((camera (z::core-active-camera core)))
      (z::write-shader-buffer :camera :path :view :value (list (tr.cam::view camera)))
      (z::write-shader-buffer :camera :path :proj :value (list (tr.cam::projection camera))))
    (z::with-time-buffer (core :render-phase)
      (z::map-draw-order draw-order
                         (lambda (x)
                           (unless (culled-p x)
                             (render-game-object x)))))))

;;; Hooks

(u:fn-> setup (render) null)
(defun setup (render)
  (declare (optimize speed))
  (let ((viewport-manager (z::core-viewports (z:trait-core render))))
    (setf (viewport render) (z::ensure-viewport viewport-manager (viewport-name render)))
    nil))

(u:fn-> attach (render) null)
(defun attach (render)
  (declare (optimize speed))
  (u:if-let ((material-name (material-name render)))
    (let ((core (z:trait-core render)))
      (setf (material render) (z::make-material core material-name))
      (z::register-draw-order core render)
      nil)
    (error "Render trait must have a material specified.")))

(u:fn-> detach (render) null)
(defun detach (render)
  (declare (optimize speed))
  (let ((core (z:trait-core render)))
    (z::destroy-material core (material render))
    (z::deregister-draw-order core render)
    nil))

(u:fn-> render (render) null)
(defun render (render)
  (declare (optimize speed))
  (let ((owner (z:trait-owner render))
        (material (material render)))
    (z::set-uniform material :model (z::get-transform owner :space :world))
    nil))
