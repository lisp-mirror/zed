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
  (< (+ (ash (layer x) 16) (z::game-object-depth (z::trait-owner x)))
     (+ (ash (layer y) 16) (z::game-object-depth (z::trait-owner y)))))

(u:fn-> render-game-object (render) null)
(defun render-game-object (render-trait)
  (declare (optimize speed))
  (let* ((context (z:trait-context render-trait))
         (owner (z::trait-owner render-trait))
         (material (material render-trait))
         (func (z::material-data-render-func (z::material-data material))))
    (z::configure-viewport (viewport render-trait))
    (z::with-debug-group (format nil "Game Object: ~a" (z::game-object-label owner))
      (funcall func context owner material))
    nil))

(u:fn-> render-frame (z::context) null)
(defun render-frame (context)
  (declare (optimize speed))
  ;; NOTE: This `when-let` is required: The tree is not created until the first render trait is
  ;; created, but the game loop calls this function unconditionally each frame.
  (u:when-let ((draw-order (z::context-draw-order context)))
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer :depth-buffer)
    (z::map-draw-order draw-order #'render-game-object)))

;;; Hooks

(u:fn-> setup (render) null)
(defun setup (render)
  (declare (optimize speed))
  (let ((context (z:trait-context render)))
    (unless (z::context-draw-order context)
      (setf (z::context-draw-order context) (z::make-draw-order-manager #'draw-order-tree-sort)))
    (setf (viewport render) (z::ensure-viewport (z::context-viewports context)
                                                (viewport-name render)))
    nil))

(u:fn-> attach (render) null)
(defun attach (render)
  (declare (optimize speed))
  (u:if-let ((material-name (material-name render)))
    (let ((context (z:trait-context render)))
      (setf (material render) (z::ensure-material context material-name))
      (z::register-draw-order context render)
      nil)
    (error "Render trait must have a material specified.")))

(u:fn-> detach (render) null)
(defun detach (render)
  (declare (optimize speed))
  (let ((context (z:trait-context render)))
    (z::deregister-draw-order context render)
    nil))

(u:fn-> render (render) null)
(defun render (render)
  (declare (optimize speed))
  (u:when-let* ((context (z:trait-context render))
                (owner (z::trait-owner render))
                (camera (z::context-active-camera context))
                (material (material render)))
    (z::set-uniform material :model (z::get-transform owner :space :world))
    (z::set-uniform material :view (tr.cam::view camera))
    (z::set-uniform material :proj (tr.cam::projection camera))
    nil))
