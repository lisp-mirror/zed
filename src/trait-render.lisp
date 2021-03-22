(in-package #:cl-user)

(defpackage #:zed.trait.render
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:cam #:%zed.camera-state)
   (#:ctx #:%zed.context)
   (#:dbg #:%zed.debug)
   (#:do #:%zed.draw-order)
   (#:gob #:%zed.game-object)
   (#:mat #:%zed.material)
   (#:mat.data #:%zed.material.data)
   (#:mat.def #:%zed.material.definition)
   (#:trait #:%zed.trait)
   (#:ts #:%zed.transform-state)
   (#:tree #:%zed.tree)
   (#:tr.cam #:zed.trait.camera))
  (:use #:cl)
  (:export
   #:render))

(in-package #:zed.trait.render)

(trait::define-internal-trait render ()
  ((%material-name :reader material-name
                   :inline t
                   :type symbol
                   :initarg :material
                   :initform nil)
   (%material :accessor material
              :inline t
              :type (or mat.def::material null)
              :initform nil)
   (%layer :reader layer
           :inline t
           :type u:b16
           :initarg :layer
           :initform 0))
  (:setup-hook setup)
  (:attach-hook attach)
  (:detach-hook detach)
  (:pre-render-hook pre-render))

(u:fn-> draw-order-tree-sort (render render) boolean)
(defun draw-order-tree-sort (x y)
  (declare (optimize speed))
  (< (+ (ash (layer x) 16) (gob::depth (trait::owner x)))
     (+ (ash (layer y) 16) (gob::depth (trait::owner y)))))

(u:fn-> render-game-object (render) null)
(defun render-game-object (render-trait)
  (declare (optimize speed))
  (let* ((context (trait::context render-trait))
         (owner (trait::owner render-trait))
         (material (material render-trait))
         (func (mat.data::render-func (mat.def::data material))))
    (dbg::with-debug-group (format nil "Game Object: ~a" (gob::label owner))
      (funcall func context owner material))
    nil))

(u:fn-> render-frame (ctx::context) null)
(defun render-frame (context)
  (declare (optimize speed))
  ;; NOTE: This `when-let` is required: The tree is not created until the first render trait is
  ;; created, but the game loop calls this function unconditionally each frame.
  (u:when-let ((draw-order (ctx::draw-order context)))
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer :depth-buffer)
    (do::map draw-order #'render-game-object)))

;;; Hooks

(u:fn-> setup (render) null)
(defun setup (render)
  (declare (optimize speed))
  (let ((context (trait::context render)))
    (unless (ctx::draw-order context)
      (setf (ctx::draw-order context) (do::make-manager #'draw-order-tree-sort)))
    nil))

(u:fn-> attach (render) null)
(defun attach (render)
  (declare (optimize speed))
  (u:if-let ((material-name (material-name render)))
    (let ((context (trait::context render)))
      (setf (material render) (mat::ensure-material context material-name))
      (do::register context render)
      nil)
    (error "Render trait must have a material specified.")))

(u:fn-> detach (render) null)
(defun detach (render)
  (declare (optimize speed))
  (let ((context (trait::context render)))
    (do::deregister context render)
    nil))

(u:fn-> pre-render (render) null)
(defun pre-render (render)
  (declare (optimize speed))
  (u:when-let* ((context (trait::context render))
                (owner (trait::owner render))
                (camera (ctx::active-camera context))
                (camera-state (tr.cam::state camera))
                (world-matrix (ts::world-matrix (gob::transform owner)))
                (material (material render)))
    (mat::set-uniform material :model world-matrix)
    (mat::set-uniform material :view (cam::view camera-state))
    (mat::set-uniform material :proj (cam::projection camera-state))
    nil))
