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
   (#:gob #:%zed.game-object)
   (#:mat #:%zed.material)
   (#:mat.data #:%zed.material.data)
   (#:mat.def #:%zed.material.definition)
   (#:trait #:%zed.trait)
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
              :initform nil))
  (:attach-hook attach)
  (:pre-render-hook pre-render))

(u:fn-> render-game-object (gob::game-object) null)
(defun render-game-object (game-object)
  (declare (optimize speed))
  (let* ((render-trait (trait::find-trait game-object 'render))
         (material (material render-trait)))
    (dbg::with-debug-group (format nil "Game Object: ~a" (gob::label game-object))
      (funcall (mat.data::render-func (mat.def::data material)) game-object material))
    nil))

(u:fn-> render-frame (ctx::context) null)
(defun render-frame (context)
  (declare (optimize speed))
  (tree::walk-tree (x (ctx::scene-tree context))
    (render-game-object x)
    nil))

;;; Hooks

(defun attach (render)
  (u:if-let ((material-name (material-name render)))
    (let ((context (trait::context render)))
      (setf (material render) (mat::make-material context material-name)))
    (error "Render trait must have a material specified.")))

(defun pre-render (render)
  (u:when-let* ((context (trait::context render))
                (camera (ctx::active-camera context))
                (camera-state (tr.cam::state camera)))
    (mat::set-uniforms (material render)
                       :view (cam::view camera-state)
                       :proj (cam::projection camera-state))))
