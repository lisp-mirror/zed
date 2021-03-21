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
   (#:rb #:%zed.red-black-tree)
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
  (< (+ (ash (- #.(1- (expt 2 15)) (layer x)) 16) (gob::depth (trait::owner x)))
     (+ (ash (- #.(1- (expt 2 15)) (layer y)) 16) (gob::depth (trait::owner y)))))

(u:fn-> make-draw-order-tree () rb::tree)
(declaim (inline make-draw-order-tree))
(defun make-draw-order-tree ()
  (declare (optimize speed))
  (rb::make-tree :sort-func #'draw-order-tree-sort))

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
  (u:when-let ((tree (ctx::draw-order context)))
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer :depth-buffer)
    (rb::walk tree #'render-game-object)))

;;; Hooks

(defun setup (render)
  (let ((context (trait::context render)))
    (unless (ctx::draw-order context)
      (setf (ctx::draw-order context) (make-draw-order-tree)))))

(defun attach (render)
  (u:if-let ((material-name (material-name render)))
    (let ((context (trait::context render)))
      (setf (material render) (mat::make-material context material-name))
      (rb::insert (ctx::draw-order context) render))
    (error "Render trait must have a material specified.")))

(defun detach (render)
  (let ((context (trait::context render)))
    (rb::delete (ctx::draw-order context) render)))

(defun pre-render (render)
  (u:when-let* ((context (trait::context render))
                (owner (trait::owner render))
                (camera (ctx::active-camera context))
                (camera-state (tr.cam::state camera))
                (world-matrix (ts::world-matrix (gob::transform owner))))
    (mat::set-uniforms (material render)
                       :model world-matrix
                       :view (cam::view camera-state)
                       :proj (cam::projection camera-state))))
