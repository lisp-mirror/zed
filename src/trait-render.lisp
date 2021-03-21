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
           :type fixnum
           :initarg :layer
           :initform 0))
  (:setup-hook setup)
  (:attach-hook attach)
  (:detach-hook detach)
  (:pre-render-hook pre-render))

(u:fn-> draw-order-tree-sort (cons cons) boolean)
(defun draw-order-tree-sort (x y)
  (declare (optimize speed))
  (let ((x-layer (layer (cdr x)))
        (y-layer (layer (cdr y))))
    (or (> x-layer y-layer)
        (and (= x-layer y-layer)
             (< (gob::depth (car x))
                (gob::depth (car y)))))))

(u:fn-> make-draw-order-tree () rb::tree)
(declaim (inline make-draw-order-tree))
(defun make-draw-order-tree ()
  (declare (optimize speed))
  (rb::make-tree :sort-func #'draw-order-tree-sort))

(u:fn-> render-game-object (gob::game-object render) null)
(defun render-game-object (game-object render-trait)
  (declare (optimize speed))
  (let* ((material (material render-trait))
         (func (mat.data::render-func (mat.def::data material))))
    (dbg::with-debug-group (format nil "Game Object: ~a" (gob::label game-object))
      (funcall func game-object material))
    nil))

(u:fn-> render-frame (ctx::context) null)
(defun render-frame (context)
  (declare (optimize speed))
  (u:when-let ((tree (ctx::draw-order context)))
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer :depth-buffer)
    (rb::walk tree
              (lambda (x)
                (destructuring-bind (game-object . render-trait) x
                  (render-game-object game-object render-trait))))))

;;; Hooks

(defun setup (render)
  (let ((context (trait::context render)))
    (unless (ctx::draw-order context)
      (setf (ctx::draw-order context) (make-draw-order-tree)))))

(defun attach (render)
  (u:if-let ((material-name (material-name render)))
    (let ((context (trait::context render))
          (owner (trait::owner render)))
      (setf (material render) (mat::make-material context material-name))
      (rb::insert (ctx::draw-order context) (cons owner render)))
    (error "Render trait must have a material specified.")))

(defun detach (render)
  (let ((context (trait::context render))
        (owner (trait::owner render)))
    (rb::delete (ctx::draw-order context) (cons owner render))))

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
