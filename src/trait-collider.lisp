(in-package #:cl-user)

(defpackage #:zed.trait.collider
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:mat #:%zed.material)
   (#:gob #:%zed.game-object)
   (#:tr #:%zed.trait)
   (#:tr.mesh #:zed.trait.mesh)
   (#:tr.ren #:zed.trait.render)
   (#:tree #:%zed.tree)
   (#:vol #:%zed.collision.volume)
   (#:vol.struct #:%zed.collision.volume.struct))
  (:use #:cl)
  (:export
   #:collider))

(in-package #:zed.trait.collider)

(tr::define-internal-trait collider ()
  ((%visible-p :reader visible-p
               :inline t
               :type boolean
               :initarg :visible-p
               :initform t)
   (%volume-type :reader volume-type
                 :inline t
                 :type (or keyword null)
                 :initarg :volume
                 :initform :box)
   (%volume :accessor volume
            :inline t
            :type (or vol.struct::volume null)
            :initform nil)
   (%hit-p :accessor hit-p
           :inline t
           :type boolean
           :initform nil))
  (:attach attach)
  (:update update)
  (:render render))

(mat::define-material collider ()
  (:shader zed.shader::collider
   :uniforms (:hit-color (zed.math.vector4:vec 0.0 1.0 0.0 0.35)
              :miss-color (zed.math.vector4:vec 1.0 0.0 0.0 0.35))
   :features (:enable (:line-smooth)
              :disable (:cull-face)
              :polygon-mode :line
              :line-width 2.0)))

(u:fn-> enable-visibility (collider) null)
(defun enable-visibility (collider)
  (declare (optimize speed))
  (let* ((context (tr::context collider))
         (owner (tr::owner collider))
         (volume (volume collider))
         (mesh (tr::make-trait context
                               'tr.mesh:mesh
                               :asset '(:zed "meshes/colliders.glb")
                               :name (vol.struct::mesh-name volume)))
         (render (tr::make-trait context 'tr.ren:render :material 'collider)))
    (tr::attach-trait owner mesh)
    (tr::attach-trait owner render)
    nil))

;;; Hooks

(u:fn-> attach (collider) null)
(defun attach (collider)
  (declare (optimize speed))
  (let ((volume-type (volume-type collider)))
    (unless volume-type
      (error "Collider trait must have a volume type specified."))
    (setf (volume collider) (vol::make-volume volume-type collider)))
  (when (visible-p collider)
    (enable-visibility collider))
  nil)

(u:fn-> update (collider) null)
(defun update (collider)
  (declare (optimize speed))
  (let ((volume (volume collider)))
    (funcall (vol.struct::update-func volume) volume collider)
    (when (visible-p collider)
      (funcall (vol.struct::update-visualization-func volume) volume)))
  nil)

(u:fn-> render (collider) null)
(defun render (collider)
  (declare (optimize speed))
  (when (visible-p collider)
    (let* ((render-trait (tr:find-trait (tr::owner collider) 'tr.ren:render))
           (material (tr.ren::material render-trait)))
      (mat::set-uniform material :contact (hit-p collider)))))
