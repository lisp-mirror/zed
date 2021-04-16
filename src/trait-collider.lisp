(in-package #:zed.trait.collider)

(z::define-internal-trait collider (:order (:before tr.ren:render))
  ((%volume-type :reader volume-type
                 :inline t
                 :type (or keyword null)
                 :initarg :volume
                 :initform :box)
   (%volume :accessor volume
            :inline t
            :type (or z::collision-volume null)
            :initform nil)
   (%visible-p :reader visible-p
               :inline t
               :type boolean
               :initarg :visible-p
               :initform t)
   (%layer :reader layer
           :inline t
           :type symbol
           :initarg :layer
           :initform nil)
   (%source :reader source
            :inline t
            :type (or z:game-object null)
            :initarg :source
            :initform nil)
   (%pickable-p :reader pickable-p
                :inline t
                :type boolean
                :initarg :pickable-p
                :initform t))
  (:setup setup)
  (:attach attach)
  (:physics physics)
  (:render render))

(z::define-material collider ()
  (:shader zsl::collider
   :uniforms (:hit-color (v4:vec 0.0 1.0 0.0 0.35)
              :miss-color (v4:vec 1.0 0.0 0.0 0.35))
   :features (:enable (:line-smooth)
              :disable (:cull-face)
              :polygon-mode :line
              :line-width 2.0)))

(u:fn-> enable-visibility (collider) null)
(defun enable-visibility (collider)
  (let ((context (z:trait-context collider))
        (owner (z::trait-owner collider)))
    (when (or (z:find-trait owner 'tr.mesh:mesh)
              (z:find-trait owner 'tr.ren:render))
      (error "Game object ~a has a visible collider, but it must not also have a mesh or render ~
              trait."
             owner))
    (let ((mesh (z:make-trait context
                              'tr.mesh:mesh
                              :asset '(:zed "meshes/colliders.glb")
                              :name (z::collision-volume-mesh-name (volume collider))))
          (render (z:make-trait context 'tr.ren:render :material 'collider)))
      (z:attach-trait owner mesh)
      (z:attach-trait owner render)
      nil)))

(u:fn-> make-volume (z::collision-volume-type collider z:game-object) z::collision-volume)
(defun make-volume (type collider source)
  (declare (optimize speed))
  (let ((layer (layer collider)))
    (ecase type
      (:box (z::make-collision-volume-box :collider collider :layer layer :source source))
      (:sphere (z::make-collision-volume-sphere :collider collider :layer layer :source source)))))

(u:fn-> frustum-cull-collider (collider) null)
(defun frustum-cull-collider (collider)
  (declare (optimize speed))
  (u:when-let* ((context (z:trait-context collider))
                (camera (z::context-active-camera context))
                (volume (volume collider))
                (min (z::collision-volume-broad-phase-min volume))
                (max (z::collision-volume-broad-phase-max volume)))
    (let ((culled-p (z::frustum-aabb (tr.cam::frustum camera) min max)))
      (u:when-let ((render (z:find-trait (z::trait-owner collider) 'tr.ren:render)))
        (setf (tr.ren::culled-p render) culled-p))))
  nil)

;;; Hooks

(u:fn-> setup (collider) null)
(defun setup (collider)
  (declare (optimize speed))
  (unless (layer collider)
    (error "Collider ~s must have a layer specified." collider))
  nil)

(u:fn-> attach (collider) null)
(defun attach (collider)
  (declare (optimize speed))
  (let* ((source (or (source collider) (z::trait-owner collider)))
         (volume (make-volume (volume-type collider) collider source)))
    (setf (volume collider) volume)
    (when (visible-p collider)
      (enable-visibility collider))
    nil))

(u:fn-> physics (collider) null)
(defun physics (collider)
  (declare (optimize speed))
  (let ((system (z::context-collision-system (z:trait-context collider)))
        (volume (volume collider)))
    (z::register-collision-volume system volume)
    (frustum-cull-collider collider)
    nil))

(u:fn-> render (collider) null)
(defun render (collider)
  (declare (optimize speed))
  (when (visible-p collider)
    (let* ((volume (volume collider))
           (render-trait (z:find-trait (z::trait-owner collider) 'tr.ren:render))
           (material (tr.ren::material render-trait))
           (hit-p (plusp (hash-table-count (z::collision-volume-contacts volume)))))
      (z::set-uniform material :contact hit-p))))
