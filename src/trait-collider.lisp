(in-package #:zed.trait.collider)

(z::define-internal-trait collider (:before tr.ren:render)
  ((%volume-type :reader volume-type
                 :inline t
                 :type (or keyword null)
                 :initarg :volume
                 :initform :box)
   (%volume :accessor volume
            :inline t
            :type (or z::volume null)
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
   (%source :accessor source
            :inline t
            :type (or z:game-object null)
            :initarg :source
            :initform nil)
   (%picked-hook :reader picked-hook
                 :inline t
                 :type (or function symbol)
                 :initarg :picked-hook
                 :initform nil)
   (%view-enter-hook :reader view-enter-hook
                     :inline t
                     :type function
                     :initarg :view-enter-hook
                     :initform (constantly nil))
   (%view-exit-hook :reader view-exit-hook
                    :inline t
                    :type function
                    :initarg :view-exit-hook
                    :initform (constantly nil)))
  (:setup setup)
  (:attach attach)
  (:physics physics)
  (:render render))

(z::define-material collider ()
  (:shader zsl::collider
   :uniforms (:hit-color (v4:vec 0 1 0 0.75)
              :miss-color (v4:vec 1 0 0 0.75))
   :features (:enable (:line-smooth)
              :polygon-mode :line)))

(u:fn-> enable-visibility (collider) null)
(defun enable-visibility (collider)
  (let ((core (z:trait-core collider))
        (owner (z:trait-owner collider)))
    (when (or (z:find-trait owner 'tr.mesh:mesh)
              (z:find-trait owner 'tr.ren:render))
      (error "Game object ~a has a visible collider, but it must not also have a mesh or render ~
              trait."
             owner))
    (let ((mesh (z:make-trait core
                              'tr.mesh:mesh
                              :asset '(:zed "meshes/colliders.glb")
                              :name (z::volume-mesh-name (volume collider))))
          (render (z:make-trait core 'tr.ren:render :material 'collider)))
      (z:attach-trait owner mesh)
      (z:attach-trait owner render)
      nil)))

(u:fn-> make-volume (z::volume-type collider z:game-object) z::volume)
(defun make-volume (type collider source)
  (declare (optimize speed))
  (let ((layer (layer collider)))
    (ecase type
      (:box (z::make-volume/box collider layer source))
      (:sphere (z::make-volume/sphere collider layer source)))))

(u:fn-> frustum-cull-collider (collider) null)
(defun frustum-cull-collider (collider)
  (declare (optimize speed))
  (let* ((core (z:trait-core collider))
         (viewport-manager (z::core-viewports core))
         (viewport (z::viewport-manager-active viewport-manager))
         (camera (z::viewport-camera viewport))
         (volume (volume collider))
         (culled-p (geo.test:frustum/aabb (tr.cam::frustum camera)
                                          (z::volume-broad-geometry volume))))
    (u:when-let* ((source (source collider))
                  (render (z:find-trait source 'tr.ren:render)))
      (when (eq (tr.ren::culled-p render) (not culled-p))
        (if culled-p
            (funcall (view-exit-hook collider) source)
            (funcall (view-enter-hook collider) source))
        (setf (tr.ren::culled-p render) culled-p)))
    nil))

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
  (let* ((source (or (source collider) (z:trait-owner collider)))
         (volume (make-volume (volume-type collider) collider source)))
    (setf (volume collider) volume
          (source collider) source)
    (when (visible-p collider)
      (enable-visibility collider))
    nil))

(u:fn-> physics (collider) null)
(defun physics (collider)
  (declare (optimize speed))
  (let ((system (z::core-collision-system (z:trait-core collider)))
        (volume (volume collider)))
    (z::register-volume system volume)
    (frustum-cull-collider collider)
    nil))

(u:fn-> render (collider) null)
(defun render (collider)
  (declare (optimize speed))
  (when (visible-p collider)
    (let* ((volume (volume collider))
           (render-trait (z:find-trait (z:trait-owner collider) 'tr.ren:render))
           (material (tr.ren::material render-trait))
           (hit-p (plusp (hash-table-count (z::volume-contacts volume)))))
      (z::set-uniform material :contact hit-p))))
