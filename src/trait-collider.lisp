(in-package #:zed.trait.collider)

(z::define-internal-trait collider ()
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
   (%visual :accessor visual
            :inline t
            :type (or z:game-object null)
            :initform nil)
   (%layer :reader layer
           :inline t
           :type symbol
           :initarg :layer
           :initform nil)
   (%state :reader state
           :inline t
           :type z::collision-state
           :initform (z::make-collision-state)))
  (:setup setup)
  (:attach attach)
  (:physics physics)
  (:render render))

(z::define-material collider ()
  (:shader zed.shader::collider
   :uniforms (:hit-color (zed.math.vector4:vec 0.0 1.0 0.0 0.35)
              :miss-color (zed.math.vector4:vec 1.0 0.0 0.0 0.35))
   :features (:enable (:line-smooth)
              :disable (:cull-face)
              :polygon-mode :line
              :line-width 2.0)))

(u:fn-> enable-visibility (collider) null)
(defun enable-visibility (collider)
  (let* ((context (z:trait-context collider))
         (owner (z::trait-owner collider))
         (child (z:make-game-object :label "collider-visualization"))
         (volume (volume collider))
         (mesh (z::make-trait context
                              'tr.mesh:mesh
                              :asset '(:zed "meshes/colliders.glb")
                              :name (z::collision-volume-mesh-name volume)))
         (render (z:make-trait context 'tr.ren:render :material 'collider)))
    (z:attach-trait child mesh)
    (z:attach-trait child render)
    (setf (visual collider) (z:spawn-game-object context child owner))
    nil))

(u:fn-> remove-contacts (collider) null)
(defun remove-contacts (collider)
  (declare (optimize speed)
           (ignore collider))
  nil)

(u:fn-> register (collider) null)
(defun register (collider)
  (declare (optimize speed))
  (let* ((system (z::context-collision-system (z:trait-context collider)))
         (registered (z::collision-system-registered system))
         (layer (layer collider)))
    (unless (u:href registered layer)
      (error "Collider ~s has a layer that is not defined in the collision plan." collider))
    (setf (u:href registered layer collider) collider)
    nil))

(u:fn-> deregister (collider) null)
(defun deregister (collider)
  (declare (optimize speed))
  (let* ((system (z::context-collision-system (z:trait-context collider)))
         (deregistered (z::collision-system-deregistered system)))
    (setf (u:href deregistered (layer collider) collider) collider))
  nil)

(u:fn-> continue-collision (collider collider) null)
(defun continue-collision (collider1 collider2)
  (declare (optimize speed)
           (ignore collider1 collider2))
  nil)

(u:fn-> make-box (z:trait) z::collision-volume-box)
(defun make-box (collider)
  (declare (optimize speed))
  (let ((box (z::make-collision-volume-box :collider collider)))
    (u:when-let ((mesh (z:find-trait (z::trait-owner collider) 'tr.mesh:mesh)))
      (u:mvlet ((min max (tr.mesh::get-extents mesh)))
        (setf (z::collision-volume-center box) (v3:scale (v3:+ min max) 0.5)
              (z::collision-volume-box-min-extent box) min
              (z::collision-volume-box-max-extent box) max)))
    box))

(u:fn-> make-sphere (z:trait) z::collision-volume-sphere)
(defun make-sphere (collider)
  (declare (optimize speed))
  (let ((sphere (z::make-collision-volume-sphere :collider collider)))
    (u:when-let ((mesh (z:find-trait (z::trait-owner collider) 'tr.mesh:mesh)))
      (u:mvlet* ((min max (tr.mesh::get-extents mesh))
                 (center (v3:scale (v3:+ min max) 0.5)))
        (setf (z::collision-volume-center sphere) center
              (z::collision-volume-sphere-radius sphere) (v3:length (v3:- max center)))))
    sphere))

(u:fn-> make-volume (z::collision-volume-type z:trait) z::collision-volume)
(defun make-volume (type collider)
  (declare (optimize speed))
  (ecase type
    (:box (make-box collider))
    (:sphere (make-sphere collider))))

;;; Hooks

(u:fn-> setup (collider) null)
(defun setup (collider)
  (declare (optimize speed))
  (unless (layer collider)
    (error "Collider ~s must have a layer specified." collider))
  (register collider)
  nil)

(u:fn-> attach (collider) null)
(defun attach (collider)
  (declare (optimize speed))
  (let ((volume-type (volume-type collider)))
    (unless volume-type
      (error "Collider trait must have a volume type specified."))
    (setf (volume collider) (make-volume volume-type collider)))
  (when (visible-p collider)
    (enable-visibility collider))
  nil)

(u:fn-> physics (collider) null)
(defun physics (collider)
  (declare (optimize speed))
  (let ((volume (volume collider)))
    (funcall (z::collision-volume-update-func volume) volume collider)
    (when (visible-p collider)
      (funcall (z::collision-volume-update-visualization-func volume) volume (visual collider))))
  nil)

(u:fn-> render (collider) null)
(defun render (collider)
  (declare (optimize speed))
  (when (visible-p collider)
    (let* ((render-trait (z:find-trait (visual collider) 'tr.ren:render))
           (material (tr.ren::material render-trait)))
      (z::set-uniform material :contact (z::collision-state-hit-p (state collider))))))
