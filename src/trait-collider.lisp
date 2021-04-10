(in-package #:zed.trait.collider)

(z::define-internal-trait collider ()
  ((%volume-type :reader volume-type
                 :inline t
                 :type (or keyword null)
                 :initarg :volume
                 :initform :box)
   (%volume :accessor volume
            :inline nil
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
   (%pickable-p :reader pickable-p
                :inline t
                :type boolean
                :initarg :pickable-p
                :initform t)
   (%grid-cell-size :accessor grid-cell-size
                    :inline t
                    :type u:ub32
                    :initform 8)
   (%contact-count :accessor contact-count
                   :inline t
                   :type fixnum
                   :initform 0)
   (%hit-p :accessor hit-p
           :inline t
           :type boolean
           :initform nil))
  (:setup setup)
  (:attach attach)
  (:detach detach)
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

(u:fn-> enter (collider collider) null)
(defun enter (collider1 collider2)
  (declare (optimize speed))
  (incf (contact-count collider1))
  (when (plusp (contact-count collider1))
    (setf (hit-p collider1) t))
  (when (plusp (contact-count collider2))
    (setf (hit-p collider2) t))
  #++(z::on-collision-enter)
  nil)

(u:fn-> exit (collider collider) null)
(defun exit (collider1 collider2)
  (declare (optimize speed))
  (decf (contact-count collider1))
  (when (zerop (contact-count collider1))
    (setf (hit-p collider1) nil))
  (when (zerop (contact-count collider2))
    (setf (hit-p collider2) nil))
  #++(z::on-collision-exit)
  nil)

(u:fn-> continue (collider collider) null)
(defun continue (collider1 collider2)
  (declare (optimize speed)
           (ignore collider1 collider2))
  #++(z::on-collision-continue)
  nil)

(u:fn-> make-volume (z::collision-volume-type z:trait) z::collision-volume)
(defun make-volume (type collider)
  (declare (optimize speed))
  (ecase type
    (:box (z::make-collision-volume-box :collider collider))
    (:sphere (z::make-collision-volume-sphere :collider collider))))

(defun ensure-grid (collider)
  (let* ((context (z:trait-context collider))
         (system (z::context-collision-system context))
         (grids (z::collision-system-grids system))
         (bucket-size (z::collision-system-bucket-size system))
         (cell-sizes (z::collision-system-cell-sizes system))
         (volume (volume collider)))
    (v3:with-components ((min- (z::collision-volume-broad-phase-min volume))
                         (max- (z::collision-volume-broad-phase-max volume)))
      (let* ((volume-size (max (- max-x min-x) (- max-y min-y) (- max-z min-z)))
             (cell-size (ash 1 (max 3 (integer-length (ceiling volume-size)))))
             (grid (z::make-hash-grid :bucket-size bucket-size :cell-size cell-size)))
        (unless (u:href grids cell-size)
          (setf (u:href grids cell-size) grid
                (z::collision-system-cell-sizes system) (sort (list* cell-size cell-sizes) #'<)))
        (setf (grid-cell-size collider) cell-size)))))

;;; Hooks

(u:fn-> setup (collider) null)
(defun setup (collider)
  (declare (optimize speed))
  (let ((volume-type (volume-type collider)))
    (unless (layer collider)
      (error "Collider ~s must have a layer specified." collider))
    (setf (volume collider) (make-volume volume-type collider)))
  nil)

(u:fn-> attach (collider) null)
(defun attach (collider)
  (declare (optimize speed))
  (let ((volume (volume collider)))
    (when (visible-p collider)
      (enable-visibility collider))
    (funcall (z::collision-volume-update-func volume) volume collider)
    (ensure-grid collider)
    nil))

(u:fn-> detach (collider) null)
(defun detach (collider)
  (declare (optimize speed)
           (ignore collider))
  nil)

(u:fn-> physics (collider) null)
(defun physics (collider)
  (declare (optimize speed))
  (let ((volume (volume collider))
        (system (z::context-collision-system (z:trait-context collider))))
    (funcall (z::collision-volume-update-func volume) volume collider)
    (z::register-collider system (grid-cell-size collider) volume))
  nil)

(u:fn-> render (collider) null)
(defun render (collider)
  (declare (optimize speed))
  (when (visible-p collider)
    (let* ((render-trait (z:find-trait (z::trait-owner collider) 'tr.ren:render))
           (material (tr.ren::material render-trait)))
      (z::set-uniform material :contact (hit-p collider)))))
