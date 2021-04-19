(in-package #:zed.trait.mesh)

(z::define-internal-trait mesh ()
  ((%name :reader name
          :inline t
          :type (or string null)
          :initarg :name
          :initform nil)
   (%asset :reader asset
           :inline t
           :type list
           :initarg :asset
           :initform nil)
   (%index :reader index
           :inline t
           :type u:ub16
           :initarg :index
           :initform 0)
   (%instance-count :reader instance-count
                    :inline t
                    :type (and (integer 1) fixnum)
                    :initarg :instance-count
                    :initform 1)
   (%primitive :accessor primitive
               :inline t
               :type (or z::gltf-primitive null)
               :initform nil))
  (:setup setup)
  (:render render))

(defun get-extents (mesh)
  (let ((primitive (primitive mesh)))
    (values (z::gltf-primitive-extent-min primitive)
            (z::gltf-primitive-extent-max primitive))))

;;; Hooks

(u:fn-> setup (mesh) null)
(defun setup (mesh)
  (declare (optimize speed))
  (let ((asset (asset mesh)))
    (unless asset
      (error "A mesh trait must have an asset specified."))
    (let* ((core (z:trait-core mesh))
           (name (name mesh))
           (gltf (z::with-resource-cache (core :mesh asset)
                   (destructuring-bind (asset-system asset-path) asset
                     (prog1 (z::load-gltf asset)
                       (v:debug :zed "Cached mesh resource: ~a (~s)" asset-path asset-system)))))
           (mesh-data (u:href (z::gltf-meshes gltf) name)))
      (unless mesh-data
        (error "Mesh name ~s not found in mesh file ~s." name asset))
      (setf (primitive mesh) (svref (z::gltf-mesh-primitives mesh-data) (index mesh)))
      nil)))

(u:fn-> render (mesh) null)
(defun render (mesh)
  (declare (optimize speed))
  (funcall (z::gltf-primitive-draw-func (primitive mesh)) (instance-count mesh))
  nil)
