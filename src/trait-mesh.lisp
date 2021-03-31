(in-package #:cl-user)

(defpackage #:zed.trait.mesh
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:gltf #:%zed.mesh.gltf)
   (#:log #:%zed.logging)
   (#:rc #:%zed.resource-cache)
   (#:tr #:%zed.trait))
  (:use #:cl)
  (:export
   #:mesh))

(in-package #:zed.trait.mesh)

(tr::define-internal-trait mesh ()
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
               :type (or gltf::primitive null)
               :initform nil))
  (:setup setup)
  (:render render))

(defun get-extents (mesh)
  (let ((primitive (primitive mesh)))
    (values (gltf::primitive-extent-min primitive)
            (gltf::primitive-extent-max primitive))))

;;; Hooks

(u:fn-> setup (mesh) null)
(defun setup (mesh)
  (declare (optimize speed))
  (let ((asset (asset mesh)))
    (unless asset
      (error "A mesh trait must have an asset specified."))
    (let* ((context (tr:context mesh))
           (name (name mesh))
           (gltf (rc::with-resource-cache (context :mesh asset)
                   (destructuring-bind (asset-system asset-path) asset
                     (prog1 (gltf::load asset)
                       (log::debug :zed.trait.mesh "Cached mesh resource: ~a (~s)"
                                   asset-path
                                   asset-system)))))
           (mesh-data (u:href (gltf::gltf-meshes gltf) name)))
      (unless mesh-data
        (error "Mesh name ~s not found in mesh file ~s." name asset))
      (setf (primitive mesh) (svref (gltf::mesh-primitives mesh-data) (index mesh)))
      nil)))

(u:fn-> render (mesh) null)
(defun render (mesh)
  (declare (optimize speed))
  (funcall (gltf::primitive-draw-func (primitive mesh)) (instance-count mesh))
  nil)
