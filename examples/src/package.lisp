(in-package #:cl-user)

(defpackage #:zed-examples.shader
  (:use #:zed.shader-library)
  ;; shaders
  (:export
   #:geometry
   #:mesh))

(defpackage #:zed-examples
  (:local-nicknames
   (#:const #:zed.math.constants)
   (#:q #:zed.math.quaternion)
   (#:shader #:zed-examples.shader)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4)
   (#:z #:zed)
   (#:zsl #:zed.shader-library)
   (#:z.camera #:zed.trait.camera)
   (#:z.collider #:zed.trait.collider)
   (#:z.geometry #:zed.trait.geometry)
   (#:z.mesh #:zed.trait.mesh)
   (#:z.render #:zed.trait.render)
   (#:z.sprite #:zed.trait.sprite))
  (:use #:cl)
  (:export
   #:colliders
   #:geometry
   #:mesh
   #:sprite))
