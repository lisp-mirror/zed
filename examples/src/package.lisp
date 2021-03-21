(in-package #:cl-user)

(defpackage #:zed-examples.shader
  (:use #:zed.shader)
  ;; shaders
  (:export
   #:mesh))

(defpackage #:zed-examples
  (:local-nicknames
   (#:const #:origin.constants)
   (#:q #:origin.quat)
   (#:shader #:zed-examples.shader)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:z #:zed)
   (#:z.camera #:zed.trait.camera)
   (#:z.mesh #:zed.trait.mesh)
   (#:z.render #:zed.trait.render)
   (#:z.sprite #:zed.trait.sprite))
  (:use #:cl)
  (:export
   #:mesh
   #:sprite))
