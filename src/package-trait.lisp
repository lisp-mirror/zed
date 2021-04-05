(in-package #:cl-user)

(defpackage #:zed.trait.camera
  (:local-nicknames
   (#:const #:zed.math.constants)
   (#:m3 #:zed.math.matrix3)
   (#:m4 #:zed.math.matrix4)
   (#:q #:zed.math.quaternion)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:camera
   #:resolve-normal-matrix))

(defpackage #:zed.trait.geometry
  (:local-nicknames
   (#:u #:golden-utils)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:geometry))

(defpackage #:zed.trait.render
  (:local-nicknames
   (#:tr.cam #:zed.trait.camera)
   (#:u #:golden-utils)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:render))

(defpackage #:zed.trait.mesh
  (:local-nicknames
   (#:u #:golden-utils)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:mesh))

(defpackage #:zed.trait.sprite
  (:local-nicknames
   (#:tr.ren #:zed.trait.render)
   (#:u #:golden-utils)
   (#:z #:zed)
   (#:zsl #:zed.shader-library))
  (:use #:cl)
  (:export
   #:sprite))

(defpackage #:zed.trait.collider
  (:local-nicknames
   (#:tr.mesh #:zed.trait.mesh)
   (#:tr.ren #:zed.trait.render)
   (#:u #:golden-utils)
   (#:v4 #:zed.math.vector4)
   (#:z #:zed)
   (#:zsl #:zed.shader-library))
  (:use #:cl)
  (:shadow
   #:continue)
  (:export
   #:collider))
