(in-package #:cl-user)

(defpackage #:zed.trait.camera
  (:local-nicknames
   (#:const #:zed.math.constants)
   (#:frustum #:zed.math.frustum)
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

(defpackage #:zed.trait.font
  (:local-nicknames
   (#:tr.geo #:zed.trait.geometry)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:font))

(defpackage #:zed.trait.collider
  (:local-nicknames
   (#:aabb #:zed.math.aabb)
   (#:geo.test #:zed.math.primitive-test)
   (#:tr.cam #:zed.trait.camera)
   (#:tr.mesh #:zed.trait.mesh)
   (#:tr.ren #:zed.trait.render)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4)
   (#:z #:zed)
   (#:zsl #:zed.shader-library))
  (:use #:cl)
  (:shadow
   #:type)
  (:export
   #:collider))

(defpackage #:zed.trait.turn-table
  (:local-nicknames
   (#:q #:zed.math.quaternion)
   (#:tr.col #:zed.trait.collider)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:turn-table))

(defpackage #:zed.trait.curve
  (:local-nicknames
   (#:curve #:%zed.utility.bezier-curve)
   (#:tr.geo #:zed.trait.geometry)
   (#:tr.ren #:zed.trait.render)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3)
   (#:z #:zed)
   (#:zsl #:zed.shader-library))
  (:use #:cl)
  (:export
   #:curve))

(defpackage #:zed.trait.animate
  (:local-nicknames
   (#:u #:golden-utils)
   (#:util.dll #:%zed.utility.doubly-linked-list)
   (#:z #:zed))
  (:use #:cl)
  (:shadow
   #:sequence)
  (:export
   #:animate
   #:sequence))
