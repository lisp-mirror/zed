(in-package #:cl-user)

(uiop:define-package #:zed
  (:use #:cl)
  (:use-reexport
   #:%zed.asset-pool
   #:%zed.core
   #:%zed.game-object
   #:%zed.input-interface
   #:%zed.material
   #:%zed.prefab
   #:%zed.texture.data
   #:%zed.trait
   #:%zed.tree))

(uiop:define-package #:zed.shader
  (:use-reexport
   #:shadow.glsl
   #:umbra.common))
