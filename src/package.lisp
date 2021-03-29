(in-package #:cl-user)

(uiop:define-package #:zed
  (:use #:cl)
  (:use-reexport
   #:%zed.core
   #:%zed.game-object
   #:%zed.geometry
   #:%zed.geometry.layout.data
   #:%zed.input-interface
   #:%zed.material
   #:%zed.prefab
   #:%zed.texture.data
   #:%zed.trait
   #:%zed.tree
   #:%zed.viewport.data))
