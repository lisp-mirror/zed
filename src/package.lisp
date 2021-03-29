(in-package #:defpackage+-user-1)

(defpackage+ #:zed
  (:use #:cl)
  (:inherit
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
