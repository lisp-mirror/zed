(in-package #:zed-examples)

;;; Asset pools

(zed:define-asset-pool metadata (:system :zed-examples)
  :path "data/metadata")

(zed:define-asset-pool textures (:system :zed-examples)
  :path "data/textures"
  :filter "png")

;;; Prefabs

(z:define-prefab camera ()
  (z.camera:camera))

(z:define-prefab camera/perspective (:template 'camera
                                     :translate (v3:vec 0 0 50)))

(z:define-prefab camera/orthographic (:template 'camera
                                      :translate (v3:vec 0 0 1))
  (z.camera:camera :mode :orthographic
                   :clip-near 0.0
                   :clip-far 16.0))

(z:define-prefab camera/isometric (:template 'camera
                                   :translate (v3:vec 0 0 1))
  (z.camera:camera :mode :isometric
                   :clip-near -1000.0
                   :clip-far 1000.0))
