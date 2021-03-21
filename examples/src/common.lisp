(in-package #:zed-examples)

;;; Asset pools

(z:define-asset-pool metadata (:system :zed-examples)
  :path "data/metadata")

(z:define-asset-pool textures (:system :zed-examples)
  :path "data/textures"
  :filter "png")

(z:define-asset-pool environments (:system :zed-examples)
  :path "data/textures/environments"
  :filter "hdr")

(z:define-asset-pool meshes (:system :zed-examples)
  :path "data/meshes"
  :filter "glb")

;;; Textures

(z:define-texture brdf-lut ()
  (:source (textures brdf-lut)))

(z:define-texture environment-papermill (:cube-map-array)
  (:min-filter :linear-mipmap-linear
   :source ((:x+ (environments papermill-diffuse-right)
             :x- (environments papermill-diffuse-left)
             :y+ (environments papermill-diffuse-top)
             :y- (environments papermill-diffuse-bottom)
             :z+ (environments papermill-diffuse-front)
             :z- (environments papermill-diffuse-back))
            (:x+ (environments papermill-specular-right)
             :x- (environments papermill-specular-left)
             :y+ (environments papermill-specular-top)
             :y- (environments papermill-specular-bottom)
             :z+ (environments papermill-specular-front)
             :z- (environments papermill-specular-back)))))

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
