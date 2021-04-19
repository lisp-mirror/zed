(in-package #:zed-examples)

;;; Textures

(z:define-texture brdf-lut ()
  (:source (:zed-examples "textures/brdf-lut.png")))

(z:define-texture environment-papermill (:cube-map-array)
  (:min-filter :linear-mipmap-linear
   :source ((:x+ (:zed-examples "textures/environments/papermill-diffuse-right.hdr")
             :x- (:zed-examples "textures/environments/papermill-diffuse-left.hdr")
             :y+ (:zed-examples "textures/environments/papermill-diffuse-top.hdr")
             :y- (:zed-examples "textures/environments/papermill-diffuse-bottom.hdr")
             :z+ (:zed-examples "textures/environments/papermill-diffuse-front.hdr")
             :z- (:zed-examples "textures/environments/papermill-diffuse-back.hdr"))
            (:x+ (:zed-examples "textures/environments/papermill-specular-right.hdr")
             :x- (:zed-examples "textures/environments/papermill-specular-left.hdr")
             :y+ (:zed-examples "textures/environments/papermill-specular-top.hdr")
             :y- (:zed-examples "textures/environments/papermill-specular-bottom.hdr")
             :z+ (:zed-examples "textures/environments/papermill-specular-front.hdr")
             :z- (:zed-examples "textures/environments/papermill-specular-back.hdr")))))

;;; Prefabs

(z:define-prefab camera ()
  (z.camera:camera))

(z:define-prefab camera/perspective (:template 'camera
                                     :translate (v3:vec 0.0 0.0 50.0)))

(z:define-prefab camera/orthographic (:template 'camera
                                      :translate (v3:vec 0.0 0.0 1.0))
  (z.camera:camera :mode :orthographic
                   :clip-near -1000.0
                   :clip-far 1000.0))

(z:define-prefab camera/isometric (:template 'camera
                                   :translate (v3:vec 0.0 0.0 1.0))
  (z.camera:camera :mode :isometric
                   :clip-near -1000.0
                   :clip-far 1000.0))

(z:define-prefab quitter ()
  (quitter))

;;; Traits

(z:define-trait quitter ()
  ()
  (:update quitter/update))

(defun quitter/update (quitter)
  (let ((core (z:trait-core quitter)))
    (when (z:on-button-enter core :mouse :left)
      (z::pick-game-object core))
    (when (or (z:on-button-enter core :key :escape)
              (z:on-button-enter core :window :close))
      (z:stop-game core))))
