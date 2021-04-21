(in-package #:zed-examples)

(z:define-texture mesh (:2d-array)
  (:source ((:zed-examples "textures/damaged-helmet-albedo.png")
            (:zed-examples "textures/damaged-helmet-ao.png")
            (:zed-examples "textures/damaged-helmet-emissive.png")
            (:zed-examples "textures/damaged-helmet-metallic-roughness.png")
            (:zed-examples "textures/damaged-helmet-normal.png"))))

(z:define-material mesh ()
  (:shader shader:mesh
   :uniforms (:light.direction (v3:vec -0.7399 -0.6428 -0.1983)
              :light.color (v3:ones)
              :light.intensity 2
              :sampler 'mesh
              :base-color-factor (v4:ones)
              :metallic-factor 1
              :roughness-factor 1
              :normal-scale 1
              :normal-matrix #'z.camera:resolve-normal-matrix
              :occlusion-strength 1
              :emissive-factor 1
              :brdf-lut 'brdf-lut
              :environment-sampler 'environment-papermill
              :use-punctual t
              :use-ibl t)
   :features (:enable (:texture-cube-map-seamless))))

(z:define-prefab mesh (:scale 32.0)
  (z.mesh:mesh :name "helmet"
               :asset '(:zed-examples "meshes/damaged-helmet.glb"))
  (z.render:render :material 'mesh))

(z:define-prefab mesh-carousel (:template 'mesh
                                :rotate-velocity (v3:velocity v3:+up+ const:+pi/6+)))

(defun mesh-prelude (core)
  (z:load-prefab core 'quitter)
  (z:load-prefab core 'camera/perspective)
  (z:load-prefab core 'mesh-carousel))

(z:define-context mesh ()
  (:prelude #'mesh-prelude))
