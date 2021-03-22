(in-package #:zed-examples)

(z:define-texture mesh (:2d-array)
  (:source ((textures damaged-helmet-albedo)
            (textures damaged-helmet-ao)
            (textures damaged-helmet-emissive)
            (textures damaged-helmet-metallic-roughness)
            (textures damaged-helmet-normal))))

(z:define-material mesh ()
  (:shader shader:mesh
   :uniforms (:light.direction (v3:vec -0.7399 -0.6428 -0.1983)
              :light.color (v3:vec 1)
              :light.intensity 2
              :sampler 'mesh
              :base-color-factor (v4:vec 1)
              :metallic-factor 1
              :roughness-factor 1
              :normal-scale 1
              :normal-matrix 'z.camera:resolve-normal-matrix
              :occlusion-strength 1
              :emissive-factor 1
              :brdf-lut 'brdf-lut
              :environment-sampler 'environment-papermill
              :use-punctual t
              :use-ibl t)
   :features (:enable (:texture-cube-map-seamless))))

(z:define-prefab mesh (:rotate (q:orient :local :x const:pi/2)
                       :rotate-velocity (v3:velocity v3:+forward+ (- const:pi/6))
                       :scale 17)
  (z.mesh:mesh :name "helmet"
               :asset '(meshes damaged-helmet))
  (z.render:render :material 'mesh))

(defun mesh ()
  (flet ((prelude (context)
           (z:load-prefab context 'camera/perspective)
           (z:load-prefab context 'mesh)))
    (z:start-game :window-width 1280
                  :window-height 720
                  :prelude #'prelude)))