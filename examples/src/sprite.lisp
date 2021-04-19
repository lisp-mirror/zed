(in-package #:zed-examples)

(z:define-texture sprites ()
  (:source (:zed-examples "textures/sprites.png")))

(z:define-material sprite ()
  (:shader zsl:sprite
   :uniforms (:sprite.sampler 'sprites
              :opacity 1.0)
   :features (:depth-mode :lequal)))

(z:define-prefab sprite ()
  (z.sprite:sprite :asset '(:zed-examples "metadata/sprites.spec"))
  (z.render:render :material 'sprite))

(z:define-prefab planet (:template 'sprite
                         :scale 1.5)
  (z.sprite:sprite :name "planet11"))

(z:define-prefab ship (:template 'sprite
                       :translate (v3:vec 0 -120 0)
                       :scale 1.0)
  (z.sprite:sprite :name "ship29")
  (z.render:render :layer 1)
  ((exhaust :template 'sprite
            :translate (v3:vec 0 -145 0)
            :scale (v3:vec 1 0.65 1))
   (z.sprite:sprite :name "exhaust01-01"
                    :frames 8
                    :duration 0.5)
   (z.render:render :layer 2)))

(defun sprite-prelude (core)
  (z:load-prefab core 'quitter)
  (z:load-prefab core 'camera/orthographic)
  (z:load-prefab core 'planet)
  (z:load-prefab core 'ship))

(z:define-context sprite ()
  (:prelude #'sprite-prelude))
