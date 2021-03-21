(in-package #:zed-examples)

(z:define-texture sprites ()
  (:source (textures sprites)))

(z:define-material sprite ()
  (:shader umbra.sprite:sprite
   :uniforms (:sprite.sampler 'sprites
              :opacity 1.0)))

(z:define-prefab sprite ()
  (z.sprite:sprite :asset '(metadata sprites))
  (z.render:render :material 'sprite))

(z:define-prefab planet (:template 'sprite)
  (z.sprite:sprite :name "planet11"))

(z:define-prefab ship (:template 'sprite
                       :translate (v3:vec 0 -120)
                       :scale 0.6)
  (z.sprite:sprite :name "ship29")
  (z.render:render :layer 1)
  ((exhaust :template 'sprite
            :translate (v3:vec 0 -145)
            :scale (v3:vec 1 0.65 1))
   (z.sprite:sprite :name "exhaust01-01"
                    :frames 8
                    :duration 0.5)
   (z.render:render :layer 2)))

(defun sprite ()
  (flet ((prelude (context)
           (z:load-prefab context 'camera/orthographic)
           (z:load-prefab context 'planet)
           (z:load-prefab context 'ship)))
    (z:start-game :window-width 800
                  :window-height 450
                  :prelude #'prelude)))
