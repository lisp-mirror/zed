(in-package #:zed-examples)

(z:define-texture texture ()
  (:source (:zed-examples "textures/debug.png")))

(z:define-material texture ()
  (:shader shader:texture
   :uniforms (:sampler 'texture)))

(z:define-prefab texture ()
  (z.geometry:geometry :name 'tile)
  (z.render:render :material 'texture))

(defun texture-prelude (core)
  (z:load-prefab core 'quitter)
  (z:load-prefab core 'camera/orthographic)
  (z:load-prefab core 'texture))

(z:define-context texture ()
  (:prelude #'texture-prelude))
