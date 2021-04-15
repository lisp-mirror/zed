(in-package #:zed-examples)

(z:define-texture texture ()
  (:source (:zed-examples "textures/debug.png")))

(z:define-material texture ()
  (:shader shader:texture
   :uniforms (:sampler 'texture)))

(z:define-prefab texture ()
  (z.geometry:geometry :name 'tile)
  (z.render:render :material 'texture))

(defun texture-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/orthographic)
  (z:load-prefab context 'texture))

(defun texture ()
  (z:start-game :window-width 1280
                :window-height 720
                :prelude #'texture-prelude))
