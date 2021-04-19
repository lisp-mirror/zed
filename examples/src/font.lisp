(in-package #:zed-examples)

(z:define-texture font ()
  (:source (:zed-examples "textures/font.png")
   :mipmaps-p nil))

(z:define-material font ()
  (:shader zsl:font
   :uniforms (:sampler 'font
              :color (v4:vec 0 1 0 0.75))))

(z:define-prefab font (:scale 4.0)
  (z.font:font :asset '(:zed-examples "metadata/font.json")
               :update-rate 0.5
               :text (lambda () (format nil "~d" (random 1000.0))))
  (z.render:render :material 'font))

(defun font-prelude (core)
  (z:load-prefab core 'quitter)
  (z:load-prefab core 'camera/orthographic)
  (z:load-prefab core 'font))

(defun font ()
  (z:start-game :window-width 1280
                :window-height 720
                :prelude #'font-prelude))
