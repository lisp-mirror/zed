(in-package #:zed-examples)

(z:define-prefab colliders ()
  ((collider1
    :translate (v3:vec -5.0 0.0 0.0)
    :scale 5.0)
   (z.collider:collider :layer 'layer1
                        :volume :sphere))
  ((collider2
    :translate (v3:vec 5.0 0.0 0.0)
    :rotate-velocity (v3:velocity v3:+forward+ 0.3)
    :scale 5.0)
   (z.collider:collider :layer 'layer1
                        :volume :sphere)))

(z:define-collision-plan colliders ()
  (layer1 (layer1)))

(defun colliders-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/perspective)
  (z:load-prefab context 'colliders))

(defun colliders ()
  (z:start-game :window-width 1280
                :window-height 720
                :collision-plan 'colliders
                :prelude #'colliders-prelude))
