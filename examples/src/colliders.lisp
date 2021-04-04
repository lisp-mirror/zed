(in-package #:zed-examples)

(z:define-prefab colliders ()
  ((collider1))
  ((collider2)))

(z:define-collision-plan colliders ()
  (layer1 (layer1)))

(defun colliders-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/orthographic)
  (z:load-prefab context 'colliders))

(defun colliders ()
  (z:start-game :window-width 1280
                :window-height 720
                :collision-plan 'colliders
                :prelude #'colliders-prelude))
