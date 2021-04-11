(in-package #:zed-examples)

(z:define-prefab colliders1 ()
  ((gate/top :translate (v3:vec 0.0 8.0 0.0)
             :scale 5.0)
   (z.collider:collider :layer 'gate
                        :volume :sphere))
  ((gate/bottom :translate (v3:vec 0.0 -8.0 0.0)
                :scale 5.0)
   (z.collider:collider :layer 'gate
                        :volume :sphere))
  ((destroyer :translate (v3:vec 30.0 0.0 0.0)
              :rotate-velocity (v3:velocity v3:+down+ 5.0)
              :scale 3.0)
   (z.collider:collider :layer 'destroyer
                        :volume :sphere))
  ((player :translate (v3:vec -30.0 0.0 0.0)
           :translate-velocity (v3:velocity v3:+right+ 15.0)
           :scale 4.0)
   (z.collider:collider :layer 'player
                        :volume :sphere)))

(z:define-collision-plan colliders1 (:multi-level-p t)
  (player (gate destroyer))
  (gate (player))
  (destroyer (player)))

(defun colliders1-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/perspective)
  (z:load-prefab context 'colliders1))

(defun colliders1 ()
  (z:start-game :window-width 1280
                :window-height 720
                :collision-plan 'colliders1
                :prelude #'colliders1-prelude))
