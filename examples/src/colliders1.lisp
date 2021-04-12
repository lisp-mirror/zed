(in-package #:zed-examples)

(z:define-prefab colliders1 ()
  ((gate/top :translate (v3:vec 0.0 8.0 0.0)
             :rotate-velocity (v3:velocity v3:+back+ 2.0)
             :scale 5.0)
   (z.collider:collider :layer 'gate
                        :volume :sphere))
  ((gate/bottom :translate (v3:vec 0.0 -8.0 0.0)
                :rotate-velocity (v3:velocity v3:+forward+ 2.0)
                :scale 5.0)
   (z.collider:collider :layer 'gate
                        :volume :sphere))
  ((destroyer :translate (v3:vec 30.0 0.0 0.0)
              :rotate-velocity (v3:velocity v3:+down+ 1.0)
              :scale 2.0)
   (z.collider:collider :layer 'destroyer
                        :volume :sphere))
  ((player :translate (v3:vec -30.0 0.0 0.0)
           :translate-velocity (v3:velocity v3:+right+ 15.0)
           :scale 4.0)
   (z.collider:collider :layer 'player
                        :volume :sphere)))

(z:define-collision-plan colliders1 (:cell-size 8)
  (player (gate destroyer)))

(z:define-collision-hook :enter ((player player) (gate gate))
  (z::translate/velocity player v3:+right+ 4.0))

(z:define-collision-hook :exit ((player player) (gate gate))
  (z::translate/velocity player v3:+right+ 15.0))

(z:define-collision-hook :enter ((player player) (destroyer destroyer))
  (z::translate player (v3:vec -30.0 0.0 0.0) :replace-p t :instant-p t))

(defun colliders1-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/perspective)
  (z:load-prefab context 'colliders1))

(defun colliders1 ()
  (z:start-game :window-width 1280
                :window-height 720
                :collision-plan 'colliders1
                :prelude #'colliders1-prelude))
