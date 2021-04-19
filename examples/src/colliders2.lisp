(in-package #:zed-examples)

(z:define-prefab colliders2/left (:translate (v3:vec -0.55 0.0 0.0)
                                  :rotate-velocity (v3:velocity v3:+forward+ const:+pi/4+))
  (z.collider:collider :volume :box
                       :picked-hook (lambda (x) (:printv x))
                       :layer 'object))

(z:define-prefab colliders2/right (:translate (v3:vec 0.5 0.0 0.0))
  (z.collider:collider :volume :box
                       :picked-hook (lambda (x) (:printv x))
                       :layer 'object))

(z:define-prefab colliders2 (:scale 128.0)
  ((test1 :translate (v3:vec -2.0 0.0 0.0))
   ((left :template 'colliders2/left))
   ((right :template 'colliders2/right)))
  ((test2 :translate (v3:vec 2.0 0.0 0.0))
   ((left :template 'colliders2/left
          :scale (v3:vec 1.0 3.0 1.0)))
   ((right :template 'colliders2/right
           :scale 0.5)
    (z.collider:collider :volume :sphere))))

(z:define-collision-plan colliders2 (:cell-size 256)
  (object (object)))

(defun colliders2-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/orthographic)
  (z:load-prefab context 'colliders2))

(defun colliders2 ()
  (z:start-game :window-width 1280
                :window-height 720
                :collision-plan 'colliders2
                :prelude #'colliders2-prelude))
