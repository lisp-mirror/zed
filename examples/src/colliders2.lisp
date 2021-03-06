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
   ((left :template 'colliders2/left))
   ((right :template 'colliders2/right
           :scale 0.5)
    (z.collider:collider :volume :sphere))))

(z:define-collision-plan colliders2 (:cell-size 256)
  (object (object)))

(defun colliders2-prelude (core)
  (z:load-prefab core 'quitter)
  (z:load-prefab core 'camera/orthographic)
  (z:load-prefab core 'colliders2))

(z:define-context colliders2 ()
  (:collision-plan 'colliders2
   :prelude #'colliders2-prelude))
