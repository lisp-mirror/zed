(in-package #:zed-examples)

(z:define-trait foo () ()
  (:attach attach))

(defun attach (foo)
  (let ((ctx (z::trait-context foo))
        (owner (z::trait-owner foo)))
    (dotimes (i 1)
      (let ((gob (z::make-game-object))
            (col (z::make-trait ctx
                                'z.collider:collider
                                :layer 'layer1
                                :volume :box)))
        (z:spawn-game-object ctx gob owner)
        (z::translate gob (v3:random -20.0 20.0))
        (z::scale gob (v3:uniform (+ 1.0 (random 5.0))))
        (z::rotate/velocity gob v3:+forward+ 1.0)
        (z:attach-trait gob col)))))

(z:define-prefab colliders ()
  (foo))

(z:define-collision-plan colliders ()
  (layer1 (layer1)))

(defun colliders-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/orthographic)
  (z:load-prefab context 'colliders))

(defun colliders ()
  (z:start-game :window-width 1280
                :window-height 720
                :profile-p t
                :vsync-p nil
                :frame-count 30000
                :collision-plan 'colliders
                :log-repl-level :error
                :delta-time 1/10
                :prelude #'colliders-prelude))
