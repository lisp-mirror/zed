(in-package #:zed-examples)

(z:define-prefab turn-table (:template 'mesh)
  (z.turn-table:turn-table)
  (z.collider:collider :layer 'object
                       :volume :sphere
                       :visible-p nil
                       :picked-hook (constantly nil)))

(z:define-collision-plan turn-table ()
  (object ()))

(defun turn-table-prelude (context)
  (z:load-prefab context 'quitter)
  (z:load-prefab context 'camera/perspective)
  (z:load-prefab context 'turn-table))

(defun turn-table ()
  (z:start-game :window-width 1280
                :window-height 720
                :collision-plan 'turn-table
                :prelude #'turn-table-prelude))
