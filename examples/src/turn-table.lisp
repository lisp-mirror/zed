(in-package #:zed-examples)

(z:define-prefab turn-table (:template 'mesh)
  (z.turn-table:turn-table)
  (z.collider:collider :layer 'object
                       :volume :sphere
                       :visible-p nil
                       :picked-hook (constantly nil)))

(z:define-collision-plan turn-table ()
  (object ()))

(defun turn-table-prelude (core)
  (z:load-prefab core 'quitter)
  (z:load-prefab core 'camera/perspective)
  (z:load-prefab core 'turn-table))

(z:define-context turn-table ()
  (:collision-plan 'turn-table
   :prelude #'turn-table-prelude))
