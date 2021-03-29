(in-package #:zed-examples)

(z:define-geometry-layout tile-layout ()
  (:data (:format :interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(z:define-geometry tile ()
  (:layout tile-layout
   :vertex-count 4
   :primitive :triangle-strip
   :buffers
   (:data (((-0.5 0.5) (0 1))
           ((-0.5 -0.5) (0 0))
           ((0.5 0.5) (1 1))
           ((0.5 -0.5) (1 0))))))

(z:define-material tile ()
  (:shader shader:geometry
   :features (:disable (:cull-face))))

(z:define-prefab tile (:rotate-velocity (v3:velocity v3:+right+ const:+pi/2+)
                       :scale 30.0)
  (z.geometry:geometry :name 'tile)
  (z.render:render :material 'tile))

(defun geometry-prelude (context)
  (z:load-prefab context 'camera/perspective)
  (z:load-prefab context 'tile))

(defun geometry ()
  (z:start-game :window-width 1280
                :window-height 720
                :prelude #'geometry-prelude))
