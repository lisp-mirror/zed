(in-package #:zed.trait.animate)

(z::define-internal-trait animate ()
  ((%sequence :reader sequence
              :inline t
              :type util.dll:list
              :initform (util.dll:make-list)))
  (:update update))

(u:fn-> update (animate) null)
(defun update (animate)
  (declare (optimize speed))
  (let* ((core (z:trait-core animate))
         (clock (z::core-clock core))
         (game-object (z:trait-owner animate)))
    (process-animations game-object clock (sequence animate))
    nil))
