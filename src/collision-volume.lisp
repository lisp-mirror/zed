(in-package #:zed)

(deftype collision-volume-type () '(member :box :sphere))

(defstruct (collision-volume
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (type :box :type collision-volume-type)
  (collider nil :type trait)
  (layer nil :type symbol)
  (mesh-name "" :type string)
  (grid-cell-size 8 :type u:positive-fixnum)
  (contacts (u:dict #'eq) :type hash-table)
  (source nil :type game-object)
  (center (v3:zero) :type v3:vec)
  (world-center (v3:zero) :type v3:vec)
  (broad-phase-min (v3:zero) :type v3:vec)
  (broad-phase-max (v3:zero) :type v3:vec)
  (update-func (constantly nil) :type function))

(u:define-printer (collision-volume stream :type nil)
  (let ((path (game-object-path (trait-owner (collision-volume-collider collision-volume)))))
    (format stream "COLLISION-VOLUME: ~s (~a)"
            (collision-volume-type collision-volume)
            path)))
