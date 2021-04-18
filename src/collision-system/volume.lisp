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
  (contacts (u:dict #'eq) :type hash-table)
  (source nil :type game-object)
  (center (v3:zero) :type v3:vec)
  (world-center (v3:zero) :type v3:vec)
  (broad-phase-volume (aabb:aabb) :type aabb:aabb)
  (update-func (constantly nil) :type function))

(u:define-printer (collision-volume stream :type nil)
  (let ((path (game-object-path (trait-owner (collision-volume-collider collision-volume)))))
    (format stream "COLLISION-VOLUME: ~s (~a)"
            (collision-volume-type collision-volume)
            path)))
