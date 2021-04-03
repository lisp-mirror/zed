(in-package #:zed)

(deftype collision-volume-type () '(member :box :sphere))

(defstruct (collision-volume
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (type :box :type collision-volume-type)
  (collider nil :type trait)
  (mesh-name "" :type string)
  (center (v3:zero) :type v3:vec)
  (update-func (constantly nil) :type function)
  (update-visualization-func (constantly nil) :type function))

(u:define-printer (collision-volume stream :type nil)
  (format stream "COLLISION-VOLUME: ~s" (collision-volume-type collision-volume)))
