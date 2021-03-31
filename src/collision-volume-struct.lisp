(in-package #:cl-user)

(defpackage #:%zed.collision.volume.struct
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:tr #:%zed.trait)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:shadow
   #:type))

(in-package #:%zed.collision.volume.struct)

(deftype type () '(member :box :sphere))

(defstruct (volume
            (:constructor nil)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (type :box :type type)
  (collider nil :type tr:trait)
  (mesh-name "" :type string)
  (center (v3:zero) :type v3:vec)
  (update-func (constantly nil) :type function)
  (update-visualization-func (constantly nil) :type function))

(u:define-printer (volume stream :type nil)
  (format stream "COLLISION-VOLUME: ~s" (type volume)))
