(in-package #:cl-user)

(defpackage #:%zed.collision.volume
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:tr #:%zed.trait)
   (#:vol.box #:%zed.collision.volume.box)
   (#:vol.sphere #:%zed.collision.volume.sphere)
   (#:vol.struct #:%zed.collision.volume.struct))
  (:use #:cl))

(in-package #:%zed.collision.volume)

(u:fn-> make-volume (vol.struct::type tr:trait) vol.struct::volume)
(defun make-volume (type collider)
  (declare (optimize speed))
  (ecase type
    (:box (vol.box::make-box collider))
    (:sphere (vol.sphere::make-sphere collider))))
