(in-package #:cl-user)

(defpackage #:%zed.collision.volume.sphere
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:gob #:%zed.game-object)
   (#:tfm #:%zed.transform)
   (#:tr #:%zed.trait)
   (#:tr.mesh #:zed.trait.mesh)
   (#:v3 #:zed.math.vector3)
   (#:vol.struct #:%zed.collision.volume.struct))
  (:use #:cl)
  (:shadow
   #:type))

(in-package #:%zed.collision.volume.sphere)

(declaim (inline %make-sphere))
(defstruct (sphere
            (:include vol.struct::volume
             (type :sphere)
             (mesh-name "sphere")
             (update-visualization-func #'update-visualization))
            (:constructor %make-sphere)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (radius 1.0 :type u:f32))

(u:fn-> update-visualization (sphere) null)
(defun update-visualization (sphere)
  (declare (optimize speed))
  (let ((game-object (tr::owner (collider sphere))))
    (tfm::translate game-object (center sphere) :replace-p t)
    (tfm::scale game-object (v3:uniform (radius sphere)) :replace-p t))
  nil)

(u:fn-> make-sphere (tr:trait) sphere)
(defun make-sphere (collider)
  (declare (optimize speed))
  (let ((sphere (%make-sphere :collider collider)))
    (u:when-let ((mesh (tr:find-trait (tr::owner collider) 'tr.mesh:mesh)))
      (u:mvlet* ((min max (tr.mesh::get-extents mesh))
                 (center (v3:scale (v3:+ min max) 0.5)))
        (setf (center sphere) center
              (radius sphere) (v3:length (v3:- max center)))))
    sphere))
