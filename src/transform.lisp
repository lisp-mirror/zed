(in-package #:cl-user)

(defpackage #:%zed.transform
  ;; Third-party aliases
  (:local-nicknames
   (#:m3 #:origin.mat3)
   (#:m4 #:origin.mat4)
   (#:q #:origin.quat)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4))
  ;; Internal aliases
  (:local-nicknames
   (#:gob #:%zed.game-object)
   (#:trs #:%zed.transform-state))
  (:use #:cl)
  (:shadow
   #:space))

(in-package #:%zed.transform)

(deftype space () '(member :local :world))

(u:fn-> transform-game-object (gob::game-object u:f32) null)
(defun transform-game-object (game-object delta)
  (declare (optimize speed))
  (let ((state (gob::transform game-object)))
    (trs::transform/translation state delta)
    (trs::transform/rotation state delta)
    (trs::transform/scale state delta)
    nil))

(u:fn-> resolve-local-matrix (trs::state u:f32) m4:mat)
(defun resolve-local-matrix (state factor)
  (declare (optimize speed))
  (let ((translation (trs::translation/interpolated state))
        (rotation (trs::rotation/interpolated state))
        (scale (trs::scale/interpolated state))
        (local-matrix (trs::local-matrix state))
        (scaling-matrix (trs::scaling-matrix state)))
    (v3:lerp! translation (trs::translation/previous state) (trs::translation/current state) factor)
    (q:slerp! rotation (trs::rotation/previous state) (trs::rotation/current state) factor)
    (v3:lerp! scale (trs::scale/previous state) (trs::scale/current state) factor)
    (q:to-mat4! local-matrix rotation)
    (m4:set-scale! scaling-matrix m4:+id+ scale)
    (m4:*! local-matrix local-matrix scaling-matrix)
    (m4:set-translation! local-matrix local-matrix translation)
    local-matrix))

(u:fn-> resolve-world-matrix (gob::game-object u:f32) null)
(defun resolve-world-matrix (game-object factor)
  (declare (optimize speed))
  (u:when-let ((parent (gob::parent game-object))
               (state (gob::transform game-object)))
    (m4:*! (trs::world-matrix state)
           (trs::world-matrix (gob::transform parent))
           (resolve-local-matrix state factor))
    nil))

(u:fn-> resolve-normal-matrix (gob::game-object) m3:mat)
;; TODO: Need a camera implemented for this.
#++(defun resolve-normal-matrix (game-object)
     (declare (optimize speed))
     (let* ((state (gob::transform game-object))
            (normal-matrix (trs::normal-matrix state)))
       (u:when-let ((camera nil))
         (m4:set-translation! normal-matrix (trs::world-matrix state) v3:+zero+)
         (m4:*! normal-matrix (camera-view camera) normal-matrix)
         (m4:invert! normal-matrix normal-matrix)
         (m4:transpose! normal-matrix normal-matrix))
       (m4:rotation-to-mat3 normal-matrix)))

(u:fn-> get-translation (gob::game-object &key (:space space)) v3:vec)
(defun get-translation (game-object &key (space :local))
  (declare (optimize speed))
  (m4:get-translation
   (ecase space
     (:local (trs::local-matrix (gob::transform game-object)))
     (:world (trs::world-matrix (gob::transform game-object))))))

(u:fn-> get-rotation (gob::game-object &key (:space space)) q:quat)
(defun get-rotation (game-object &key (space :local))
  (declare (optimize speed))
  (q:from-mat4
   (ecase space
     (:local (trs::local-matrix (gob::transform game-object)))
     (:world (trs::world-matrix (gob::transform game-object))))))

(u:fn-> get-scale (gob::game-object &key (:space space)) v3:vec)
(defun get-scale (game-object &key (space :local))
  (declare (optimize speed))
  (m4:get-scale
   (ecase space
     (:local (trs::local-matrix (gob::transform game-object)))
     (:world (trs::world-matrix (gob::transform game-object))))))

(u:fn-> translate (gob::game-object v3:vec &key (:replace-p boolean) (:instant-p boolean)) null)
(defun translate (game-object vec &key replace-p instant-p)
  (declare (optimize speed))
  (let* ((state (gob::transform game-object))
         (current (trs::translation/current state)))
    (v3:+! current (if replace-p v3:+zero+ current) vec)
    (when instant-p
      ;; TODO: Maybe implement deferred updates.
      (v3:copy! (trs::translation/previous state) current))
    nil))

(u:fn-> translate/clamp (gob::game-object v3:vec v3:vec &key (:instant-p boolean)) null)
(defun translate/clamp (game-object min max &key instant-p)
  (declare (optimize speed))
  (let* ((state (gob::transform game-object))
         (current (trs::translation/current state)))
    (v3:max! current current min)
    (v3:min! current current max)
    ;; TODO: Maybe implement deferred updates.
    (when instant-p
      (v3:copy! (trs::translation/previous state) current))
    nil))

(u:fn-> translate/velocity (gob::game-object v3:vec u:f32) null)
(defun translate/velocity (game-object axis rate)
  (declare (optimize speed))
  (let ((state (gob::transform game-object)))
    (setf (trs::translation/incremental state) (v3:velocity axis rate))
    nil))

(u:fn-> rotate (gob::game-object q:quat &key (:replace-p boolean) (:instant-p boolean)) null)
(defun rotate (game-object quat &key replace-p instant-p)
  (declare (optimize speed))
  (let* ((state (gob::transform game-object))
         (current (trs::rotation/current state)))
    (q:rotate! current (if replace-p q:+id+ current) quat)
    (when instant-p
      ;; TODO: Maybe implement deferred updates.
      (q:copy! (trs::rotation/previous state) current))
    nil))

(u:fn-> rotate/velocity (gob::game-object v3:vec u:f32) null)
(defun rotate/velocity (game-object axis rate)
  (declare (optimize speed))
  (let ((state (gob::transform game-object)))
    (setf (trs::rotation/incremental state) (v3:velocity axis rate))
    nil))

(u:fn-> scale (gob::game-object v3:vec &key (:replace-p boolean) (:instant-p boolean)) null)
(defun scale (game-object vec &key replace-p instant-p)
  (declare (optimize speed))
  (let* ((state (gob::transform game-object))
         (current (trs::scale/current state)))
    (v3:+! current (if replace-p v3:+zero+ current) vec)
    (when instant-p
      ;; TODO: Maybe implement deferred updates.
      (v3:copy! (trs::scale/previous state) current))
    nil))

(u:fn-> scale/velocity (gob::game-object v3:vec u:f32) null)
(defun scale/velocity (game-object axis rate)
  (declare (optimize speed))
  (let ((state (gob::transform game-object)))
    (setf (trs::scale/incremental state) (v3:velocity axis rate))
    nil))

(u:fn-> transform-point (gob::game-object v3:vec &key (:space space)) v3:vec)
(defun transform-point (game-object point &key (space :local))
  (declare (optimize speed))
  (let ((world-matrix (trs::world-matrix (gob::transform game-object))))
    (values
     (v3:vec
      (ecase space
        (:local (m4:*v4 world-matrix (v4:vec point 1)))
        (:world (m4:*v4 (m4:invert world-matrix) (v4:vec point 1))))))))

(u:fn-> transform-vector (gob::game-object v3:vec &key (:space space)) v3:vec)
(defun transform-vector (game-object vector &key (space :local))
  (declare (optimize speed))
  (let ((world-matrix (trs::world-matrix (gob::transform game-object))))
    (m4:set-translation! world-matrix world-matrix v3:+zero+)
    (values
     (v3:vec
      (ecase space
        (:local (m4:*v4 world-matrix (v4:vec vector 1)))
        (:world (m4:*v4 (m4:invert world-matrix) (v4:vec vector 1))))))))

(u:fn-> transform-direction (gob::game-object v3:vec &key (:space space)) v3:vec)
(defun transform-direction (game-object direction &key (space :local))
  (declare (optimize speed))
  (let ((world-matrix (trs::world-matrix (gob::transform game-object))))
    (m4:set-translation! world-matrix world-matrix v3:+zero+)
    (m4:normalize-rotation! world-matrix world-matrix)
    (values
     (v3:vec
      (ecase space
        (:local (m4:*v4 world-matrix (v4:vec direction 1)))
        (:world (m4:*v4 (m4:invert world-matrix) (v4:vec direction 1))))))))
