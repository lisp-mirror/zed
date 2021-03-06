(in-package #:zed)

(deftype transform-space () '(member :local :world))

(u:fn-> transform-game-object (game-object u:f32) null)
(defun transform-game-object (game-object delta)
  (declare (optimize speed))
  (let ((state (game-object-transform game-object)))
    (transform-translate-state state delta)
    (transform-rotate-state state delta)
    (transform-scale-state state delta)
    nil))

(u:fn-> resolve-local-matrix (transform-state u:f32) m4:mat)
(defun resolve-local-matrix (state factor)
  (declare (optimize speed))
  (let ((translation (transform-state-translation/interpolated state))
        (rotation (transform-state-rotation/interpolated state))
        (scale (transform-state-scale/interpolated state))
        (local-matrix (transform-state-local-matrix state))
        (scaling-matrix (load-time-value (m4:id))))
    (v3:lerp! translation
              (transform-state-translation/previous state)
              (transform-state-translation/current state)
              factor)
    (q:slerp! rotation
              (transform-state-rotation/previous state)
              (transform-state-rotation/current state)
              factor)
    (v3:lerp! scale
              (transform-state-scale/previous state)
              (transform-state-scale/current state)
              factor)
    (q:to-mat4! local-matrix rotation)
    (m4:set-scale! scaling-matrix m4:+id+ scale)
    (m4:*! local-matrix local-matrix scaling-matrix)
    (m4:set-translation! local-matrix local-matrix translation)
    local-matrix))

(u:fn-> resolve-world-matrix (game-object u:f32) null)
(defun resolve-world-matrix (game-object factor)
  (declare (optimize speed))
  (let ((parent (game-object-parent game-object))
        (state (game-object-transform game-object)))
    (m4:*! (transform-state-world-matrix state)
           (transform-state-world-matrix (game-object-transform parent))
           (resolve-local-matrix state factor))
    nil))

(u:fn-> get-translation (game-object &key (:space transform-space)) v3:vec)
(defun get-translation (game-object &key (space :local))
  (declare (optimize speed))
  (m4:get-translation
   (ecase space
     (:local (transform-state-local-matrix (game-object-transform game-object)))
     (:world (transform-state-world-matrix (game-object-transform game-object))))))

(u:fn-> get-rotation (game-object &key (:space transform-space)) q:quat)
(defun get-rotation (game-object &key (space :local))
  (declare (optimize speed))
  (q:from-mat4
   (ecase space
     (:local (transform-state-local-matrix (game-object-transform game-object)))
     (:world (transform-state-world-matrix (game-object-transform game-object))))))

(u:fn-> get-scale (game-object &key (:space transform-space)) v3:vec)
(defun get-scale (game-object &key (space :local))
  (declare (optimize speed))
  (m4:get-scale
   (ecase space
     (:local (transform-state-local-matrix (game-object-transform game-object)))
     (:world (transform-state-world-matrix (game-object-transform game-object))))))

(u:fn-> get-transform (game-object &key (:space transform-space)) m4:mat)
(defun get-transform (game-object &key (space :local))
  (declare (optimize speed))
  (ecase space
    (:local (transform-state-local-matrix (game-object-transform game-object)))
    (:world (transform-state-world-matrix (game-object-transform game-object)))))

(u:fn-> translate (game-object v3:vec &key (:replace-p boolean) (:instant-p boolean)) null)
(defun translate (game-object vec &key replace-p instant-p)
  (declare (optimize speed))
  (let* ((state (game-object-transform game-object))
         (current (transform-state-translation/current state)))
    (v3:+! current (if replace-p v3:+zero+ current) vec)
    (when instant-p
      (v3:copy! (transform-state-translation/previous state) current))
    nil))

(u:fn-> translate/clamp (game-object v3:vec v3:vec &key (:instant-p boolean)) null)
(defun translate/clamp (game-object min max &key instant-p)
  (declare (optimize speed))
  (let* ((state (game-object-transform game-object))
         (current (transform-state-translation/current state)))
    (v3:max! current current min)
    (v3:min! current current max)
    (when instant-p
      (v3:copy! (transform-state-translation/previous state) current))
    nil))

(u:fn-> translate/velocity (game-object v3:vec u:f32) null)
(defun translate/velocity (game-object axis rate)
  (declare (optimize speed))
  (let ((state (game-object-transform game-object)))
    (v3:velocity! (transform-state-translation/incremental state) axis rate)
    nil))

(u:fn-> rotate (game-object q:quat &key (:replace-p boolean) (:instant-p boolean)) null)
(defun rotate (game-object quat &key replace-p instant-p)
  (declare (optimize speed))
  (let* ((state (game-object-transform game-object))
         (current (transform-state-rotation/current state)))
    (q:rotate! current (if replace-p q:+id+ current) quat)
    (when instant-p
      (q:copy! (transform-state-rotation/previous state) current))
    nil))

(u:fn-> rotate/velocity (game-object v3:vec u:f32) null)
(defun rotate/velocity (game-object axis rate)
  (declare (optimize speed))
  (let ((state (game-object-transform game-object)))
    (v3:velocity! (transform-state-rotation/incremental state) axis rate)
    nil))

(u:fn-> scale (game-object v3:vec &key (:replace-p boolean) (:instant-p boolean)) null)
(defun scale (game-object vec &key replace-p instant-p)
  (declare (optimize speed))
  (let* ((state (game-object-transform game-object))
         (current (transform-state-scale/current state)))
    (v3:+! current (if replace-p v3:+zero+ current) vec)
    (when instant-p
      (v3:copy! (transform-state-scale/previous state) current))
    nil))

(u:fn-> scale/velocity (game-object v3:vec u:f32) null)
(defun scale/velocity (game-object axis rate)
  (declare (optimize speed))
  (let ((state (game-object-transform game-object)))
    (v3:velocity! (transform-state-scale/incremental state) axis rate)
    nil))

(u:fn-> transform-point! (game-object v3:vec v3:vec &key (:space transform-space)) v3:vec)
(declaim (inline transform-point!))
(defun transform-point! (game-object point out &key (space :local))
  (declare (optimize speed))
  (v4:with-components ((v point))
    (let ((model (m4:copy (transform-state-world-matrix (game-object-transform game-object))))
          (temp (v4:vec vx vy vz 1)))
      (declare (dynamic-extent model temp))
      (when (eq space :world)
        (m4:invert! model model))
      (m4:*v4! temp model temp)
      (v4:with-components ((v temp))
        (v3:with-components ((o out))
          (setf ox vx
                oy vy
                oz vz)
          out)))))

(u:fn-> transform-point (game-object v3:vec &key (:space transform-space)) v3:vec)
(defun transform-point (game-object point &key (space :local))
  (declare (optimize speed))
  (transform-point! game-object point (v3:zero) :space space))

(u:fn-> transform-vector! (game-object v3:vec v3:vec &key (:space transform-space)) v3:vec)
(declaim (inline transform-vector!))
(defun transform-vector! (game-object vector out &key (space :local))
  (declare (optimize speed))
  (v4:with-components ((v vector))
    (let ((model (m4:copy (transform-state-world-matrix (game-object-transform game-object))))
          (temp (v4:vec vx vy vz 1)))
      (declare (dynamic-extent model temp))
      (m4:set-translation! model model v3:+zero+)
      (when (eq space :world)
        (m4:invert! model model))
      (m4:*v4! temp model temp)
      (v4:with-components ((v temp))
        (v3:with-components ((o out))
          (setf ox vx
                oy vy
                oz vz)
          out)))))

(u:fn-> transform-vector (game-object v3:vec &key (:space transform-space)) v3:vec)
(defun transform-vector (game-object vector &key (space :local))
  (declare (optimize speed))
  (transform-vector! game-object vector (v3:zero) :space space))

(u:fn-> transform-direction! (game-object v3:vec v3:vec &key (:space transform-space)) v3:vec)
(declaim (inline transform-direction!))
(defun transform-direction! (game-object direction out &key (space :local))
  (declare (optimize speed))
  (v4:with-components ((v direction))
    (let ((model (m4:copy (transform-state-world-matrix (game-object-transform game-object))))
          (temp (v4:vec vx vy vz 1)))
      (declare (dynamic-extent model temp))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (when (eq space :world)
        (m4:invert! model model))
      (m4:*v4! temp model temp)
      (v4:with-components ((v temp))
        (v3:with-components ((o out))
          (setf ox vx
                oy vy
                oz vz)
          out)))))

(u:fn-> transform-direction (game-object v3:vec &key (:space transform-space)) v3:vec)
(defun transform-direction (game-object direction &key (space :local))
  (declare (optimize speed))
  (transform-direction! game-object direction (v3:zero) :space space))
