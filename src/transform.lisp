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
        (scaling-matrix (transform-state-scaling-matrix state)))
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
  (u:when-let ((parent (game-object-parent game-object))
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
    (setf (transform-state-translation/incremental state) (v3:velocity axis rate))
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
    (setf (transform-state-rotation/incremental state) (v3:velocity axis rate))
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
    (setf (transform-state-scale/incremental state) (v3:velocity axis rate))
    nil))

(u:fn-> transform-point (game-object v3:vec &key (:space transform-space)) v3:vec)
(defun transform-point (game-object point &key (space :local))
  (declare (optimize speed))
  (let* ((world-matrix (transform-state-world-matrix (game-object-transform game-object)))
         (temp1 (v4:vec (v3:x point) (v3:y point) (v3:z point) 1.0))
         (temp2 (ecase space
                  (:local (m4:*v4 world-matrix temp1))
                  (:world (m4:*v4 (m4:invert world-matrix) temp1)))))
    (declare (dynamic-extent temp1))
    (v3:vec (v4:x temp2) (v4:y temp2) (v4:z temp2))))

(u:fn-> transform-vector (game-object v3:vec &key (:space transform-space)) v3:vec)
(defun transform-vector (game-object vector &key (space :local))
  (declare (optimize speed))
  (let* ((world-matrix (transform-state-world-matrix (game-object-transform game-object)))
         (temp1 (v4:vec (v3:x vector) (v3:y vector) (v3:z vector) 1.0))
         (temp2 (ecase space
                  (:local (m4:*v4 world-matrix temp1))
                  (:world (m4:*v4 (m4:invert world-matrix) temp1)))))
    (declare (dynamic-extent temp1))
    (m4:set-translation! world-matrix world-matrix v3:+zero+)
    (v3:vec (v4:x temp2) (v4:y temp2) (v4:z temp2))))

(u:fn-> transform-direction (game-object v3:vec &key (:space transform-space)) v3:vec)
(defun transform-direction (game-object direction &key (space :local))
  (declare (optimize speed))
  (let* ((world-matrix (transform-state-world-matrix (game-object-transform game-object)))
         (temp1 (v4:vec (v3:x direction) (v3:y direction) (v3:z direction) 1.0))
         (temp2 (ecase space
                  (:local (m4:*v4 world-matrix temp1))
                  (:world (m4:*v4 (m4:invert world-matrix) temp1)))))
    (declare (dynamic-extent temp1))
    (m4:set-translation! world-matrix world-matrix v3:+zero+)
    (m4:normalize-rotation! world-matrix world-matrix)
    (v3:vec (v4:x temp2) (v4:y temp2) (v4:z temp2))))
