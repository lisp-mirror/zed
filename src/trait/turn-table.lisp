(in-package #:zed.trait.turn-table)

(z::define-internal-trait turn-table (:order (:before render))
  ((%rotation-speed :reader rotation-speed
                    :inline t
                    :type u:f32
                    :initarg :rotation-speed
                    :initform 0.005)
   (%rotation-button :reader rotation-button
                     :inline t
                     :type (member :left :middle :right)
                     :initarg :rotation-button
                     :initform :left)
   (%dragging-p :accessor dragging-p
                :inline t
                :type boolean
                :initform nil)
   (%drag-start :accessor drag-start
                :inline t
                :type v2:vec
                :initform (v2:zero))
   (%drag-end :accessor drag-end
              :inline t
              :type v2:vec
              :initform (v2:zero))
   (%initial :accessor initial
             :inline t
             :type q:quat
             :initform (q:id))
   (%rotation-vector :accessor rotation-vector
                     :inline t
                     :type v2:vec
                     :initform (v2:zero)))
  (:attach attach)
  (:update update))

(u:fn-> start-rotation (turn-table) null)
(defun start-rotation (turn-table)
  (declare (optimize speed))
  (u:mvlet* ((context (z:trait-context turn-table))
             (x y (z:get-mouse-position context)))
    (setf (drag-start turn-table) (v2:vec (float x 1f0) (float y 1f0))
          (dragging-p turn-table) t)
    nil))

(u:fn-> rotate (turn-table) null)
(defun rotate (turn-table)
  (declare (optimize speed))
  (u:mvlet* ((context (z:trait-context turn-table))
             (game-object (z:trait-owner turn-table))
             (x y (z:get-mouse-position context))
             (speed (rotation-speed turn-table)))
    (v2:with-components ((r (rotation-vector turn-table))
                         (v (v2:- (v2:vec (float x 1f0) (float y 1f0)) (drag-start turn-table))))
      (z:rotate game-object
                (q:rotate (q:orient :local :y (+ rx (* vx speed)) :x (- (+ ry (* vy speed))))
                          (initial turn-table))
                :replace-p t))
    nil))

(u:fn-> finish-rotation (turn-table) null)
(defun finish-rotation (turn-table)
  (declare (optimize speed))
  (u:mvlet* ((context (z:trait-context turn-table))
             (x y (z:get-mouse-position context))
             (vector (rotation-vector turn-table))
             (start (drag-start turn-table))
             (end (v2:vec (float x 1f0) (float y 1f0))))
    (setf (dragging-p turn-table) nil
          (drag-end turn-table) end)
    (v2:+! vector vector (v2:scale (v2:- end start) (rotation-speed turn-table)))
    nil))

(u:fn-> attach (turn-table) null)
(defun attach (turn-table)
  (declare (optimize speed))
  (let ((game-object (z:trait-owner turn-table)))
    (setf (initial turn-table) (q:copy (z:get-rotation game-object)))
    nil))

(u:fn-> update (turn-table) null)
(defun update (turn-table)
  (declare (optimize speed))
  (let* ((context (z:trait-context turn-table))
         (dragging-p (dragging-p turn-table))
         (button (rotation-button turn-table)))
    (cond
      ((and (z:on-button-enter context :mouse button)
            ;; Check if the source of the picked collider is the game object with the turn table.
            (u:when-let* ((picked (z::pick-game-object context))
                          (collider (z:find-trait picked 'tr.col:collider)))
              (eq (tr.col::source collider)
                  (z:trait-owner turn-table))))
       (start-rotation turn-table))
      ((and (z:on-button-exit context :mouse button)
            dragging-p)
       (finish-rotation turn-table))
      (dragging-p
       (rotate turn-table)))
    nil))
