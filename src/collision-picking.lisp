(in-package #:zed)

(defstruct (picker
            (:predicate nil)
            (:copier nil))
  (start (v3:zero) :type v3:vec)
  (end (v3:zero) :type v3:vec))

(defgeneric pick-collision-volume (system volume)
  (:method (system volume)))

(defmethod pick-collision-volume (system (volume collision-volume-sphere))
  (declare (optimize speed))
  (let* ((picker (collision-system-picker system))
         (center (collision-volume-world-center volume))
         (start (line3d:start picker))
         (direction (line3d:direction picker))
         (m (v3:- start center))
         (b (v3:dot m direction))
         (c (- (v3:length-squared m)
               (expt (collision-volume-sphere-radius volume) 2))))
    (unless (and (plusp c) (plusp b))
      (let ((discriminant (- (expt b 2) c)))
        (unless (minusp discriminant)
          (let ((x (max 0.0 (- (- b) (sqrt discriminant)))))
            (when (<= x (line3d:length picker))
              x)))))))

(defmethod pick-collision-volume (system (volume collision-volume-box))
  (declare (optimize speed))
  (let* ((picker (collision-system-picker system))
         (game-object (trait-owner (collision-volume-collider volume)))
         (center (collision-volume-center volume))
         (start (v3:- (transform-point game-object (line3d:start picker) :space :world)
                      center))
         (end (v3:- (transform-point game-object (line3d:end picker) :space :world)
                    center))
         (line (line3d:line :start start :end end))
         (direction (line3d:direction line))
         (t-min 0.0)
         (t-max (line3d:length line)))
    (declare (u:f32 t-min t-max))
    (dotimes (i 3)
      (let ((p-i (aref start i))
            (min-i (aref (collision-volume-box-min volume) i))
            (max-i (aref (collision-volume-box-max volume) i)))
        (if (< (abs (aref direction i)) 1e-7)
            (when (or (< p-i min-i)
                      (> p-i max-i))
              (return-from pick-collision-volume nil))
            (let* ((inv-d (/ (aref direction i)))
                   (t1 (* (- min-i p-i) inv-d))
                   (t2 (* (- max-i p-i) inv-d)))
              (when (> t1 t2)
                (rotatef t1 t2))
              (setf t-min (max t-min t1)
                    t-max (min t-max t2))
              (when (> t-min t-max)
                (return-from pick-collision-volume nil))))))
    t-min))

(u:fn-> update-collision-picker (context u:b16 u:b16) null)
(defun update-collision-picker (context x y)
  (declare (optimize speed))
  (u:when-let* ((collision-system (context-collision-system context))
                (viewport-manager (context-viewports context))
                (viewport (find-viewport-by-coordinates viewport-manager x y))
                (picker (collision-system-picker collision-system))
                (start (line3d:start picker))
                (end (line3d:end picker))
                (camera (context-active-camera context))
                (view (tr.cam::view camera))
                (projection (tr.cam::projection camera))
                (viewport (v4:vec (float (viewport-x viewport) 1f0)
                                  (float (viewport-y viewport) 1f0)
                                  (float (viewport-width viewport) 1f0)
                                  (float (viewport-height viewport) 1f0)))
                (mx (float x 1f0))
                (my (float y 1f0)))
    (v3:copy! start (p3:unproject (p3:point mx my 0.0) view projection viewport))
    (v3:copy! end (p3:unproject (p3:point mx my 1.0) view projection viewport))
    nil))

(u:fn-> pick-game-object (context) (or game-object null))
(defun pick-game-object (context)
  (u:mvlet* ((active-traits (trait-manager-active-by-type (context-trait-manager context)))
             (collision-system (context-collision-system context))
             (mx my (get-mouse-position context))
             (picked nil))
    (update-collision-picker context mx my)
    (u:do-hash-keys (k (u:href active-traits 'tr.col:collider))
      (u:when-let* ((volume (tr.col::volume k))
                    (n (pick-collision-volume collision-system volume)))
        (when (tr.col::picked-hook k)
          (push (cons n k) picked))))
    (when picked
      (let* ((collider (cdar (stable-sort picked #'< :key #'car)))
             (game-object (trait-owner collider)))
        (funcall (tr.col::picked-hook collider) game-object)
        game-object))))
