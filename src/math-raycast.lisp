(in-package #:zed.math.raycast)

(u:fn-> ray/sphere (ray:ray sphere:sphere) (or u:f32 null))
(defun ray/sphere (ray sphere)
  (declare (optimize speed))
  (let* ((e (v3:- (sphere:origin sphere) (ray:origin ray)))
         (radius-squared (expt (sphere:radius sphere) 2))
         (e-squared (v3:length-squared e))
         (a (v3:dot e (ray:direction ray)))
         (b (- e-squared (* a a)))
         (f (sqrt (abs (- radius-squared b)))))
    (cond
      ((minusp (- radius-squared b))
       nil)
      ((< e-squared radius-squared)
       (+ a f))
      (t
       (- a f)))))

(u:fn-> ray/aabb (ray:ray aabb:aabb) (or u:f32 null))
(defun ray/aabb (ray aabb)
  (declare (optimize speed))
  (v3:with-components ((min- (aabb:min aabb))
                       (max- (aabb:max aabb))
                       (o (ray:origin ray))
                       (d (ray:direction ray)))
    (let* ((dx (if (com:= dx 0.0 1e-7 1e-7) 1e-7 dx))
           (dy (if (com:= dy 0.0 1e-7 1e-7) 1e-7 dy))
           (dz (if (com:= dz 0.0 1e-7 1e-7) 1e-7 dz))
           (t1 (/ (- min-x ox) dx))
           (t2 (/ (- max-x ox) dx))
           (t3 (/ (- min-y oy) dy))
           (t4 (/ (- max-y oy) dy))
           (t5 (/ (- min-z oz) dz))
           (t6 (/ (- max-x oz) dz))
           (t-min (max (min t1 t2) (min t3 t4) (min t5 t6)))
           (t-max (min (max t1 t2) (max t3 t4) (max t5 t6))))
      (cond
        ((or (minusp t-max)
             (> t-min t-max))
         nil)
        ((minusp t-min)
         t-max)
        (t
         t-min)))))

(u:fn-> ray/obb (ray:ray obb:obb) (or u:f32 null))
(defun ray/obb (ray obb)
  (declare (optimize speed))
  (let* ((rotation (obb:rotation obb))
         (x-axis (m3:get-column rotation 0))
         (y-axis (m3:get-column rotation 1))
         (z-axis (m3:get-column rotation 2))
         (dir (ray:direction ray))
         (p (v3:- (obb:origin obb) (ray:origin ray)))
         (f (v3:vec (v3:dot x-axis dir) (v3:dot y-axis dir) (v3:dot z-axis dir)))
         (e (v3:vec (v3:dot x-axis p) (v3:dot y-axis p) (v3:dot z-axis p)))
         (x (u:make-f32-array 6)))
    (declare (dynamic-extent x-axis y-axis z-axis p f e x))
    (dotimes (i 3)
      (let ((ei (aref e i))
            (hi (aref (obb:size obb) i)))
        (when (zerop (aref f i))
          (when (or (plusp (- (- ei) hi))
                    (minusp (+ (- ei) hi)))
            (return-from ray/obb nil))
          (setf (aref f i) 1e-7))
        (let ((fi (aref f i)))
          (setf (aref x (* i 2)) (/ (+ ei hi) fi)
                (aref x (1+ (* i 2))) (/ (- ei hi) fi)))))
    (let* ((x0 (aref x 0))
           (x1 (aref x 1))
           (x2 (aref x 2))
           (x3 (aref x 3))
           (x4 (aref x 4))
           (x5 (aref x 5))
           (t-min (max (min x0 x1) (min x2 x3) (min x4 x5)))
           (t-max (min (max x0 x1) (max x2 x3) (max x4 x5))))
      (cond
        ((or (minusp t-max)
             (> t-min t-max))
         nil)
        ((minusp t-min)
         t-max)
        (t
         t-min)))))
