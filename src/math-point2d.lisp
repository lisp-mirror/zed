(in-package #:cl-user)

(defpackage #:zed.math.point2d
  (:local-nicknames
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2))
  (:use #:cl)
  (:import-from
   #:zed.math.vector2
   #:x
   #:y)
  (:export
   #:distance
   #:distance-squared
   #:find-min-max
   #:point
   #:translate
   #:x
   #:y))

(in-package #:zed.math.point2d)

(deftype point () 'v2:vec)

(u:fn-> point (u:f32 u:f32) point)
(declaim (inline point))
(defun point (x y)
  (declare (optimize speed))
  (v2:vec x y))

(u:fn-> translate (point v2:vec u:f32) point)
(declaim (inline translate))
(defun translate (point direction distance)
  (declare (optimize speed))
  (v2:+ point (v2:scale direction distance)))

(u:fn-> distance-squared (point point) u:f32)
(declaim (inline distance-squared))
(defun distance-squared (point1 point2)
  (declare (optimize speed))
  (v2:length-squared (v2:- point2 point1)))

(u:fn-> distance (point point) u:f32)
(declaim (inline distance))
(defun distance (point1 point2)
  (declare (optimize speed))
  (sqrt (distance-squared point1 point2)))

(u:fn-> find-min-max (simple-vector) (values point point))
(defun find-min-max (points)
  (declare (optimize speed)
           (simple-vector points))
  (let ((min (v2:uniform most-positive-single-float))
        (max (v2:uniform most-negative-single-float)))
    (dotimes (i (length points))
      (let ((x (svref points i)))
        (v2:min! min min x)
        (v2:max! max max x)))
    (values min max)))
