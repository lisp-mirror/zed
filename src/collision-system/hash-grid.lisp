(in-package #:zed)

(defstruct (hash-grid
            (:constructor %make-hash-grid)
            (:predicate nil)
            (:copier nil))
  (cell-size 32 :type u:positive-fixnum)
  (bucket-size 1024 :type u:ub32)
  (buckets nil :type (simple-array t (*))))

(u:define-constant +hash-grid-x+ #x8da6b343)
(u:define-constant +hash-grid-y+ #xd8163841)
(u:define-constant +hash-grid-z+ #xcb1ab31f)

(u:fn-> make-hash-grid (&key (:cell-size u:positive-fixnum) (:bucket-size u:ub32)) hash-grid)
(defun make-hash-grid (&key (cell-size 32) (bucket-size 1024))
  (declare (optimize speed))
  (let ((buckets (make-array bucket-size)))
    (dotimes (i bucket-size)
      (setf (aref buckets i) (make-array 8 :fill-pointer 0 :adjustable t)))
    (%make-hash-grid :cell-size cell-size
                     :bucket-size bucket-size
                     :buckets buckets)))

(u:fn-> hash-grid-coordinates (hash-grid u:b32 u:b32 u:b32) u:array-index)
(declaim (inline hash-grid-coordinates))
(defun hash-grid-coordinates (grid x y z)
  (declare (optimize speed))
  (mod (+ (logand (* +hash-grid-x+ x) #.(1- (expt 2 32)))
          (logand (* +hash-grid-y+ y) #.(1- (expt 2 32)))
          (logand (* +hash-grid-z+ z) #.(1- (expt 2 32))))
       (hash-grid-bucket-size grid)))

(u:fn-> hash-grid-insert (hash-grid volume) null)
(defun hash-grid-insert (grid volume)
  (declare (optimize speed))
  (let* ((buckets (hash-grid-buckets grid))
         (cell-size (hash-grid-cell-size grid))
         (broad-geometry (volume-broad-geometry volume))
         (min (map '(vector fixnum 3)
                   (lambda (x)
                     (declare (fix-sf60 x))
                     (floor x cell-size))
                   (aabb:min broad-geometry)))
         (max (map '(vector fixnum 3)
                   (lambda (x)
                     (declare (fix-sf60 x))
                     (floor x cell-size))
                   (aabb:max broad-geometry))))
    (declare (dynamic-extent min max))
    (loop :for z :from (aref min 2) :to (aref max 2)
          :do (loop :for y :from (aref min 1) :to (aref max 1)
                    :do (loop :for x :from (aref min 0) :to (aref max 0)
                              :for hash = (hash-grid-coordinates grid x y z)
                              :do (vector-push-extend volume (aref buckets hash)))))))
