(in-package #:cl-user)

(defpackage #:zed.math.vector4
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:const #:zed.math.constants)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:/
   #:random
   #:length
   #:round
   #:abs
   #:<
   #:<=
   #:>
   #:>=
   #:min
   #:max
   #:expt
   #:sqrt
   #:floor
   #:ceiling
   #:mod
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan)
  (:export
   #:vec
   #:x
   #:y
   #:z
   #:w
   #:with-components
   #:+zero+
   #:+ones+
   #:zero
   #:zero!
   #:zero-p
   #:ones!
   #:ones
   #:uniform!
   #:uniform
   #:random!
   #:random
   #:copy!
   #:copy
   #:sign!
   #:sign
   #:fract!
   #:fract
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:=
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:/!
   #:/
   #:scale!
   #:scale
   #:invert!
   #:invert
   #:dot
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:round!
   #:round
   #:abs!
   #:abs
   #:negate!
   #:negate
   #:angle
   #:direction=
   #:parallel-p
   #:lerp!
   #:lerp
   #:<
   #:<=
   #:>
   #:>=
   #:min!
   #:min
   #:max!
   #:max
   #:radians!
   #:radians
   #:degrees!
   #:degrees
   #:expt!
   #:expt
   #:sqrt!
   #:sqrt
   #:floor!
   #:floor
   #:ceiling!
   #:ceiling
   #:mod!
   #:mod
   #:sin!
   #:sin
   #:cos!
   #:cos
   #:tan!
   #:tan
   #:asin!
   #:asin
   #:acos!
   #:acos
   #:atan!
   #:atan))

(in-package #:zed.math.vector4)

(deftype vec () '(u:f32a 4))

(defmacro with-components (((prefix vec) &rest rest) &body body)
  (u:once-only (vec)
    `(symbol-macrolet
         ((,prefix ,vec)
          (,(com::make-accessor-symbol prefix "X") (aref ,vec 0))
          (,(com::make-accessor-symbol prefix "Y") (aref ,vec 1))
          (,(com::make-accessor-symbol prefix "Z") (aref ,vec 2))
          (,(com::make-accessor-symbol prefix "W") (aref ,vec 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(u:fn-> vec (u:f32 u:f32 u:f32 u:f32) vec)
(declaim (inline vec))
(u:eval-always
  (defun vec (x y z w)
    (declare (optimize speed))
    (let ((vec (u:make-f32-array 4)))
      (setf (aref vec 0) x
            (aref vec 1) y
            (aref vec 2) z
            (aref vec 3) w)
      vec)))

(u:fn-> x (vec) u:f32)
(declaim (inline x))
(defun x (vec)
  (declare (optimize speed))
  (aref vec 0))

(u:fn-> (setf x) (u:f32 vec) u:f32)
(declaim (inline (setf x)))
(defun (setf x) (value vec)
  (declare (optimize speed))
  (setf (aref vec 0) value))

(u:fn-> y (vec) u:f32)
(declaim (inline y))
(defun y (vec)
  (declare (optimize speed))
  (aref vec 1))

(u:fn-> (setf y) (u:f32 vec) u:f32)
(declaim (inline (setf y)))
(defun (setf y) (value vec)
  (declare (optimize speed))
  (setf (aref vec 1) value))

(u:fn-> z (vec) u:f32)
(declaim (inline z))
(defun z (vec)
  (declare (optimize speed))
  (aref vec 2))

(u:fn-> (setf z) (u:f32 vec) u:f32)
(declaim (inline (setf z)))
(defun (setf z) (value vec)
  (declare (optimize speed))
  (setf (aref vec 2) value))

(u:fn-> w (vec) u:f32)
(declaim (inline w))
(defun w (vec)
  (declare (optimize speed))
  (aref vec 3))

(u:fn-> (setf w) (u:f32 vec) u:f32)
(declaim (inline (setf w)))
(defun (setf w) (value vec)
  (declare (optimize speed))
  (setf (aref vec 3) value))

(u:define-constant +zero+ (vec 0.0 0.0 0.0 0.0) :test #'equalp)

(u:define-constant +ones+ (vec 1.0 1.0 1.0 1.0) :test #'equalp)

(u:fn-> = (vec vec &key (:rel u:f32) (:abs u:f32)) boolean)
(declaim (inline =))
(defun = (vec1 vec2 &key (rel 1e-7) (abs rel))
  (declare (optimize speed))
  (com::cwcmp 4 (vec1 vec2) (com::= vec1 vec2 rel abs)))

(u:fn-> zero! (vec) vec)
(declaim (inline zero!))
(defun zero! (vec)
  (declare (optimize speed))
  (com::cwset 4 vec nil 0.0)
  vec)

(u:fn-> zero () vec)
(declaim (inline zero))
(defun zero ()
  (declare (optimize speed))
  (vec 0.0 0.0 0.0 0.0))

(u:fn-> zero-p (vec) boolean)
(declaim (inline zero-p))
(defun zero-p (vec)
  (declare (optimize speed))
  (= vec +zero+))

(u:fn-> ones! (vec) vec)
(declaim (inline ones!))
(defun ones! (vec)
  (declare (optimize speed))
  (com::cwset 4 vec nil 1.0)
  vec)

(u:fn-> ones () vec)
(declaim (inline ones))
(defun ones ()
  (declare (optimize speed))
  (vec 1.0 1.0 1.0 1.0))

(u:fn-> uniform! (vec u:f32) vec)
(declaim (inline uniform!))
(defun uniform! (vec value)
  (declare (optimize speed))
  (com::cwset 4 vec nil value)
  vec)

(u:fn-> uniform (u:f32) vec)
(declaim (inline uniform))
(defun uniform (value)
  (declare (optimize speed))
  (vec value value value value))

(u:fn-> random! (vec u:f32 u:f32) vec)
(declaim (inline random!))
(defun random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (com::cwset 4 out nil (cl:+ min (cl:random diff))))
  out)

(u:fn-> random (u:f32 u:f32) vec)
(declaim (inline random))
(defun random (min max)
  (declare (optimize speed))
  (random! (zero) min max))

(u:fn-> copy! (vec vec) vec)
(declaim (inline copy!))
(defun copy! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec vec)
  out)

(u:fn-> copy (vec) vec)
(declaim (inline copy))
(defun copy (vec)
  (declare (optimize speed))
  (copy! (zero) vec))

(u:fn-> sign! (vec vec) vec)
(declaim (inline sign!))
(defun sign! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (signum vec))
  out)

(u:fn-> sign (vec) vec)
(declaim (inline sign))
(defun sign (vec)
  (declare (optimize speed))
  (sign! (zero) vec))

(u:fn-> fract! (vec vec) vec)
(declaim (inline fract!))
(defun fract! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:- vec (ffloor vec)))
  out)

(u:fn-> fract (vec) vec)
(declaim (inline fract))
(defun fract (vec)
  (declare (optimize speed))
  (fract! (zero) vec))

(u:fn-> clamp! (vec vec vec vec) vec)
(declaim (inline clamp!))
(defun clamp! (out vec min max)
  (declare (optimize speed))
  (com::cwset 2 out (vec min max) (u:clamp vec min max))
  out)

(u:fn-> clamp (vec vec vec) vec)
(declaim (inline clamp))
(defun clamp (vec min max)
  (declare (optimize speed))
  (clamp! (zero) vec min max))

(u:fn-> clamp-range! (vec vec u:f32 u:f32) vec)
(declaim (inline clamp-range!))
(defun clamp-range! (out vec min max)
  (declare (optimize speed))
  (com::cwset 4 out vec (u:clamp vec min max))
  out)

(u:fn-> clamp-range (vec u:f32 u:f32) vec)
(declaim (inline clamp-range))
(defun clamp-range (vec min max)
  (declare (optimize speed))
  (clamp-range! (zero) vec min max))

(u:fn-> +! (vec vec vec) vec)
(declaim (inline +!))
(defun +! (out vec1 vec2)
  (declare (optimize speed))
  (com::cwset 4 out (vec1 vec2) (cl:+ vec1 vec2))
  out)

(u:fn-> + (vec vec) vec)
(declaim (inline +))
(defun + (vec1 vec2)
  (declare (optimize speed))
  (+! (zero) vec1 vec2))

(u:fn-> -! (vec vec vec) vec)
(declaim (inline -!))
(defun -! (out vec1 vec2)
  (declare (optimize speed))
  (com::cwset 4 out (vec1 vec2) (cl:- vec1 vec2))
  out)

(u:fn-> - (vec vec) vec)
(declaim (inline -))
(defun - (vec1 vec2)
  (declare (optimize speed))
  (-! (zero) vec1 vec2))

(u:fn-> *! (vec vec vec) vec)
(declaim (inline *!))
(defun *! (out vec1 vec2)
  (declare (optimize speed))
  (com::cwset 4 out (vec1 vec2) (cl:* vec1 vec2))
  out)

(u:fn-> * (vec vec) vec)
(declaim (inline *))
(defun * (vec1 vec2)
  (declare (optimize speed))
  (*! (zero) vec1 vec2))

(u:fn-> /! (vec vec vec) vec)
(declaim (inline /!))
(defun /! (out vec1 vec2)
  (declare (optimize speed))
  (com::cwset 4 out (vec1 vec2) (if (zerop vec2) 0.0 (cl:/ vec1 vec2)))
  out)

(u:fn-> / (vec vec) vec)
(declaim (inline /))
(defun / (vec1 vec2)
  (declare (optimize speed))
  (/! (zero) vec1 vec2))

(u:fn-> scale! (vec vec u:f32) vec)
(declaim (inline scale!))
(defun scale! (out vec scalar)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:* vec scalar))
  out)

(u:fn-> scale (vec u:f32) vec)
(declaim (inline scale))
(defun scale (vec scalar)
  (declare (optimize speed))
  (scale! (zero) vec scalar))

(u:fn-> invert! (vec vec) vec)
(declaim (inline invert!))
(defun invert! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (if (zerop vec) 0.0 (cl:/ vec)))
  out)

(u:fn-> invert (vec) vec)
(declaim (inline invert))
(defun invert (vec)
  (declare (optimize speed))
  (invert! (zero) vec))

(u:fn-> dot (vec vec) u:f32)
(declaim (inline dot))
(defun dot (vec1 vec2)
  (declare (optimize speed))
  (with-components ((v1 vec1) (v2 vec2))
    (cl:+ (cl:* v1x v2x) (cl:* v1y v2y) (cl:* v1z v2z) (cl:* v1w v2w))))

(u:fn-> length-squared (vec) u:f32)
(declaim (inline length-squared))
(defun length-squared (vec)
  (declare (optimize speed))
  (with-components ((v vec))
    (cl:+ (cl:expt vx 2) (cl:expt vy 2) (cl:expt vz 2) (cl:expt vw 2))))

(u:fn-> length (vec) u:f32)
(declaim (inline length))
(defun length (vec)
  (declare (optimize speed))
  (cl:sqrt (length-squared vec)))

(u:fn-> normalize! (vec vec) vec)
(declaim (inline normalize!))
(defun normalize! (out vec)
  (declare (optimize speed))
  (let ((length (length vec)))
    (unless (zerop length)
      (scale! out vec (cl:/ length))))
  out)

(u:fn-> normalize (vec) vec)
(declaim (inline normalize))
(defun normalize (vec)
  (declare (optimize speed))
  (normalize! (zero) vec))

(u:fn-> round! (vec vec) vec)
(declaim (inline round!))
(defun round! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (fround vec))
  out)

(u:fn-> round (vec) vec)
(declaim (inline round))
(defun round (vec)
  (declare (optimize speed))
  (round! (zero) vec))

(u:fn-> abs! (vec vec) vec)
(declaim (inline abs!))
(defun abs! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:abs vec))
  out)

(u:fn-> abs (vec) vec)
(declaim (inline abs))
(defun abs (vec)
  (declare (optimize speed))
  (abs! (zero) vec))

(u:fn-> negate! (vec vec) vec)
(declaim (inline negate!))
(defun negate! (out vec)
  (declare (optimize speed))
  (scale! out vec -1.0))

(u:fn-> negate (vec) vec)
(declaim (inline negate))
(defun negate (vec)
  (declare (optimize speed))
  (negate! (zero) vec))

(u:fn-> angle (vec vec) u:f32)
(declaim (inline angle))
(defun angle (vec1 vec2)
  (declare (optimize speed))
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (length vec1) (length vec2))))
    (if (zerop m*m)
        0.0
        (cl:acos (the (u:f32 -1.0 1.0) (cl:/ dot m*m))))))

(u:fn-> direction= (vec vec &key (:rel u:f32) (:abs u:f32)) boolean)
(declaim (inline direction=))
(defun direction= (vec1 vec2 &key (rel 1e-7) (abs rel))
  (declare (optimize speed))
  (com::= (dot (normalize vec1) (normalize vec2)) 1f0 rel abs))

(u:fn-> parallel-p (vec vec &key (:rel u:f32) (:abs u:f32)) boolean)
(declaim (inline parallel-p))
(defun parallel-p (vec1 vec2 &key (rel 1e-7) (abs rel))
  (declare (optimize speed))
  (com::= (cl:abs (dot (normalize vec1) (normalize vec2))) 1f0 rel abs))

(u:fn-> lerp! (vec vec vec u:f32) vec)
(declaim (inline lerp!))
(defun lerp! (out vec1 vec2 factor)
  (declare (optimize speed))
  (com::cwset 4 out (vec1 vec2) (u:lerp factor vec1 vec2))
  out)

(u:fn-> lerp (vec vec u:f32) vec)
(declaim (inline lerp))
(defun lerp (vec1 vec2 factor)
  (declare (optimize speed))
  (lerp! (zero) vec1 vec2 factor))

(u:fn-> < (vec vec &optional (member :and :or)) boolean)
(declaim (inline <))
(defun < (vec1 vec2 &optional (op :and))
  (declare (optimize speed))
  (if (eq op :and)
      (com::cwcmp 4 (vec1 vec2) (cl:< vec1 vec2))
      (com::cwcmp-or 4 (vec1 vec2) (cl:< vec1 vec2))))

(u:fn-> <= (vec vec &optional (member :and :or)) boolean)
(declaim (inline <=))
(defun <= (vec1 vec2 &optional (op :and))
  (declare (optimize speed))
  (if (eq op :and)
      (com::cwcmp 4 (vec1 vec2) (cl:<= vec1 vec2))
      (com::cwcmp-or 4 (vec1 vec2) (cl:<= vec1 vec2))))

(u:fn-> > (vec vec &optional (member :and :or)) boolean)
(declaim (inline >))
(defun > (vec1 vec2 &optional (op :and))
  (declare (optimize speed))
  (if (eq op :and)
      (com::cwcmp 4 (vec1 vec2) (cl:> vec1 vec2))
      (com::cwcmp-or 4 (vec1 vec2) (cl:> vec1 vec2))))

(u:fn-> >= (vec vec &optional (member :and :or)) boolean)
(declaim (inline >=))
(defun >= (vec1 vec2 &optional (op :and))
  (declare (optimize speed))
  (if (eq op :and)
      (com::cwcmp 4 (vec1 vec2) (cl:>= vec1 vec2))
      (com::cwcmp-or 4 (vec1 vec2) (cl:>= vec1 vec2))))

(u:fn-> min! (vec vec vec) vec)
(declaim (inline min!))
(defun min! (out vec1 vec2)
  (declare (optimize speed))
  (com::cwset 4 out (vec1 vec2) (cl:min vec1 vec2))
  out)

(u:fn-> min (vec vec) vec)
(declaim (inline min))
(defun min (vec1 vec2)
  (declare (optimize speed))
  (min! (zero) vec1 vec2))

(u:fn-> max! (vec vec vec) vec)
(declaim (inline max!))
(defun max! (out vec1 vec2)
  (declare (optimize speed))
  (com::cwset 4 out (vec1 vec2) (cl:max vec1 vec2))
  out)

(u:fn-> max (vec vec) vec)
(declaim (inline max))
(defun max (vec1 vec2)
  (declare (optimize speed))
  (max! (zero) vec1 vec2))

(u:fn-> radians! (vec vec) vec)
(declaim (inline radians!))
(defun radians! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:* vec const:+deg+))
  out)

(u:fn-> radians (vec) vec)
(declaim (inline radians))
(defun radians (vec)
  (declare (optimize speed))
  (radians! (zero) vec))

(u:fn-> degrees! (vec vec) vec)
(declaim (inline degrees!))
(defun degrees! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:* vec const:+rad+))
  out)

(u:fn-> degrees (vec) vec)
(declaim (inline degrees))
(defun degrees (vec)
  (declare (optimize speed))
  (degrees! (zero) vec))

(u:fn-> expt! (vec vec real) vec)
(declaim (inline expt!))
(defun expt! (out vec power)
  (com::cwset 4 out vec (cl:expt vec power))
  out)

(u:fn-> expt (vec real) vec)
(declaim (inline expt))
(defun expt (vec power)
  (expt! (zero) vec power))

(u:fn-> sqrt! (vec vec) vec)
(declaim (inline sqrt!))
(defun sqrt! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:sqrt (the (u:f32 0.0) vec)))
  out)

(u:fn-> sqrt (vec) vec)
(declaim (inline sqrt))
(defun sqrt (vec)
  (declare (optimize speed))
  (sqrt! (zero) vec))

(u:fn-> floor! (vec vec &optional u:f32) vec)
(declaim (inline floor!))
(defun floor! (out vec &optional (divisor 1.0))
  (declare (optimize speed))
  (com::cwset 4 out vec (ffloor vec divisor))
  out)

(u:fn-> floor (vec &optional u:f32) vec)
(declaim (inline floor))
(defun floor (vec &optional (divisor 1.0))
  (declare (optimize speed))
  (floor! (zero) vec divisor))

(u:fn-> ceiling! (vec vec &optional u:f32) vec)
(declaim (inline ceiling!))
(defun ceiling! (out vec &optional (divisor 1.0))
  (declare (optimize speed))
  (com::cwset 4 out vec (fceiling vec divisor))
  out)

(u:fn-> ceiling (vec &optional u:f32) vec)
(declaim (inline ceiling))
(defun ceiling (vec &optional (divisor 1.0))
  (declare (optimize speed))
  (ceiling! (zero) vec divisor))

(u:fn-> mod! (vec vec u:f32) vec)
(declaim (inline mod!))
(defun mod! (out vec divisor)
  (declare (optimize speed))
  (com::cwset 4 out vec (nth-value 1 (ffloor vec divisor)))
  out)

(u:fn-> mod (vec u:f32) vec)
(declaim (inline mod))
(defun mod (vec divisor)
  (declare (optimize speed))
  (mod! (zero) vec divisor))

(u:fn-> sin! (vec vec) vec)
(declaim (inline sin!))
(defun sin! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:sin vec))
  out)

(u:fn-> sin (vec) vec)
(declaim (inline sin))
(defun sin (vec)
  (declare (optimize speed))
  (sin! (zero) vec))

(u:fn-> cos! (vec vec) vec)
(declaim (inline cos!))
(defun cos! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:cos vec))
  out)

(u:fn-> cos (vec) vec)
(declaim (inline cos))
(defun cos (vec)
  (declare (optimize speed))
  (cos! (zero) vec))

(u:fn-> tan! (vec vec) vec)
(declaim (inline tan!))
(defun tan! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:tan vec))
  out)

(u:fn-> tan (vec) vec)
(declaim (inline tan))
(defun tan (vec)
  (declare (optimize speed))
  (tan! (zero) vec))

(u:fn-> asin! (vec vec) vec)
(declaim (inline asin!))
(defun asin! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:asin (the (u:f32 -1.0 1.0) vec)))
  out)

(u:fn-> asin (vec) vec)
(declaim (inline asin))
(defun asin (vec)
  (declare (optimize speed))
  (asin! (zero) vec))

(u:fn-> acos! (vec vec) vec)
(declaim (inline acos!))
(defun acos! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:acos (the (u:f32 -1.0 1.0) vec)))
  out)

(u:fn-> acos (vec) vec)
(declaim (inline acos))
(defun acos (vec)
  (declare (optimize speed))
  (acos! (zero) vec))

(u:fn-> atan! (vec vec) vec)
(declaim (inline atan!))
(defun atan! (out vec)
  (declare (optimize speed))
  (com::cwset 4 out vec (cl:atan vec))
  out)

(u:fn-> atan (vec) vec)
(declaim (inline atan))
(defun atan (vec)
  (declare (optimize speed))
  (atan! (zero) vec))