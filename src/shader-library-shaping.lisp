(in-package #:zed.shader-library)

(defun shape/almost-identity ((x :float)
                              (threshold :float)
                              (min :float))
  (if (> x threshold)
      x
      (let ((a (- (* 2.0 min) threshold))
            (b (- (* 2.0 threshold) (* 3.0 min)))
            (v (/ x threshold)))
        (+ (* (+ (* a v) b) v v) min))))

(defun shape/impulse ((x :float) (k :float))
  (let ((h (* x k)))
    (* h (exp (- 1 h)))))

(defun shape/cubic-pulse ((x :float)
                          (center :float)
                          (width :float))
  (let ((x (abs (- x center))))
    (if (> x width)
        0.0
        (progn
          (divf x width)
          (- 1.0 (* x x (- 3.0 (* 2.0 x))))))))

(defun shape/exponential-step ((x :float)
                               (exponent :float)
                               (sharpness :float))
  (exp (* (- sharpness) (expt x exponent))))

(defun shape/gain ((x :float)
                   (k :float))
  (let* ((v (if (< x 0.5) x (- 1.0 x)))
         (a (* 0.5 (expt (* 2 v) k))))
    (if (< x 0.5)
        a
        (- 1.0 a))))

(defun shape/parabola ((x :float)
                       (k :float))
  (expt (* 4.0 x (- 1.0 x)) k))


(defun shape/power-curve ((x :float)
                          (a :float)
                          (b :float))
  (let ((k (/ (expt (+ a b) (+ a b))
              (* (expt a a) (expt b b)))))
    (* k (expt x a) (expt (- 1.0 x) b))))

(defun shape/sinc-curve ((x :float)
                         (k :float))
  (let ((a (* +pi+ (1- (* k x)))))
    (/ (sin a) a)))

(defun shape/exponential-emphasis ((x :float)
                                   (a :float))
  (let ((a (max +epsilon+ (min (- 1 +epsilon+) a))))
    (if (< a 0.5)
        (expt x (* 2 a))
        (expt x (/ (- 1 (* 2 (- a 0.5))))))))

(defun shape/double-exponential-seat ((x :float)
                                      (a :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a))))
    (if (<= x 0.5)
        (/ (expt (* 2 x) (- 1 a)) 2)
        (- 1 (/ (expt (* 2 (- 1 x)) (- 1 a)) 2)))))

(defun shape/double-exponential-sigmoid ((x :float)
                                         (a :float))
  (let ((a (- 1 (min (- 1 +epsilon+) (max +epsilon+ a)))))
    (if (<= x 0.5)
        (/ (expt (* 2 x) (/ a)) 2)
        (- 1 (/ (expt (* 2 (- 1 x)) (/ a)) 2)))))

(defun shape/logistic-sigmoid ((x :float)
                               (a :float))
  (let* ((a (1- (/ (- 1 (max +epsilon+ (min (- 1 +epsilon+) a))))))
         (a2 (/ (1+ (exp (- (* (- x 0.5) a 2.0))))))
         (b (/ (1+ (exp a))))
         (c (/ (1+ (exp (- a))))))
    (/ (- a2 b) (- c b))))

(defun shape/double-circle-seat ((x :float)
                                 (a :float))
  (let* ((a (max 0 (min 1 a)))
         (b (* (- x a) (- x a)))
         (c (* (- 1 a) (- 1 a))))
    (if (<= x a)
        (sqrt (- (* a a) b))
        (- 1 (sqrt (- c b))))))

(defun shape/double-circle-sigmoid ((x :float)
                                    (a :float))
  (let* ((a (max 0 (min 1 a)))
         (b (* (- 1 a) (- 1 a)))
         (c (* (1- x) (1- x))))
    (if (<= x a)
        (- a (sqrt (- (* a a) (* x x))))
        (+ a (sqrt (- b c))))))

(defun shape/double-elliptical-seat ((x :float)
                                     (a :float)
                                     (b :float))
  (let* ((a (max +epsilon+ (min (- 1 +epsilon+) a)))
         (b (max 0 (min 1 b)))
         (c (* (- x a) (- x a)))
         (d (* (- 1 a) (- 1 a))))
    (if (<= x a)
        (* (/ b a) (sqrt (- (* a a) c)))
        (- 1 (* (/ (- 1 b) (- 1 a)) (sqrt (- d c)))))))

(defun shape/double-elliptical-sigmoid ((x :float)
                                        (a :float)
                                        (b :float))
  (let* ((a (max +epsilon+ (min (- 1 +epsilon+) a)))
         (b (max 0 (min 1 b)))
         (c (* (- 1 a) (- 1 a)))
         (d (* (1- x) (1- x))))
    (if (<= x a)
        (* b (- 1 (/ (sqrt (- (* a a) (* x x))) a)))
        (+ b (* (/ (- 1 b) (- 1 a)) (sqrt (- c d)))))))

(defun shape/blinn-wyvill-raised-inverted-cosine ((x :float))
  (let* ((x2 (* x x))
         (x4 (* x2 x2))
         (x6 (* x4 x2)))
    (+ (- (* (/ 4 9.0) x6) (* (/ 17 9.0) x4))
       (* (/ 22 9.0) x2))))

(defun shape/double-cubic-seat ((x :float)
                                (a :float)
                                (b :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (min 1 (max 0 b))))
    (if (<= x a)
        (- b (* b (expt (- 1 (/ x a)) 3.0)))
        (+ b (* (- 1 b) (expt (/ (- x a) (- 1 a)) 3.0))))))

(defun shape/double-cubic-seat-linear-blend ((x :float)
                                             (a :float)
                                             (b :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (- 1 (min 1 (max 0 b)))))
    (if (<= x a)
        (+ (* b x) (* (- 1 b) a (- 1 (expt (- 1 (/ x a)) 3.0))))
        (+ (* b x)
           (* (- 1 b) (+ a (* (- 1 a) (expt (/ (- x a) (- 1 a)) 3.0))))))))

(defun shape/double-odd-polynomial-seat ((x :float)
                                         (a :float)
                                         (b :float)
                                         (n :uint))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (min 1 (max 0 b)))
        (p (1+ (* 2 n))))
    (if (<= x a)
        (- b (* b (expt (- 1 (/ x a)) p)))
        (+ b (* (- 1 b) (expt (/ (- x a) (- 1 a)) p))))))

(defun shape/quadratic-point ((x :float)
                              (a :float)
                              (b :float))
  (let* ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
         (b (min 1 (max 0 b)))
         (a2 (- (/ (- 1 b) (- 1 a)) (/ b a)))
         (b2 (/ (- (* a2 a a) b) a))
         (y (- (* a2 x x) (* b2 x))))
    (min 1 (max 0 y))))

(defun shape/linear ((x :float))
  x)

(defun shape/sine-out ((x :float))
  (sin (* +half-pi+ x)))

(defun shape/sine-in ((x :float))
  (1+ (sin (* (1- x) +half-pi+))))

(defun shape/sine-in-out ((x :float))
  (* 0.5 (- 1 (cos (* x +pi+)))))

(defun shape/quadratic-out ((x :float))
  (- (* x (- x 2))))

(defun shape/quadratic-in ((x :float))
  (* x x))

(defun shape/quadratic-in-out ((x :float))
  (if (< x 0.5)
      (* x x 2)
      (1- (+ (* x x -2) (* x 4)))))

(defun shape/cubic-out ((x :float))
  (let ((f (1- x)))
    (1+ (* f f f))))

(defun shape/cubic-in ((x :float))
  (* x x x))

(defun shape/cubic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x 4)
      (let ((f (- (* x 2) 2)))
        (1+ (* 0.5 f f f)))))

(defun shape/quartic-out ((x :float))
  (let ((f (1- x)))
    (1+ (* f f f (- 1 x)))))

(defun shape/quartic-in ((x :float))
  (* x x x x))

(defun shape/quartic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x x 8)
      (let ((f (1- x)))
        (1+ (* -8 f f f f)))))

(defun shape/quintic-out ((x :float))
  (let ((f (- x 1)))
    (1+ (* f f f f f))))

(defun shape/quintic-in ((x :float))
  (* x x x x x))

(defun shape/quintic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x x x 16)
      (let ((f (- (* x 2) 2)))
        (1+ (* 0.5 f f f f f)))))

(defun shape/exponential-out ((x :float))
  (if (= x 1)
      x
      (- 1 (expt 2 (* x -10)))))

(defun shape/exponential-in ((x :float))
  (if (zerop x)
      x
      (expt 2 (* 10 (1- x)))))

(defun shape/exponential-in-out ((x :float))
  (cond
    ((or (zerop x) (= x 1))
     x)
    ((< x 0.5)
     (* 0.5 (expt 2 (- (* x 20) 10))))
    (t
     (1+ (* -0.5 (expt 2 (+ (* x -20) 10)))))))

(defun shape/circular-out ((x :float))
  (sqrt (* (- 2 x) x)))

(defun shape/circular-in ((x :float))
  (- 1 (sqrt (- 1 (* x x)))))

(defun shape/circular-in-out ((x :float))
  (if (< x 0.5)
      (* 0.5 (- 1 (sqrt (- 1 (* 4 x x)))))
      (* 0.5 (1+ (sqrt (- (* (- (* 2 x) 3) (1- (* 2 x)))))))))

(defun shape/back-out ((x :float))
  (let ((f (- 1 x)))
    (- 1 (- (* f f f) (* f (sin (* f +pi+)))))))

(defun shape/back-in ((x :float))
  (- (* x x x) (* x (sin (* x +pi+)))))

(defun shape/back-in-out ((x :float))
  (if (< x 0.5)
      (let ((f (* x 2)))
        (* 0.5 (- (* f f f) (* f (sin (* f +pi+))))))
      (let ((f (- 2 (* x 2))))
        (+ (* 0.5 (- 1 (- (* f f f) (* f (sin (* f +pi+)))))) 0.5))))

(defun shape/elastic-out ((x :float))
  (1+ (* (sin (* -13 +half-pi+ (1+ x))) (expt 2 (* -10 x)))))

(defun shape/elastic-in ((x :float))
  (* (sin (* 13 +half-pi+ x)) (expt 2 (* 10 (1- x)))))

(defun shape/elastic-in-out ((x :float))
  (if (< x 0.5)
      (let ((v (* x 2)))
        (* 0.5 (sin (* 13 +half-pi+ v)) (expt 2 (* 10 (1- v)))))
      (let ((v (1- (* x 2))))
        (* 0.5 (+ (* (sin (* -13 +half-pi+ (1+ v))) (expt 2 (* -10 v))) 2)))))

(defun shape/bounce-out ((x :float))
  (cond
    ((< x 0.36363637)
     (/ (* x x 121) 16.0))
    ((< x 0.72727275)
     (+ (- (* 9.075 x x) (* 9.9 x)) 3.4))
    ((< x 0.9)
     (+ (- (* 12.066482 x x) (* 19.635458 x)) 8.898061))
    (t
     (+ (- (* 10.8 x x) (* 20.52 x)) 10.72))))

(defun shape/bounce-in ((x :float))
  (- 1 (shape/bounce-out (- 1 x))))

(defun shape/bounce-in-out ((x :float))
  (let ((f (* x 2)))
    (if (< x 0.5)
        (* 0.5 (shape/bounce-in f))
        (+ 0.5 (* 0.5 (shape/bounce-out (1- f)))))))

(defun shape/hermite-curve ((x :float))
  (* x x (- 3 (* 2 x))))

(defun shape/hermite-curve ((x :vec2))
  (* x x (- 3 (* 2 x))))

(defun shape/hermite-curve ((x :vec3))
  (* x x (- 3 (* 2 x))))

(defun shape/hermite-curve ((x :vec4))
  (* x x (- 3 (* 2 x))))

(defun shape/hermite-curve ((x :float)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/hermite-curve x)))

(defun shape/hermite-curve ((x :vec2)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/hermite-curve x)))

(defun shape/hermite-curve ((x :vec3)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/hermite-curve x)))

(defun shape/hermite-curve ((x :vec4)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/hermite-curve x)))

(defun shape/quintic-curve ((x :float))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun shape/quintic-curve ((x :vec2))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun shape/quintic-curve ((x :vec3))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun shape/quintic-curve ((x :vec4))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun shape/quintic-curve ((x :float)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve x)))

(defun shape/quintic-curve ((x :vec2)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve x)))

(defun shape/quintic-curve ((x :vec3)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve x)))

(defun shape/quintic-curve ((x :vec4)
                            (min :float)
                            (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve x)))

(defun shape/quintic-curve-interpolate-derivative ((x :vec2))
  (let ((x (.xyxy x)))
    (* x x (+ (* x (+ (* x (+ (* x (vec4 6 6 0 0)) (vec4 -15 -15 30 30)))
                      (vec4 10 10 -60 -60)))
              (vec4 0 0 30 30)))))

(defun shape/quintic-curve-derivative ((x :vec3))
  (* x x (+ (* x (- (* x 30) 60)) 30)))

(defun shape/quintic-curve-fast ((x :float))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun shape/quintic-curve-fast ((x :vec2))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun shape/quintic-curve-fast ((x :vec3))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun shape/quintic-curve-fast ((x :vec4))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun shape/quintic-curve-fast ((x :float)
                                 (min :float)
                                 (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve-fast x)))

(defun shape/quintic-curve-fast ((x :vec2)
                                 (min :float)
                                 (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve-fast x)))

(defun shape/quintic-curve-fast ((x :vec3)
                                 (min :float)
                                 (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve-fast x)))

(defun shape/quintic-curve-fast ((x :vec4)
                                 (min :float)
                                 (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (shape/quintic-curve-fast x)))

(defun shape/quintic-hermite ((x :float)
                              (ival0 :float)
                              (ival1 :float)
                              (egrad0 :float)
                              (egrad1 :float))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (+ ival0
       (dot (vec3 (- ival1 ival0) egrad0 egrad1)
            (+ h123 (vec3 0 x 0))))))

(defun shape/quintic-hermite ((x :float)
                              (ival0 :vec4)
                              (ival1 :vec4)
                              (egrad0 :vec4)
                              (egrad1 :vec4))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (+ ival0
       (* (- ival1 ival0) (.x h123))
       (* egrad0 (vec4 (+ (.y h123) x)))
       (* egrad1 (.z h123)))))

(defun shape/quintic-hermite ((x :float)
                              (igrad0 :vec2)
                              (igrad1 :vec2)
                              (egrad0 :vec2)
                              (egrad1 :vec2))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (+ (* (vec4 egrad1 igrad0)
          (vec4 (.zz h123) 1 1))
       (* (vec4 egrad0 (.xx h123))
          (vec4 (vec2 (+ (.y h123) x))  (- igrad1 igrad0))))))

(defun shape/quintic-hermite ((x :float)
                              (ival0 :vec4)
                              (ival1 :vec4)
                              (igrad-x0 :vec4)
                              (igrad-x1 :vec4)
                              (igrad-y0 :vec4)
                              (igrad-y1 :vec4)
                              (egrad0 :vec4)
                              (egrad1 :vec4))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (values (+ ival0
               (* (- ival1 ival0) (.x h123))
               (* egrad0 (vec4 (+ (.y h123) x)))
               (* egrad1 (.z h123)))
            (+ igrad-x0 (* (- igrad-x1 igrad-x0) (.x h123)))
            (+ igrad-y0 (* (- igrad-y1 igrad-y0) (.x h123))))))

(defun shape/quintic-hermite ((x :float)
                              (igrad-x0 :vec4)
                              (igrad-x1 :vec4)
                              (igrad-y0 :vec4)
                              (igrad-y1 :vec4)
                              (egrad0 :vec4)
                              (egrad1 :vec4))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (values (+ (* egrad0 (vec4 (+ (.y h123) x)))
               (* egrad1 (.z h123)))
            (+ igrad-x0 (* (- igrad-x1 igrad-x0) (.x h123)))
            (+ igrad-y0 (* (- igrad-y1 igrad-y0) (.x h123))))))

(defun shape/quintic-hermite-derivative ((x :float)
                                         (ival0 :float)
                                         (ival1 :float)
                                         (egrad0 :float)
                                         (egrad1 :float))
  (let* ((c0 (vec3 30 -15 -15))
         (c1 (vec3 -60 32 28))
         (c2 (vec3 30 -18 -12))
         (h123 (* (+ (* (+ c1 (* c0 x)) x) c2) x x)))
    (dot (vec3 (- ival1 ival0) egrad0 egrad1)
         (+ h123 (vec3 0 1 0)))))

(defun shape/falloff-squared-c1 ((x :float))
  (let ((x (- 1 x)))
    (* x x)))

(defun shape/falloff-squared-c2 ((x :float))
  (let ((x (- 1 x)))
    (* x x x)))

(defun shape/falloff-squared-c2 ((x :vec4))
  (let ((x (- 1 x)))
    (* x x x)))
