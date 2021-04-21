(in-package #:zed.shader-library)

(generate-swizzle-operators)

(defstruct mesh-attrs
  (mesh/pos :vec3)
  (mesh/normal :vec3)
  (mesh/tangent :vec4)
  (mesh/color :vec4)
  (mesh/uv1 :vec2)
  (mesh/uv2 :vec2)
  (mesh/joints :vec4)
  (mesh/weights :vec4))

(defstruct camera-data
  (view :mat4 :accessor camera/view)
  (proj :mat4 :accessor camera/proj))

(defconstant +epsilon+ 1e-7)

(defconstant +pi+ (float pi 1f0))

(defconstant +half-pi+ (/ +pi+ 2))

(defmacro mvlet* ((&rest bindings) &body body)
  (destructuring-bind (&optional car . cdr) bindings
    (etypecase car
      (null
       `(progn ,@body))
      (list
       (case (length car)
         (0 (error "Missing variable in binding list."))
         ((1 2) `(let (,car) (mvlet* ,cdr ,@body)))
         (t `(multiple-value-bind ,(butlast car) ,(car (last car))
               (mvlet* ,cdr ,@body)))))
      (symbol
       `(mvlet* ,cdr ,@body)))))

(defun saturate ((x :float))
  (clamp x 0.0 1.0))

(defun saturate ((x :vec2))
  (clamp x 0.0 1.0))

(defun saturate ((x :vec3))
  (clamp x 0.0 1.0))

(defun saturate ((x :vec4))
  (clamp x 0.0 1.0))

(defun log10 ((x :float))
  (* (log2 x) 0.30103))

(defun log10 ((x :vec2))
  (* (log2 x) 0.30103))

(defun log10 ((x :vec3))
  (* (log2 x) 0.30103))

(defun log10 ((x :vec4))
  (* (log2 x) 0.30103))

(defun map-domain ((x :float)
                   (source-min :float)
                   (source-max :float)
                   (dest-min :float)
                   (dest-max :float))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))

(defun map-domain ((x :vec2)
                   (source-min :vec2)
                   (source-max :vec2)
                   (dest-min :vec2)
                   (dest-max :vec2))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))

(defun map-domain ((x :vec3)
                   (source-min :vec3)
                   (source-max :vec3)
                   (dest-min :vec3)
                   (dest-max :vec3))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))

(defun map-domain ((x :vec4)
                   (source-min :vec4)
                   (source-max :vec4)
                   (dest-min :vec4)
                   (dest-max :vec4))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))
