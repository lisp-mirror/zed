(in-package #:zed.shader)

(sw::generate-swizzle-operators)

(defstruct mesh-attrs
  (mesh/pos :vec3)
  (mesh/normal :vec3)
  (mesh/tangent :vec4)
  (mesh/color :vec4)
  (mesh/uv1 :vec2)
  (mesh/uv2 :vec2)
  (mesh/joints :vec4)
  (mesh/weights :vec4))

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
