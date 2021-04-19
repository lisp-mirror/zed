(in-package #:cl-user)

(defpackage #:%zed.math.common
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:=)
  (:export
   #:=))

(defpackage #:zed.math.constants
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:+rad+
   #:+deg+
   #:+2pi_
   #:+2pi/3+
   #:+2pi/12+
   #:+3pi/2+
   #:+3pi/4+
   #:+3pi/12+
   #:+4pi/3+
   #:+4pi/12+
   #:+5pi/3+
   #:+5pi/4+
   #:+5pi/6+
   #:+5pi/12+
   #:+6pi/12+
   #:+7pi/4+
   #:+7pi/6+
   #:+7pi/12+
   #:+8pi/12+
   #:+9pi/12+
   #:+10pi/12+
   #:+11pi/6+
   #:+11pi/12+
   #:+12pi/12+
   #:+13pi/12+
   #:+14pi/12+
   #:+15pi/12+
   #:+16pi/12+
   #:+17pi/12+
   #:+18pi/12+
   #:+19pi/12+
   #:+20pi/12+
   #:+21pi/12+
   #:+22pi/12+
   #:+23pi/12+
   #:+24pi/12+
   #:+pi+
   #:+pi/2+
   #:+pi/3+
   #:+pi/4+
   #:+pi/6+
   #:+pi/12+))

(defpackage #:zed.math.vector2
  (:local-nicknames)
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:const #:zed.math.constants)
   (#:u #:golden-utils))
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
   #:with-components
   #:+zero+
   #:+ones+
   #:+up+
   #:+down+
   #:+left+
   #:+right+
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
   #:atan
   #:velocity!
   #:velocity))

(defpackage #:zed.math.vector3
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:const #:zed.math.constants)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2))
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
   #:with-components
   #:+zero+
   #:+ones+
   #:+up+
   #:+down+
   #:+left+
   #:+right+
   #:+forward+
   #:+back+
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
   #:cross!
   #:cross
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
   #:atan
   #:velocity!
   #:velocity))

(defpackage #:zed.math.vector4
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:const #:zed.math.constants)
   (#:u #:golden-utils)
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

(defpackage #:zed.math.matrix2
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:pretty-print
   #:+zero+
   #:+id+
   #:zero
   #:zero!
   #:zero-p
   #:random
   #:id
   #:id!
   #:id-p
   #:=
   #:copy!
   #:copy
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotation-from-angle!
   #:rotation-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v2!
   #:*v2
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(defpackage #:zed.math.matrix3
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:m2 #:zed.math.matrix2)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:pretty-print
   #:+zero+
   #:+id+
   #:zero
   #:zero!
   #:zero-p
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
   #:copy!
   #:copy
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-to-mat2!
   #:rotation-to-mat2
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotation-x-from-angle!
   #:rotation-x-from-angle
   #:rotation-y-from-angle!
   #:rotation-y-from-angle
   #:rotation-z-from-angle!
   #:rotation-z-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v3!
   #:*v3
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal))

(defpackage #:zed.math.matrix4
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:m2 #:zed.math.matrix2)
   (#:m3 #:zed.math.matrix3)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:pretty-print
   #:+zero+
   #:+id+
   #:zero
   #:zero!
   #:zero-p
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
   #:copy!
   #:copy
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-to-mat3!
   #:rotation-to-mat3
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec3!
   #:rotation-axis-to-vec3
   #:rotation-axis-from-vec3!
   #:rotation-axis-from-vec3
   #:rotation-x-from-angle!
   #:rotation-x-from-angle
   #:rotation-y-from-angle!
   #:rotation-y-from-angle!
   #:rotation-z-from-angle
   #:rotation-z-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v4!
   #:*v4
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:orthonormalize!
   #:orthonormalize
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal
   #:determinant
   #:invert-orthogonal!
   #:invert-orthogonal
   #:invert!
   #:invert
   #:look-at!
   #:look-at
   #:ortho!
   #:ortho
   #:perspective!
   #:perspective))

(defpackage #:zed.math.quaternion
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:const #:zed.math.constants)
   (#:m3 #:zed.math.matrix3)
   (#:m4 #:zed.math.matrix4)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:conjugate
   #:length
   #:random)
  (:export
   #:quat
   #:with-components
   #:w
   #:x
   #:y
   #:z
   #:+id+
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
   #:copy!
   #:copy
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:scale!
   #:scale
   #:conjugate!
   #:conjugate
   #:cross!
   #:cross
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:negate!
   #:negate
   #:dot
   #:inverse!
   #:inverse
   #:rotate-euler!
   #:rotate-euler
   #:rotate!
   #:rotate
   #:to-euler!
   #:to-euler
   #:to-mat3!
   #:to-mat3
   #:to-mat4!
   #:to-mat4
   #:from-mat3!
   #:from-mat3
   #:from-mat4!
   #:from-mat4
   #:slerp!
   #:slerp
   #:from-axis-angle!
   #:from-axis-angle
   #:orient!
   #:orient
   #:from-velocity!
   #:from-velocity))

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

(defpackage #:zed.math.point3d
  (:local-nicknames
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4)
   (#:m4 #:zed.math.matrix4))
  (:use #:cl)
  (:import-from
   #:zed.math.vector3
   #:x
   #:y
   #:z)
  (:export
   #:distance
   #:distance-squared
   #:find-min-max
   #:point
   #:translate
   #:unproject
   #:x
   #:y
   #:z))

(defpackage #:zed.math.line3d
  (:local-nicknames
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:shadow
   #:length)
  (:export
   #:direction
   #:end
   #:length
   #:length-squared
   #:line
   #:midpoint
   #:start))

(defpackage #:zed.math.easing
  (:local-nicknames
   (#:const #:zed.math.constants)
   (#:u #:golden-utils))
  (:use #:cl)
  ;; shaping
  (:export
   #:linear
   #:sine-out
   #:sine-in
   #:sine-in-out
   #:quadratic-out
   #:quadratic-in
   #:quadratic-in-out
   #:cubic-out
   #:cubic-in
   #:cubic-in-out
   #:quartic-out
   #:quartic-in
   #:quartic-in-out
   #:quintic-out
   #:quintic-in
   #:quintic-in-out
   #:exponential-out
   #:exponential-in
   #:exponential-in-out
   #:circular-out
   #:circular-in
   #:circular-in-out
   #:back-out
   #:back-in
   #:back-in-out
   #:elastic-out
   #:elastic-in
   #:elastic-in-out
   #:bounce-out
   #:bounce-in
   #:bounce-in-out
   #:hermite-curve
   #:quintic-curve))

(defpackage #:zed.math.geometry.aabb
  (:local-nicknames
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:shadow
   #:max
   #:min)
  (:export
   #:aabb
   #:from-min/max
   #:from-min/max!
   #:interval
   #:max
   #:min
   #:origin
   #:size
   #:vertices))

(defpackage #:zed.math.geometry.obb
  (:local-nicknames
   (#:aabb #:zed.math.geometry.aabb)
   (#:m3 #:zed.math.matrix3)
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:export
   #:bounding-aabb
   #:bounding-aabb!
   #:obb
   #:interval
   #:origin
   #:rotation
   #:size
   #:vertices))

(defpackage #:zed.math.geometry.sphere
  (:local-nicknames
   (#:aabb #:zed.math.geometry.aabb)
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:export
   #:bounding-aabb
   #:bounding-aabb!
   #:origin
   #:radius
   #:sphere))

(defpackage #:zed.math.geometry.ray
  (:local-nicknames
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:export
   #:direction
   #:from-points
   #:origin
   #:ray))

(defpackage #:zed.math.geometry.frustum
  (:local-nicknames
   (#:u #:golden-utils)
   (#:m4 #:zed.math.matrix4)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  (:export
   #:bottom
   #:far
   #:frustum
   #:left
   #:make-frustum
   #:near
   #:right
   #:top
   #:update))

(defpackage #:zed.math.geometry.closest-point
  (:local-nicknames
   (#:m3 #:zed.math.matrix3)
   (#:obb #:zed.math.geometry.obb)
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:export
   #:obb))

(defpackage #:zed.math.geometry.test
  (:local-nicknames
   (#:aabb #:zed.math.geometry.aabb)
   (#:frustum #:zed.math.geometry.frustum)
   (#:geo.cp #:zed.math.geometry.closest-point)
   (#:m3 #:zed.math.matrix3)
   (#:obb #:zed.math.geometry.obb)
   (#:p3 #:zed.math.point3d)
   (#:sphere #:zed.math.geometry.sphere)
   (#:u #:golden-utils)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  (:export
   #:aabb/frustum
   #:frustum/aabb
   #:obb/obb
   #:obb/sphere
   #:sphere/obb
   #:sphere/sphere))

(defpackage #:zed.math.geometry.raycast
  (:local-nicknames
   (#:com #:%zed.math.common)
   (#:aabb #:zed.math.geometry.aabb)
   (#:m3 #:zed.math.matrix3)
   (#:obb #:zed.math.geometry.obb)
   (#:ray #:zed.math.geometry.ray)
   (#:sphere #:zed.math.geometry.sphere)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:export
   #:ray/aabb
   #:ray/obb
   #:ray/sphere))
