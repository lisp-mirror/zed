(in-package #:defpackage+-user-1)

(defpackage+ #:zed.shader
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:sw #:%zed.shader.swizzle))
  (:inherit-except #:cl #:defun #:defstruct #:defmacro)
  (:inherit #:shadow.glsl #:vari)
  ;; Utilities
  (:export
   #:mvlet*
   #:saturate)
  ;; Structures and accessors
  (:export
   #:mesh-attrs
   #:mesh/pos
   #:mesh/normal
   #:mesh/tangent
   #:mesh/color
   #:mesh/uv1
   #:mesh/uv2
   #:mesh/joints
   #:mesh/weights)
  ;; Color
  (:export
   #:color-filter
   #:hcy->rgb
   #:hsl->rgb
   #:hsv->rgb
   #:hue->rgb
   #:rgb->grayscale
   #:rgb->hcv
   #:rgb->hcy
   #:rgb->hsl
   #:rgb->hsv
   #:rgb->srgb
   #:rgb->srgb-approx
   #:rgb->xyy
   #:rgb->xyz
   #:set-brightness
   #:set-contrast
   #:set-exposure
   #:set-gamma
   #:set-saturation
   #:srgb->rgb
   #:srgb->rgb-approx
   #:tone-map/aces
   #:tone-map/haarm-peter-duiker
   #:tone-map/hejl-burgess-dawson
   #:tone-map/linear
   #:tone-map/reinhard
   #:tone-map/uncharted2
   #:xyy->rgb
   #:xyy->xyz
   #:xyz->rgb
   #:xyz->xyy)
  ;; Sprite
  (:export
   #:sprite))
