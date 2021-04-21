(in-package #:zed-examples.shader)

(defun texture/vertex ((pos :vec3)
                       (uv :vec2)
                       &uniforms
                       (model :mat4)
                       (view :mat4)
                       (proj :mat4))
  (values (vec4 (* (.xy pos) 2) 0 1)
          uv))

(defun texture/fragment ((uv :vec2)
                         &uniforms
                         (sampler :sampler-2d))
  (texture sampler uv))

(define-shader texture ()
  (:vertex (texture/vertex :vec3 :vec2))
  (:fragment (texture/fragment :vec2)))
