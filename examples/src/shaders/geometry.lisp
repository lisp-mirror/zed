(in-package #:zed-examples.shader)

(defun geometry/vertex ((pos :vec2)
                        (uv :vec2)
                        &uniforms
                        (model :mat4)
                        (view :mat4)
                        (proj :mat4))
  (values (* proj view model (vec4 pos 0 1))
          uv))

(defun geometry/fragment ((uv :vec2))
  (vec4 1 0 0 1))

(define-shader geometry ()
  (:vertex (geometry/vertex :vec2 :vec2))
  (:fragment (geometry/fragment :vec2)))
