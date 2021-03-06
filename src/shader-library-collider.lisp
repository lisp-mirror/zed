(in-package #:zed.shader-library)

(defun collider/vertex ((mesh-attrs mesh-attrs)
                        &uniforms
                        (model :mat4)
                        (view :mat4)
                        (proj :mat4))
  (with-slots (mesh/pos) mesh-attrs
    (* proj view model (vec4 mesh/pos 1))))

(defun collider/fragment (&uniforms
                          (hit-color :vec4)
                          (miss-color :vec4)
                          (contact :bool))
  (if contact
      hit-color
      miss-color))

(define-shader collider ()
  (:vertex (collider/vertex mesh-attrs))
  (:fragment (collider/fragment)))
