(in-package #:zed.shader-library)

(defun collider/vertex ((mesh-attrs mesh-attrs)
                        &uniforms
                        (model :mat4)
                        (camera camera-data :ssbo :std-430))
  (with-slots (mesh/pos) mesh-attrs
    (* (camera/proj camera) (camera/view camera) model (vec4 mesh/pos 1))))

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
