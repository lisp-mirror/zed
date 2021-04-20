(in-package #:zed.shader-library)

(defun default/vertex ((pos :vec3)
                       &uniforms
                       (model :mat4)
                       (camera camera-data :ssbo :std-430))
  (* (camera/proj camera) (camera/view camera) model (vec4 pos 1)))

(defun default/fragment (&uniforms
                         (color :vec3)
                         (opacity :float))
  (vec4 color opacity))

(define-shader default ()
  (:vertex (default/vertex :vec3))
  (:fragment (default/fragment)))
