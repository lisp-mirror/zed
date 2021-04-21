(in-package #:zed.shader-library)

(defun matcap/vertex ((mesh-attrs mesh-attrs)
                      &uniforms
                      (model :mat4)
                      (camera camera-data :ssbo :std-430)
                      (normal-matrix :mat3))
  (with-slots (mesh/pos mesh/normal mesh/uv1) mesh-attrs
    (let ((normal (* normal-matrix mesh/normal)))
      (values (* (camera/proj camera) (camera/view camera) model (vec4 mesh/pos 1))
              normal))))

(defun matcap/fragment ((normal :vec3)
                        &uniforms
                        (camera camera-data :ssbo :std-430)
                        (sampler :sampler-2d))
  (let ((uv (+ 0.5 (* 0.5 (vec2 (* (camera/view camera) (vec4 (normalize normal) 0)))))))
    (texture sampler (vec2 (.x uv) (- 1 (.y uv))))))

(define-shader matcap ()
  (:vertex (matcap/vertex mesh-attrs))
  (:fragment (matcap/fragment :vec3)))
