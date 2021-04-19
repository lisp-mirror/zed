(in-package #:zed)

(u:fn-> generate-uniform-function/sampler (material) function)
(defun generate-uniform-function/sampler (material)
  (lambda (program k v)
    (declare (optimize speed))
    (let ((unit (material-texture-unit-state material)))
      (incf (material-texture-unit-state material))
      (bind-texture v unit)
      (shadow:uniform-int program k unit))))

(u:fn-> generate-uniform-function/sampler-array (material u:ub32) function)
(defun generate-uniform-function/sampler-array (material dimensions)
  (lambda (program k v)
    (declare (optimize speed))
    (loop :with unit-state = (material-texture-unit-state material)
          :with unit-count = (+ unit-state dimensions)
          :for texture :in v
          :for unit :from unit-state :to unit-count
          :do (bind-texture texture unit)
          :collect unit :into units
          :finally (incf (material-texture-unit-state material) dimensions)
                   (shadow:uniform-int-array program k units))))

(u:fn-> generate-uniform-function (material uniform) function)
(defun generate-uniform-function (material uniform)
  (declare (optimize speed))
  (let ((type (uniform-type uniform))
        (resolved-type (uniform-resolved-type uniform)))
    (etypecase type
      (symbol
       (ecase resolved-type
         (:sampler (generate-uniform-function/sampler material))
         (:bool #'shadow:uniform-bool)
         (:int #'shadow:uniform-int)
         (:float #'shadow:uniform-float)
         (:vec2 #'shadow:uniform-vec2)
         (:vec3 #'shadow:uniform-vec3)
         (:vec4 #'shadow:uniform-vec4)
         (:mat2 #'shadow:uniform-mat2)
         (:mat3 #'shadow:uniform-mat3)
         (:mat4 #'shadow:uniform-mat4)))
      (cons
       (destructuring-bind (type . dimensions) type
         (declare (ignore type))
         (ecase resolved-type
           (:sampler (generate-uniform-function/sampler-array material dimensions))
           (:bool #'shadow:uniform-bool-array)
           (:int #'shadow:uniform-int-array)
           (:float #'shadow:uniform-float-array)
           (:vec2 #'shadow:uniform-vec2-array)
           (:vec3 #'shadow:uniform-vec3-array)
           (:vec4 #'shadow:uniform-vec4-array)
           (:mat2 #'shadow:uniform-mat2-array)
           (:mat3 #'shadow:uniform-mat3-array)
           (:mat4 #'shadow:uniform-mat4-array)))))))

(u:fn-> register-uniform (material uniform) null)
(defun register-uniform (material uniform)
  (declare (optimize speed))
  (let* ((material-data (material-data material))
         (shader (material-data-shader material-data))
         (program (shadow:find-program shader))
         (key (uniform-key uniform)))
    (unless (u:href (shadow:uniforms program) key)
      (error "Material ~s has the uniform ~s but shader ~s does not use it."
             (material-data-name material-data)
             key
             shader))
    (let* ((type (u:href (shadow:uniforms program) key :type))
           (resolved-type (if (search "SAMPLER" (symbol-name type))
                              :sampler
                              type)))
      (setf (uniform-program uniform) program
            (uniform-type uniform) type
            (uniform-resolved-type uniform) resolved-type
            (uniform-func uniform) (generate-uniform-function material uniform)))
    nil))

(defun as-uniform (func)
  (lambda (game-object)
    (declare (ignore game-object))
    (funcall func)))

(u:fn-> resolve-uniform-value (core game-object uniform) t)
(defun resolve-uniform-value (core game-object uniform)
  (let ((value (uniform-value uniform)))
    (etypecase value
      (boolean value)
      ((or symbol function) (funcall value core game-object))
      (t value))))

(u:fn-> resolve-uniform-function (core game-object uniform) t)
(defun resolve-uniform-function (core game-object uniform)
  (funcall (uniform-func uniform)
           (uniform-program uniform)
           (uniform-key uniform)
           (resolve-uniform-value core game-object uniform)))

(u:fn-> load-uniform-texture (core material uniform) null)
(defun load-uniform-texture (core material uniform)
  (declare (optimize speed))
  (let ((value (uniform-value uniform)))
    (when (eq (uniform-resolved-type uniform) :sampler)
      (let ((material-name (material-data-name (material-data material)))
            (texture (load-texture core value)))
        (setf (uniform-value uniform) texture)
        (pushnew material-name (texture-materials texture))
        (pushnew value (material-textures material))))
    nil))

(u:fn-> make-uniforms (core material) null)
(defun make-uniforms (core material)
  (declare (optimize speed))
  (let ((material-data (material-data material)))
    (clrhash (material-uniforms material))
    (dolist (texture-name (material-textures material))
      (let ((texture (find-resource core :texture texture-name)))
        (u:deletef (texture-materials texture) (material-data-name material-data))))
    (setf (material-textures material) nil)
    (u:do-hash (k v (copy-material-data-uniforms material-data))
      (let ((uniform (make-uniform :key k :value v)))
        (register-uniform material uniform)
        (load-uniform-texture core material uniform)
        (setf (u:href (material-uniforms material) k) uniform)))
    nil))
