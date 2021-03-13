(in-package #:cl-user)

(defpackage #:%zed.material.uniform
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:asset #:%zed.asset-pool)
   (#:ctx #:%zed.context)
   (#:fb #:%zed.framebuffer)
   (#:gob #:%zed.game-object)
   (#:matdef #:%zed.material.definition)
   (#:matdat #:%zed.material.data)
   (#:tex #:%zed.texture))
  (:use #:cl)
  (:shadow
   #:type))

(in-package #:%zed.material.uniform)

(defstruct (uniform
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (program nil :type (or shadow::program null))
  (key nil :type symbol)
  (type nil :type (or symbol cons))
  (resolved-type nil :type symbol)
  (value nil :type t)
  (func nil :type (or function null)))

(u:define-printer (uniform stream :type nil)
  (format stream "UNIFORM: ~s ~s"
          (resolved-type uniform)
          (value uniform)))

(u:fn-> %generate-func (matdef::material keyword) function)
(defun %generate-func (material type)
  (let ((func (fdefinition (u:format-symbol :shadow "UNIFORM-~a" type))))
    (lambda (k v)
      (declare (optimize speed))
      (funcall func (matdat::shader (matdef::data material)) k v))))

(u:fn-> generate-func/sampler (matdef::material) function)
(defun generate-func/sampler (material)
  (lambda (program k v)
    (declare (optimize speed))
    (let ((unit (matdef::texture-unit-state material)))
      (incf (matdef::texture-unit-state material))
      (tex::bind v unit)
      (shadow:uniform-int program k unit))))

(u:fn-> generate-func/sampler-array (matdef::material u:ub32) function)
(defun generate-func/sampler-array (material dimensions)
  (lambda (program k v)
    (declare (optimize speed))
    (loop :with unit-state = (matdef::texture-unit-state material)
          :with unit-count = (+ unit-state dimensions)
          :for texture :in v
          :for unit :from unit-state :to unit-count
          :do (tex::bind texture unit)
          :collect unit :into units
          :finally (incf (matdef::texture-unit-state material) dimensions)
                   (shadow:uniform-int-array program k units))))

(u:fn-> generate-func (matdef::material uniform) function)
(defun generate-func (material uniform)
  (declare (optimize speed))
  (let ((type (type uniform))
        (resolved-type (resolved-type uniform)))
    (etypecase type
      (symbol
       (ecase resolved-type
         (:sampler (generate-func/sampler material))
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
           (:sampler (generate-func/sampler-array material dimensions))
           (:bool #'shadow:uniform-bool-array)
           (:int #'shadow:uniform-int-array)
           (:float #'shadow:uniform-float-array)
           (:vec2 #'shadow:uniform-vec2-array)
           (:vec3 #'shadow:uniform-vec3-array)
           (:vec4 #'shadow:uniform-vec4-array)
           (:mat2 #'shadow:uniform-mat2-array)
           (:mat3 #'shadow:uniform-mat3-array)
           (:mat4 #'shadow:uniform-mat4-array)))))))

(u:fn-> register (matdef::material uniform) null)
(defun register (material uniform)
  (declare (optimize speed))
  (let* ((material-data (matdef::data material))
         (shader (matdat::shader material-data))
         (program (shadow:find-program shader))
         (key (key uniform)))
    (unless (u:href (shadow:uniforms program) key)
      (error "Material ~s has the uniform ~s but shader ~s does not use it."
             (matdat::name material-data)
             key
             shader))
    (let* ((type (u:href (shadow:uniforms program) key :type))
           (resolved-type (if (search "SAMPLER" (symbol-name type))
                              :sampler
                              type)))
      (setf (program uniform) program
            (type uniform) type
            (resolved-type uniform) resolved-type
            (func uniform) (generate-func material uniform)))
    nil))

(defun as-uniform (func)
  (lambda (game-object)
    (declare (ignore game-object))
    (funcall func)))

(u:fn-> resolve-value (gob::game-object uniform) t)
(defun resolve-value (game-object uniform)
  (let ((value (value uniform)))
    (etypecase value
      ((or real simple-array boolean) value)
      ((or symbol function) (funcall value game-object)))))

(u:fn-> resolve-func (gob::game-object uniform) t)
(defun resolve-func (game-object uniform)
  (funcall (func uniform)
           (program uniform)
           (key uniform)
           (resolve-value game-object uniform)))

(u:fn-> load-texture (ctx::context matdef::material uniform) null)
(defun load-texture (context material uniform)
  (declare (optimize speed))
  (let ((value (value uniform)))
    (when (eq (resolved-type uniform) :sampler)
      (let ((material-name (matdat::name (matdef::data material)))
            (texture (tex::load context value)))
        (setf (value uniform) texture)
        (pushnew material-name (tex::materials texture))
        (pushnew value (matdef::textures material))))
    nil))

(u:fn-> make-material-uniforms (ctx::context matdef::material) null)
(defun make-material-uniforms (context material)
  (declare (optimize speed))
  (let ((material-data (matdef::data material)))
    (clrhash (matdef::uniforms material))
    (dolist (texture-name (matdef::textures material))
      (let ((texture (asset::find context :texture texture-name)))
        (u:deletef (tex::materials texture) (matdat::name material-data))))
    (setf (matdef::textures material) nil)
    (u:do-hash (k v (matdat::copy-uniforms material-data))
      (let ((uniform (make-uniform :key k :value v)))
        (register material uniform)
        (load-texture context material uniform)
        (setf (u:href (matdef::uniforms material) k) uniform)))
    nil))
