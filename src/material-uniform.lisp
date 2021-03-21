(in-package #:cl-user)

(defpackage #:%zed.material.uniform
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ap #:%zed.asset-pool)
   (#:ctx #:%zed.context)
   (#:fb #:%zed.framebuffer)
   (#:gob #:%zed.game-object)
   (#:mat.data #:%zed.material.data)
   (#:mat.def #:%zed.material.definition)
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

(u:fn-> %generate-func (mat.def::material keyword) function)
(defun %generate-func (material type)
  (let ((func (fdefinition (u:format-symbol :shadow "UNIFORM-~a" type))))
    (lambda (k v)
      (declare (optimize speed))
      (funcall func (mat.data::shader (mat.def::data material)) k v))))

(u:fn-> generate-func/sampler (mat.def::material) function)
(defun generate-func/sampler (material)
  (lambda (program k v)
    (declare (optimize speed))
    (let ((unit (mat.def::texture-unit-state material)))
      (incf (mat.def::texture-unit-state material))
      (tex::bind v unit)
      (shadow:uniform-int program k unit))))

(u:fn-> generate-func/sampler-array (mat.def::material u:ub32) function)
(defun generate-func/sampler-array (material dimensions)
  (lambda (program k v)
    (declare (optimize speed))
    (loop :with unit-state = (mat.def::texture-unit-state material)
          :with unit-count = (+ unit-state dimensions)
          :for texture :in v
          :for unit :from unit-state :to unit-count
          :do (tex::bind texture unit)
          :collect unit :into units
          :finally (incf (mat.def::texture-unit-state material) dimensions)
                   (shadow:uniform-int-array program k units))))

(u:fn-> generate-func (mat.def::material uniform) function)
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

(u:fn-> register (mat.def::material uniform) null)
(defun register (material uniform)
  (declare (optimize speed))
  (let* ((material-data (mat.def::data material))
         (shader (mat.data::shader material-data))
         (program (shadow:find-program shader))
         (key (key uniform)))
    (unless (u:href (shadow:uniforms program) key)
      (error "Material ~s has the uniform ~s but shader ~s does not use it."
             (mat.data::name material-data)
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

(u:fn-> resolve-value (ctx::context gob::game-object uniform) t)
(defun resolve-value (context game-object uniform)
  (let ((value (value uniform)))
    (etypecase value
      (boolean value)
      ((or symbol function) (funcall value context game-object))
      (t value))))

(u:fn-> resolve-func (ctx::context gob::game-object uniform) t)
(defun resolve-func (context game-object uniform)
  (funcall (func uniform)
           (program uniform)
           (key uniform)
           (resolve-value context game-object uniform)))

(u:fn-> load-texture (ctx::context mat.def::material uniform) null)
(defun load-texture (context material uniform)
  (declare (optimize speed))
  (let ((value (value uniform)))
    (when (eq (resolved-type uniform) :sampler)
      (let ((material-name (mat.data::name (mat.def::data material)))
            (texture (tex::load context value)))
        (setf (value uniform) texture)
        (pushnew material-name (tex::materials texture))
        (pushnew value (mat.def::textures material))))
    nil))

(u:fn-> make-material-uniforms (ctx::context mat.def::material) null)
(defun make-material-uniforms (context material)
  (declare (optimize speed))
  (let ((material-data (mat.def::data material)))
    (clrhash (mat.def::uniforms material))
    (dolist (texture-name (mat.def::textures material))
      (let ((texture (ap::find context :texture texture-name)))
        (u:deletef (tex::materials texture) (mat.data::name material-data))))
    (setf (mat.def::textures material) nil)
    (u:do-hash (k v (mat.data::copy-uniforms material-data))
      (let ((uniform (make-uniform :key k :value v)))
        (register material uniform)
        (load-texture context material uniform)
        (setf (u:href (mat.def::uniforms material) k) uniform)))
    nil))
