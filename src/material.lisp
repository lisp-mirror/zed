(in-package #:cl-user)

(defpackage #:%zed.material
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:fb #:%zed.framebuffer)
   (#:gob #:%zed.game-object)
   (#:mat.data #:%zed.material.data)
   (#:mat.def #:%zed.material.definition)
   (#:ogl #:%zed.opengl)
   (#:trait #:%zed.trait)
   (#:uni #:%zed.material.uniform))
  (:use #:cl))

(in-package #:%zed.material)

(u:fn-> ensure-framebuffer (ctx::context mat.def::material) null)
(defun ensure-framebuffer (context material)
  (declare (optimize speed))
  (u:when-let* ((data (mat.def::data material))
                (framebuffer-name (mat.data::framebuffer data))
                (framebuffer (fb::load context framebuffer-name))
                (attachments (fb::attachment-names->points framebuffer
                                                           (mat.data::attachments data))))
    ;; TODO: The check for valid framebuffer in ndjinn is not correct.
    (setf (mat.def::framebuffer material) framebuffer
          (mat.def::attachments material) attachments)
    nil))

(u:fn-> make-material (ctx::context symbol) mat.def::material)
(defun make-material (context type)
  (declare (optimize speed))
  (let* ((materials (ctx::materials context))
         (data (mat.data::find type))
         (material (mat.def::%make-material :data data)))
    (uni::make-material-uniforms context material)
    (ensure-framebuffer context material)
    (setf (u:href materials type) material)
    material))

(u:fn-> set-uniforms (mat.def::material &rest t) null)
(defun set-uniforms (material &rest args)
  (declare (optimize speed))
  (let ((uniforms (mat.def::uniforms material)))
    (u:do-plist (k v args)
      (unless (u:href uniforms k)
        (setf (u:href uniforms k) (uni::make-uniform :key k)))
      (let ((uniform (u:href uniforms k)))
        (setf (uni::value uniform) v)
        (unless (uni::func uniform)
          (uni::register material uniform))))
    nil))

(defun generate-render-func (features)
  (destructuring-bind (&key enable disable blend-mode depth-mode polygon-mode line-width point-size)
      features
    (u:with-gensyms (game-object material)
      (let ((enable (set-difference enable ogl::+enabled-capabilities+))
            (disable (set-difference disable ogl::+disabled-capabilities+))
            (blend-mode (and (not (equal blend-mode ogl::+blend-mode+)) blend-mode))
            (depth-mode (and (not (eq depth-mode ogl::+depth-mode+)) depth-mode))
            (polygon-mode (and (not (eq polygon-mode ogl::+polygon-mode+)) polygon-mode)))
        `(lambda (,game-object ,material)
           (fb::with-framebuffer (mat.def::framebuffer ,material)
               (:attachments (mat.def::attachments ,material))
             (shadow:with-shader (mat.data::shader (mat.def::data ,material))
               ,@(when enable
                   `((gl:enable ,@enable)))
               ,@(when disable
                   `((gl:disable ,@disable)))
               ,@(when blend-mode
                   `((gl:blend-func ,@blend-mode)))
               ,@(when depth-mode
                   `((gl:depth-func ,depth-mode)))
               ,@(when polygon-mode
                   `((gl:polygon-mode :front-and-back ,polygon-mode)))
               ,@(when line-width
                   `((gl:line-width ,line-width)))
               ,@(when point-size
                   `((gl:point-size ,point-size)))
               (dolist (x (gob::traits ,game-object))
                 (funcall (fdefinition (trait::pre-render-hook x)) x))
               (u:do-hash-values (v (mat.def::uniforms ,material))
                 (uni::resolve-func ,game-object v))
               (dolist (x (gob::traits ,game-object))
                 (funcall (fdefinition (trait::render-hook x)) x))
               (setf (mat.def::texture-unit-state ,material) 0)
               ,@(when disable
                   `((gl:enable ,@disable)))
               ,@(when enable
                   `((gl:disable ,@enable)))
               ,@(when blend-mode
                   `((gl:blend-func ,@ogl::+blend-mode+)))
               ,@(when depth-mode
                   `((gl:depth-func ,ogl::+depth-mode+)))
               ,@(when polygon-mode
                   `((gl:polygon-mode :front-and-back ,ogl::+polygon-mode+)))
               ,@(when line-width
                   `((gl:line-width 1.0)))
               ,@(when point-size
                   `((gl:point-size 1.0))))))))))

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms features pass output) (car body)
    (u:with-gensyms (func)
      `(let ((,func ,(generate-render-func features)))
         (if (u:href mat.data::=data= ',name)
             (mat.data::update ',name ',master ',shader (list ,@uniforms) ',pass ',output ,func)
             (mat.data::make-data ',name ',master ',shader (list ,@uniforms) ',pass ',output ,func))))))
