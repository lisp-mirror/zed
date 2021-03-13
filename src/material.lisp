(in-package #:cl-user)

(defpackage #:%zed.material
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:data #:%zed.material.data)
   (#:fb #:%zed.framebuffer)
   (#:gob #:%zed.game-object)
   (#:matdef #:%zed.material.definition)
   (#:ogl #:%zed.opengl)
   (#:ren #:%zed.trait.render)
   (#:trait #:%zed.trait)
   (#:uni #:%zed.material.uniform))
  (:use #:cl))

(in-package #:%zed.material)

(u:fn-> ensure-framebuffer (ctx::context matdef::material) null)
(defun ensure-framebuffer (context material)
  (declare (optimize speed))
  (u:when-let* ((data (matdef::data material))
                (framebuffer-name (data::framebuffer data))
                (framebuffer (fb::load context framebuffer-name))
                (attachments (fb::attachment-names->points framebuffer (data::attachments data))))
    ;; TODO: The check for valid framebuffer in ndjinn is not correct.
    (setf (matdef::framebuffer material) framebuffer
          (matdef::attachments material) attachments)
    nil))

(u:fn-> make-material (ctx::context symbol) matdef::material)
(defun make-material (context type)
  (declare (optimize speed))
  (let* ((materials (ctx::materials context))
         (data (data::find type))
         (material (matdef::%make-material :data data)))
    (uni::make-material-uniforms context material)
    (ensure-framebuffer context material)
    (setf (u:href materials type) material)
    material))

(u:fn-> ensure-material (ctx::context symbol) matdef::material)
(declaim (inline ensure-material))
(defun ensure-material (context type)
  (declare (optimize speed))
  (or (u:href (ctx::materials context) type)
      (make-material context type)))

(defun generate-render-func (features)
  (destructuring-bind (&key enable disable blend-mode depth-mode polygon-mode line-width point-size)
      features
    (u:with-gensyms (game-object render-trait material)
      (let ((enable (set-difference enable ogl::+enabled-capabilities+))
            (disable (set-difference disable ogl::+disabled-capabilities+))
            (blend-mode (and (not (equal blend-mode ogl::+blend-mode+)) blend-mode))
            (depth-mode (and (not (eq depth-mode ogl::+depth-mode+)) depth-mode))
            (polygon-mode (and (not (eq polygon-mode ogl::+polygon-mode+)) polygon-mode)))
        `(lambda (,render-trait)
           (let ((,game-object (trait::owner ,render-trait))
                 (,material (ren::current-material ,render-trait)))
             (fb::with-framebuffer (framebuffer ,material) (:attachments (attachments ,material))
               (shadow:with-shader (data::shader (data ,material))
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
                   (funcall (trait::pre-render-hook x) x))
                 (u:do-hash-values (v (matdef::uniforms ,material))
                   (uni::resolve-func ,game-object v))
                 (dolist (x (gob::traits ,game-object))
                   (funcall (trait::render-hook x) x))
                 (setf (matdef::texture-unit-state ,material) 0)
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
                     `((gl:point-size 1.0)))))))))))

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms features pass output) (car body)
    (u:with-gensyms (func)
      `(let ((,func ,(generate-render-func features)))
         (if (u:href =data= ',name)
             (data::update ',name ',master ',shader (list ,@uniforms) ',pass ',output ,func)
             (data::make-material ',name ',master ',shader (list ,@uniforms) ',pass ',output ,func))))))
