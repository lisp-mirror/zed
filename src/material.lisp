(in-package #:zed)

(defstruct (material
            (:constructor %make-material)
            (:predicate nil)
            (:copier nil))
  (data nil :type material-data)
  (uniforms (u:dict #'eq) :type hash-table)
  (framebuffer nil :type (or framebuffer null))
  (attachments nil :type list)
  (texture-unit-state 0 :type u:ub8)
  (textures nil :type list))

(u:define-printer (material stream :type nil)
  (format stream "MATERIAL: ~s" (material-data-name (material-data material))))

(u:fn-> ensure-framebuffer (context material) null)
(defun ensure-framebuffer (context material)
  (declare (optimize speed))
  (u:when-let* ((data (material-data material))
                (framebuffer-name (material-data-framebuffer data))
                (framebuffer (load-framebuffer context framebuffer-name))
                (attachments (framebuffer-attachment-names->points
                              framebuffer
                              (material-data-attachments data))))
    (setf (material-framebuffer material) framebuffer
          (material-attachments material) attachments)
    nil))

(u:fn-> make-material (context symbol) material)
(defun make-material (context type)
  (declare (optimize speed))
  (let* ((data (find-material-data type))
         (material (%make-material :data data))
         (materials (context-materials context)))
    (unless (u:href materials type)
      (setf (u:href materials type) (u:dict #'eq)))
    (setf (u:href materials type material) material)
    (make-uniforms context material)
    (ensure-framebuffer context material)
    material))

(u:fn-> destroy-material (context material) null)
(defun destroy-material (context material)
  (let ((type (material-data-name (material-data material))))
    (remhash material (u:href (context-materials context) type))
    nil))

(u:fn-> set-uniform (material keyword t) null)
(defun set-uniform (material key value)
  (declare (optimize speed))
  (with-allowed-scopes set-uniform (:trait-render-hook)
    (let ((uniforms (material-uniforms material)))
      (unless (u:href uniforms key)
        (setf (u:href uniforms key) (make-uniform :key key)))
      (let ((uniform (u:href uniforms key)))
        (setf (uniform-value uniform) value)
        (unless (uniform-func uniform)
          (register-uniform material uniform)))))
  nil)

(defun generate-material-render-func (features output)
  (destructuring-bind (&key enable disable blend-mode depth-mode polygon-mode line-width point-size)
      features
    (u:with-gensyms (context game-object material x)
      (let ((enable (set-difference enable +enabled-capabilities+))
            (disable (set-difference disable +disabled-capabilities+))
            (blend-mode (and (not (equal blend-mode +blend-mode+)) blend-mode))
            (depth-mode (and (not (eq depth-mode +depth-mode+)) depth-mode))
            (polygon-mode (and (not (eq polygon-mode +polygon-mode+)) polygon-mode)))
        `(lambda (,context ,game-object ,material)
           (with-framebuffer ,(when output
                                `(material-framebuffer ,material))
               (:attachments (material-attachments ,material))
             (shadow:with-shader (material-data-shader (material-data ,material))
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
               (u:do-hash-values (,x (material-uniforms ,material))
                 (resolve-uniform-function ,context ,game-object ,x))
               (invoke-trait-hook ,context :render :game-object ,game-object)
               (setf (material-texture-unit-state ,material) 0)
               ,@(when disable
                   `((gl:enable ,@disable)))
               ,@(when enable
                   `((gl:disable ,@enable)))
               ,@(when blend-mode
                   `((gl:blend-func ,@+blend-mode+)))
               ,@(when depth-mode
                   `((gl:depth-func ,+depth-mode+)))
               ,@(when polygon-mode
                   `((gl:polygon-mode :front-and-back ,+polygon-mode+)))
               ,@(when line-width
                   `((gl:line-width 1.0)))
               ,@(when point-size
                   `((gl:point-size 1.0))))))))))

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms features pass output) (car body)
    (u:with-gensyms (func)
      `(let ((,func ,(generate-material-render-func features output)))
         (if (u:href =materials= ',name)
             (update-material-data ',name ',master ',shader (list ,@uniforms) ',pass ',output ,func)
             (make-material-data ',name ',master ',shader (list ,@uniforms) ',pass ',output
                                 ,func))))))

(defmethod recompile ((type (eql :material)) data)
  (u:when-let ((shader (material-data-shader (find-material-data data))))
    (recompile :shader (list shader)))
  (u:do-hash-keys (material (u:href (context-materials =context=) data))
    (make-uniforms =context= material)
    (ensure-framebuffer =context= material))
  (v:debug :zed "Recompiled material: ~s" data))
