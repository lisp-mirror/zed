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

(u:fn-> ensure-framebuffer (core material) null)
(defun ensure-framebuffer (core material)
  (declare (optimize speed))
  (u:when-let* ((data (material-data material))
                (framebuffer-name (material-data-framebuffer data))
                (framebuffer (load-framebuffer core framebuffer-name))
                (attachments (framebuffer-attachment-names->points
                              framebuffer
                              (material-data-attachments data))))
    (setf (material-framebuffer material) framebuffer
          (material-attachments material) attachments)
    nil))

(u:fn-> make-material-shader-buffers (core material) null)
(defun make-material-shader-buffers (core material)
  (declare (optimize speed))
  (let ((shader (material-data-shader (material-data material))))
    (with-resource-cache (core :shader-bindings (cons :camera shader))
      (make-shader-buffer (core-shader-manager core) :camera :camera shader))
    nil))

(u:fn-> make-material (core symbol) material)
(defun make-material (core type)
  (declare (optimize speed))
  (let* ((data (find-material-data type))
         (material (%make-material :data data))
         (materials (core-materials core)))
    (unless (u:href materials type)
      (setf (u:href materials type) (u:dict #'eq)))
    (setf (u:href materials type material) material)
    (make-uniforms core material)
    (ensure-framebuffer core material)
    (make-material-shader-buffers core material)
    material))

(u:fn-> destroy-material (core material) null)
(defun destroy-material (core material)
  (let ((type (material-data-name (material-data material))))
    (remhash material (u:href (core-materials core) type))
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
    (u:with-gensyms (core game-object material x)
      (let ((enable (set-difference enable +enabled-capabilities+))
            (disable (set-difference disable +disabled-capabilities+))
            (blend-mode (and (not (equal blend-mode +blend-mode+)) blend-mode))
            (depth-mode (and (not (eq depth-mode +depth-mode+)) depth-mode))
            (polygon-mode (and (not (eq polygon-mode +polygon-mode+)) polygon-mode)))
        `(lambda (,core ,game-object ,material)
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
                 (resolve-uniform-function ,core ,game-object ,x))
               (invoke-trait-hook ,core :render :game-object ,game-object)
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
  (let ((material-data (find-material-data data)))
    (u:when-let ((shader (material-data-shader material-data)))
      (recompile :shader (list shader)))
    (u:when-let ((table (u:href (core-materials =core=) data)))
      (u:do-hash-keys (material table)
        (make-uniforms =core= material)
        (ensure-framebuffer =core= material))
      (v:debug :zed "Recompiled material: ~s" data)
      (dolist (slave (material-data-slaves material-data))
        (recompile :material slave)))))
