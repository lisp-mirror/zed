(in-package #:cl-user)

(defpackage #:%zed.material.data
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:dbg #:%zed.debug)
   (#:fb #:%zed.framebuffer)
   (#:fb.data #:%zed.framebuffer.data)
   (#:tp #:%zed.thread-pool))
  (:use #:cl)
  (:shadow
   #:find))

(in-package #:%zed.material.data)

(glob:define-global-var =data= (u:dict #'eq))

(defstruct (data
            (:constructor %make-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (master nil :type symbol)
  (slaves nil :type list)
  (shader nil :type symbol)
  (direct-uniforms (u:dict #'eq) :type hash-table)
  (effective-uniforms (u:dict #'eq) :type hash-table)
  (pass nil :type symbol)
  (framebuffer nil :type symbol)
  (attachments nil :type list)
  (render-func (constantly nil) :type function))

(u:define-printer (data stream :type nil)
  (format stream "MATERIAL-DATA: ~s" (name data)))

(defun find (name)
  (or (u:href =data= name)
      (error "Material ~s is not defined." name)))

(defun find-master (data)
  (let* ((master-name (master data))
         (master-data (u:href =data= master-name)))
    (when (and master-name (not master-data))
      (error "Material ~s inherits from the unknown master ~s."
             (name data)
             master-name))
    master-data))

(defun copy-uniforms (data)
  (let ((uniforms (u:dict #'eq)))
    (labels ((copy (value)
               (typecase value
                 (sequence (map-into (copy-seq value) #'copy value))
                 (t value))))
      (when data
        (u:do-hash (k v (effective-uniforms data))
          (setf (u:href uniforms k) (copy v))))
      uniforms)))

(defun update-uniforms (data uniform-data)
  (let* ((master-data (find-master data))
         (new-direct (apply #'u:dict #'eq uniform-data))
         (new-effective (u:hash-merge (copy-uniforms master-data) new-direct)))
    (clrhash (direct-uniforms data))
    (u:do-hash (k v new-direct)
      (setf (u:href (direct-uniforms data) k) v))
    (clrhash (effective-uniforms data))
    (u:do-hash (k v new-effective)
      (setf (u:href (effective-uniforms data) k) v))))

(defun update-relationships (data)
  (u:when-let ((master (u:href =data= (master data))))
    (pushnew (name data) (slaves master))))

(defun update-framebuffer-link (material-name framebuffer-name)
  (u:do-hash-values (v fb.data::=data=)
    (dolist (framebuffer-material-name (fb.data::materials v))
      (when (eq material-name framebuffer-material-name)
        (u:deletef (fb.data::materials v) framebuffer-material-name))))
  (when framebuffer-name
    (push material-name (fb.data::materials (fb.data::find framebuffer-name)))))

(defun update (name master shader uniforms pass output func)
  (let* ((data (find name))
         (master-data (u:href =data= master))
         (shader (or shader (and master-data (shader master-data))))
         (shader-definition (shadow:find-shader-definition shader)))
    (when (and shader (not shader-definition))
      (error "Shader program ~s is not found for material ~s." shader name))
    (destructuring-bind (&optional framebuffer attachments) output
      (setf (master data) master
            (shader data) shader
            (pass data) (or pass :default)
            (framebuffer data) framebuffer
            (attachments data) attachments
            (render-func data) func)
      (update-framebuffer-link name framebuffer)
      (update-uniforms data uniforms)
      (update-relationships data)
      (tp::enqueue (list :material name))
      (update-slaves data))))

(defun update-slaves (master-data)
  (dolist (slave-name (slaves master-data))
    (let ((slave (find slave-name)))
      (update (name slave)
              (name master-data)
              (or (shader slave)
                  (shader master-data))
              (u:hash->plist (direct-uniforms slave))
              (or (pass slave)
                  (pass master-data))
              (list (or (framebuffer slave) (framebuffer master-data))
                    (or (attachments slave) (attachments master-data)))
              (render-func slave)))))

(defun make-data (name master shader uniforms pass output func)
  (let ((data (%make-data :name name)))
    (setf (u:href =data= name) data)
    (update name master shader uniforms pass output func)
    data))
