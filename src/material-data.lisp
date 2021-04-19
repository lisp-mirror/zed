(in-package #:zed)

(defstruct (material-data
            (:constructor %make-material-data)
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

(u:define-printer (material-data stream :type nil)
  (format stream "MATERIAL-DATA: ~s" (material-data-name material-data)))

(glob:define-global-var =materials= (u:dict #'eq))

(defun find-material-data (name)
  (or (u:href =materials= name)
      (error "Material ~s is not defined." name)))

(defun find-material-data-master (data)
  (let* ((master-name (material-data-master data))
         (master-data (u:href =materials= master-name)))
    (when (and master-name (not master-data))
      (error "Material ~s inherits from the unknown master ~s."
             (material-data-name data)
             master-name))
    master-data))

(defun copy-material-data-uniforms (data)
  (let ((uniforms (u:dict #'eq)))
    (labels ((copy (value)
               (typecase value
                 (sequence (map-into (copy-seq value) #'copy value))
                 (t value))))
      (when data
        (u:do-hash (k v (material-data-effective-uniforms data))
          (setf (u:href uniforms k) (copy v))))
      uniforms)))

(defun update-material-data-uniforms (data uniform-data)
  (let* ((master-data (find-material-data-master data))
         (new-direct (apply #'u:dict #'eq uniform-data))
         (new-effective (u:hash-merge (copy-material-data-uniforms master-data) new-direct)))
    (clrhash (material-data-direct-uniforms data))
    (u:do-hash (k v new-direct)
      (setf (u:href (material-data-direct-uniforms data) k) v))
    (clrhash (material-data-effective-uniforms data))
    (u:do-hash (k v new-effective)
      (setf (u:href (material-data-effective-uniforms data) k) v))))

(defun update-material-data-relationships (data)
  (u:when-let ((master (u:href =materials= (material-data-master data))))
    (pushnew (material-data-name data) (material-data-slaves master))))

(defun update-material-data-framebuffer-link (material-name framebuffer-name)
  (u:do-hash-values (v =framebuffers=)
    (dolist (framebuffer-material-name (framebuffer-data-materials v))
      (when (eq material-name framebuffer-material-name)
        (u:deletef (framebuffer-data-materials v) framebuffer-material-name))))
  (when framebuffer-name
    (push material-name (framebuffer-data-materials (find-framebuffer-data framebuffer-name)))))

(defun update-material-data (name master shader uniforms pass output func)
  (let* ((data (find-material-data name))
         (master-data (u:href =materials= master))
         (shader (or shader (and master-data (material-data-shader master-data)))))
    (destructuring-bind (&optional framebuffer attachments) output
      (setf (material-data-master data) master
            (material-data-shader data) shader
            (material-data-pass data) (or pass :default)
            (material-data-framebuffer data) framebuffer
            (material-data-attachments data) attachments
            (material-data-render-func data) func)
      (update-material-data-framebuffer-link name framebuffer)
      (update-material-data-uniforms data uniforms)
      (update-material-data-relationships data)
      (thread-pool-enqueue (list :material name))
      (update-material-data-slaves data))))

(defun update-material-data-slaves (master-data)
  (dolist (slave-name (material-data-slaves master-data))
    (let ((slave (find-material-data slave-name)))
      (update-material-data (material-data-name slave)
                            (material-data-name master-data)
                            (or (material-data-shader slave)
                                (material-data-shader master-data))
                            (u:hash->plist (material-data-direct-uniforms slave))
                            (or (material-data-pass slave)
                                (material-data-pass master-data))
                            (list (or (material-data-framebuffer slave)
                                      (material-data-framebuffer master-data))
                                  (or (material-data-attachments slave)
                                      (material-data-attachments master-data)))
                            (material-data-render-func slave)))))

(defun make-material-data (name master shader uniforms pass output func)
  (let ((data (%make-material-data :name name)))
    (setf (u:href =materials= name) data)
    (update-material-data name master shader uniforms pass output func)
    data))
