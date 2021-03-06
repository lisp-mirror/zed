(in-package #:zed)

(defstruct (shader-manager
            (:predicate nil)
            (:copier nil))
  (bindings (u:dict #'equalp) :type hash-table)
  (released-bindings nil :type list))

(u:define-printer (shader-manager stream :type nil)
  (format stream "SHADER-MANAGER"))

(defun register-shaders ()
  (let ((table (shadow:load-shaders (lambda (x) (thread-pool-enqueue (list :shader x))))))
    (v:info :zed "Loaded ~d shader programs" (hash-table-count table))
    table))

(defun get-shader-buffer-binding (manager)
  (declare (optimize speed))
  (let ((id-count (hash-table-count (shader-manager-bindings manager)))
        (max-bindings (gl:get* :max-shader-storage-buffer-bindings)))
    (declare (u:ub16 max-bindings))
    (when (= id-count max-bindings)
      (error "Cannot create shader buffer. Maximum bindings reached: ~d" max-bindings))
    (or (pop (shader-manager-released-bindings manager))
        (1+ id-count))))

(defun release-shader-buffer-binding (manager key)
  (declare (optimize speed))
  (u:when-let* ((bindings (shader-manager-bindings manager))
                (id (u:href bindings key)))
    (when (typep id 'u:ub16)
      (remhash key bindings)
      (pushnew id (shader-manager-released-bindings manager))
      (setf (shader-manager-released-bindings manager)
            (sort (copy-list (shader-manager-released-bindings manager)) #'<)))
    nil))

(declaim (inline read-shader-buffer))
(defun read-shader-buffer (key &key path (index 0) count)
  (declare (optimize speed))
  (shadow:read-buffer-path key :path path :index index :count count))

(declaim (inline write-shader-buffer))
(defun write-shader-buffer (key &key path (index 0) value)
  (declare (optimize speed))
  (shadow:write-buffer-path key :path path :index index :value value))

(declaim (inline find-shader-buffer))
(defun find-shader-buffer (key)
  (declare (optimize speed))
  (shadow:find-buffer key))

(declaim (inline clear-shader-buffer))
(defun clear-shader-buffer (key)
  (shadow:clear-buffer key))

(defun delete-shader-buffer (manager key)
  (declare (optimize speed))
  (release-shader-buffer-binding manager key)
  (shadow:clear-buffer key)
  (shadow:delete-buffer key)
  (shadow:unbind-block key))

(defun make-shader-buffer (manager key)
  (let ((bindings (shader-manager-bindings manager))
        (binding (get-shader-buffer-binding manager)))
    (when (find-shader-buffer key)
      (delete-shader-buffer manager key))
    (setf (u:href bindings key) binding)
    (shadow:create-buffer key key)
    (shadow:bind-buffer key binding)
    binding))

(defun bind-shader-buffer (manager key)
  (let* ((bindings (shader-manager-bindings manager))
         (binding (u:href bindings key)))
    (shadow:bind-block key binding)
    (shadow:bind-buffer key binding)))

(defun unbind-shader-buffer (key)
  (shadow:unbind-buffer key)
  (shadow:unbind-block key))

(defmacro with-shader-buffer ((manager key) &body body)
  `(progn
     (bind-shader-buffer ,manager ,key)
     ,@body
     (unbind-shader-buffer ,key)))

(defmethod recompile ((type (eql :shader)) data)
  (shadow:recompile-shaders data)
  (dolist (x data)
    (v:debug :zed "Recompiled shader: ~s" x)))
