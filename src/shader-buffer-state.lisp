(in-package #:cl-user)

(defpackage #:%zed.shader-buffer-state
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:read
   #:write))

(in-package #:%zed.shader-buffer-state)

(defstruct (state
            (:constructor make-state)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (bindings (u:dict #'equalp) :type hash-table)
  (released-bindings nil :type list))

(u:define-printer (state stream :type nil)
  (format stream "SHADER-BUFFER-STATE"))

(defun get-binding (state)
  (declare (optimize speed))
  (let ((id-count (hash-table-count (bindings state)))
        (max-bindings (gl:get* :max-shader-storage-buffer-bindings)))
    (declare (u:ub16 max-bindings))
    (when (= id-count max-bindings)
      (error "Cannot create shader buffer. Maximum bindings reached: ~d" max-bindings))
    (or (pop (released-bindings state))
        (1+ id-count))))

(defun release (state key)
  (declare (optimize speed))
  (u:when-let* ((bindings (bindings state))
                (id (u:href bindings key)))
    (when (typep id 'u:ub16)
      (remhash key bindings)
      (pushnew id (released-bindings state))
      (setf (released-bindings state) (sort (copy-list (released-bindings state)) #'<)))
    nil))

(declaim (inline read))
(defun read (key &key path (index 0) count)
  (declare (optimize speed))
  (shadow:read-buffer-path key :path path :index index :count count))

(declaim (inline write))
(defun write (key &key path (index 0) value)
  (declare (optimize speed))
  (shadow:write-buffer-path key :path path :index index :value value))

(declaim (inline find-buffer))
(defun find-buffer (key)
  (declare (optimize speed))
  (shadow:find-buffer key))

(declaim (inline clear-buffer))
(defun clear-buffer (key)
  (shadow:clear-buffer key))

(defun delete-buffer (state key)
  (declare (optimize speed))
  (release state key)
  (shadow:clear-buffer key)
  (shadow:delete-buffer key)
  (shadow:unbind-block key))

(defun make-buffer (state key block-id shader)
  (declare (optimize speed))
  (let ((bindings (bindings state))
        (binding (get-binding state)))
    (when (find-buffer key)
      (delete-buffer state key))
    (setf (u:href bindings key) binding)
    (shadow:create-block-alias :buffer block-id shader key)
    (shadow:bind-block key binding)
    (shadow:create-buffer key key)
    (shadow:bind-buffer key binding)
    binding))

(defun bind-buffer (state key)
  (let* ((bindings (bindings state))
         (binding (u:href bindings key)))
    (shadow:bind-block key binding)
    (shadow:bind-buffer key binding)))

(defun unbind-buffer (key)
  (shadow:unbind-buffer key)
  (shadow:unbind-block key))

(defmacro with-buffers ((state &rest keys) &body body)
  (u:with-gensyms (table)
    (let ((key-syms (mapcar (lambda (x) (list (u:make-gensym x) x)) keys)))
      `(let ((,table (bindings ,state))
             ,@key-syms)
         ,@(mapcar
            (lambda (x)
              `(shadow:bind-block ,(car x) (u:href ,table ,(car x))))
            key-syms)
         ,@body
         ,@(mapcar
            (lambda (x)
              `(unbind-buffer ,(car x)))
            key-syms)))))
