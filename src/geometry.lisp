(in-package #:zed)

(defstruct (geometry-data
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (id 0 :type u:ub32)
  (layout (%make-geometry-layout) :type geometry-layout)
  (buffers (vector) :type vector)
  (buffer-names (u:dict #'eq) :type hash-table)
  (primitive :triangles :type keyword)
  (vertex-count 0 :type u:ub32)
  (primitive-count 0 :type u:ub32))

(glob:define-global-var =geometry= (u:dict #'eq))

(defun get-geometry-buffer-names (group)
  (ecase (geometry-group-format group)
    (:separate
     (let ((names nil))
       (u:do-hash-keys (k (geometry-group-attributes group))
         (push (u:format-symbol :keyword "~a/~a" (geometry-group-name group) k) names))
       (nreverse names)))
    (:interleaved
     (list (geometry-group-name group)))))

(defun make-geometry-buffers (geometry)
  (let ((layout (geometry-data-layout geometry))
        (buffers (make-array 0 :fill-pointer 0 :adjustable t)))
    (setf (geometry-data-buffers geometry) buffers)
    (dolist (group-name (geometry-layout-group-order layout))
      (let ((group (u:href (geometry-layout-groups layout) group-name)))
        (dolist (name (get-geometry-buffer-names group))
          (let ((buffer (gl:gen-buffer)))
            (setf (u:href (geometry-data-buffer-names geometry) name) buffer)
            (vector-push-extend buffer (geometry-data-buffers geometry))))))))

(defun configure-geometry-buffers (geometry)
  (let ((layout (geometry-data-layout geometry))
        (buffer-offset 0)
        (attribute-offset 0))
    (dolist (group-name (geometry-layout-group-order layout))
      (let* ((group (u:href (geometry-layout-groups layout) group-name))
             (buffer-count (get-geometry-buffer-count group))
             (group-buffers (make-array buffer-count
                                        :displaced-to (geometry-data-buffers geometry)
                                        :displaced-index-offset buffer-offset))
             (attribute-count (length (geometry-group-attribute-order group))))
        (dotimes (i attribute-count)
          (gl:enable-vertex-attrib-array (+ attribute-offset i)))
        (configure-geometry-group group attribute-offset group-buffers)
        (incf buffer-offset buffer-count)
        (incf attribute-offset attribute-count)))))

(u:fn-> get-geometry-buffer-size (vector) fixnum)
(defun get-geometry-buffer-size (buffer)
  (declare (optimize speed))
  (* (the (u:ub32) (length buffer))
     (etypecase buffer
       (u:b8a 1)
       (u:ub8a 1)
       (u:b16a 2)
       (u:ub16a 2)
       (u:b32a 4)
       (u:ub32a 4)
       (u:f32a 4)
       (u:f64a 8))))

(defmacro with-geometry-buffer ((ptr size vector) &body body)
  (u:with-gensyms (sv)
    `(sv:with-static-vector
         (,sv (length ,vector) :element-type (array-element-type ,vector) :initial-contents ,vector)
       (let ((,size (get-geometry-buffer-size ,vector))
             (,ptr (sv:static-vector-pointer ,sv)))
         ,@body))))

(defun fill-geometry-buffer (geometry name data &key (usage :dynamic-draw))
  (let ((data (u:flatten-numbers data)))
    (with-geometry-buffer (ptr size data)
      (let ((buffer (u:href (geometry-data-buffer-names geometry) name)))
        (gl:bind-buffer :array-buffer buffer)
        (%gl:buffer-data :array-buffer size ptr usage)
        (gl:bind-buffer :array-buffer 0)
        (values)))))

(defun make-geometry (name)
  (let ((geometry-data (u:href =geometry= name)))
    (unless geometry-data
      (error "Geometry ~s is not defined." name))
    (let* ((geometry (funcall (u:href =geometry= name)))
           (id (geometry-data-id geometry)))
      (v:debug :zed "Created geometry: ~s (VAO: ~d)" name id)
      geometry)))

(defun update-geometry (geometry buffer-name data)
  (let ((data (or data (make-array (geometry-data-vertex-count geometry) :initial-element 0))))
    (fill-geometry-buffer geometry buffer-name data)
    (setf (geometry-data-primitive-count geometry) (length data))
    (values)))

(defun draw-geometry (geometry instance-count)
  (gl:bind-vertex-array (geometry-data-id geometry))
  (%gl:draw-arrays-instanced (geometry-data-primitive geometry)
                             0
                             (* (geometry-data-primitive-count geometry)
                                (geometry-data-vertex-count geometry))
                             instance-count)
  (gl:bind-vertex-array 0)
  (values))

(defun delete-geometry (geometry)
  (let ((name (geometry-data-name geometry))
        (id (geometry-data-id geometry)))
    (gl:delete-buffers (geometry-data-buffers geometry))
    (gl:delete-vertex-arrays (list id))
    (v:debug :zed "Deleted geometry: ~s (VAO: ~d)" name id)))

(defmacro define-geometry (name options &body body)
  (declare (ignore options))
  (u:with-gensyms (data func)
    (destructuring-bind (&key layout (primitive :triangles) (vertex-count 0) buffers) (car body)
      `(let ((,func (lambda ()
                      (let ((,data (make-geometry-data :name ',name
                                                       :id (gl:gen-vertex-array)
                                                       :layout (find-geometry-layout ',layout)
                                                       :vertex-count ,vertex-count
                                                       :primitive ',primitive)))
                        (gl:bind-vertex-array (geometry-data-id ,data))
                        (make-geometry-buffers ,data)
                        (configure-geometry-buffers ,data)
                        ,@(loop :for (k v) :on buffers :by #'cddr
                                :collect `(update-geometry ,data ,k ',v))
                        ,data))))
         (setf (u:href =geometry= ',name) ,func)))))
