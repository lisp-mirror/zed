(in-package #:cl-user)

(defpackage #:%zed.geometry
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:geo.buf #:%zed.geometry.buffer)
   (#:geo.data #:%zed.geometry.data)
   (#:geo.layout.data #:%zed.geometry.layout.data)
   (#:log #:%zed.logging))
  (:use #:cl)
  (:shadow
   #:delete)
  (:export
   #:define-geometry))

(in-package #:%zed.geometry)

(defun make-geometry (name)
  (let* ((geometry (funcall (u:href geo.data::=data= name)))
         (id (geo.data::id geometry)))
    (log::debug :zed.geometry "Created geometry: ~s (VAO: ~d)" name id)
    geometry))

(defun update (geometry buffer-name data)
  (let ((data (or data (make-array (geo.data::vertex-count geometry) :initial-element 0))))
    (geo.buf::fill-buffer geometry buffer-name data)
    (values)))

(defun draw (geometry instance-count)
  (gl:bind-vertex-array (geo.data::id geometry))
  (%gl:draw-arrays-instanced (geo.data::primitive geometry)
                             0
                             (geo.data::vertex-count geometry)
                             instance-count)
  (gl:bind-vertex-array 0)
  (values))

(defun delete (geometry)
  (let ((name (geo.data::name geometry))
        (id (geo.data::id geometry)))
    (gl:delete-buffers (geo.data::buffers geometry))
    (gl:delete-vertex-arrays (list id))
    (log::debug :zed.geometry "Deleted geometry: ~s (VAO: ~d)" name id)))

(defmacro define-geometry (name options &body body)
  (declare (ignore options))
  (u:with-gensyms (data func)
    (destructuring-bind (&key layout (primitive :triangles) (vertex-count 0) buffers) (car body)
      `(let ((,func (lambda ()
                      (let ((,data (geo.data::%make-data :name ',name
                                                         :id (gl:gen-vertex-array)
                                                         :layout (geo.layout.data::find ',layout)
                                                         :vertex-count ,vertex-count
                                                         :primitive ',primitive)))
                        (gl:bind-vertex-array (geo.data::id ,data))
                        (geo.buf::make-buffers ,data)
                        (geo.buf::configure-buffers ,data)
                        ,@(loop :for (k v) :on buffers :by #'cddr
                                :collect `(update ,data ,k ',v))
                        ,data))))
         (setf (u:href geo.data::=data= ',name) ,func)))))
