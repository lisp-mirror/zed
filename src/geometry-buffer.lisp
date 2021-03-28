(in-package #:cl-user)

(defpackage #:%zed.geometry.buffer
  ;; Third-party aliases
  (:local-nicknames
   (#:sv #:static-vectors)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:geo.data #:%zed.geometry.data)
   (#:geo.group #:%zed.geometry.group)
   (#:geo.layout.data #:%zed.geometry.layout.data))
  (:use #:cl))

(in-package #:%zed.geometry.buffer)

(defun get-names (group)
  (ecase (geo.group::format group)
    (:separate
     (let ((names nil))
       (u:do-hash-keys (k (geo.group::attributes group))
         (push (u:format-symbol :keyword "~a/~a" (geo.group::name group) k) names))
       (nreverse names)))
    (:interleaved
     (list (geo.group::name group)))))

(defun make-buffers (geometry)
  (let ((layout (geo.data::layout geometry))
        (buffers (make-array 0 :fill-pointer 0 :adjustable t)))
    (setf (geo.data::buffers geometry) buffers)
    (dolist (group-name (geo.layout.data::group-order layout))
      (let ((group (u:href (geo.layout.data::groups layout) group-name)))
        (dolist (name (get-names group))
          (let ((buffer (gl:gen-buffer)))
            (setf (u:href (geo.data::buffer-names geometry) name) buffer)
            (vector-push-extend buffer (geo.data::buffers geometry))))))))

(defun configure-buffers (geometry)
  (let ((layout (geo.data::layout geometry))
        (buffer-offset 0)
        (attribute-offset 0))
    (dolist (group-name (geo.layout.data::group-order layout))
      (let* ((group (u:href (geo.layout.data::groups layout) group-name))
             (buffer-count (geo.group::get-buffer-count group))
             (group-buffers (make-array buffer-count
                                        :displaced-to (geo.data::buffers geometry)
                                        :displaced-index-offset buffer-offset))
             (attribute-count (length (geo.group::attribute-order group))))
        (dotimes (i attribute-count)
          (gl:enable-vertex-attrib-array (+ attribute-offset i)))
        (geo.group::configure group attribute-offset group-buffers)
        (incf buffer-offset buffer-count)
        (incf attribute-offset attribute-count)))))

(u:fn-> get-size (vector) fixnum)
(defun get-size (buffer)
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

(defmacro with-buffer ((ptr size vector) &body body)
  (u:with-gensyms (sv)
    `(sv:with-static-vector
         (,sv (length ,vector) :element-type (array-element-type ,vector) :initial-contents ,vector)
       (let ((,size (get-size ,vector))
             (,ptr (sv:static-vector-pointer ,sv)))
         ,@body))))

(defun fill-buffer (geometry name data &key (usage :dynamic-draw))
  (let ((data (u:flatten-numbers data)))
    (with-buffer (ptr size data)
      (let ((buffer (u:href (geo.data::buffer-names geometry) name)))
        (gl:bind-buffer :array-buffer buffer)
        (%gl:buffer-data :array-buffer size ptr usage)
        (gl:bind-buffer :array-buffer 0)
        (values)))))
