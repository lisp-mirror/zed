(in-package #:zed)

(defun resolve-asset-path (asset)
  (destructuring-bind (system-name path) asset
    #+zed.release
    (format nil "/~s/~a" system-name path)
    #-zed.release
    (let ((path (uiop:merge-pathnames* path #.(make-pathname :directory '(:relative "data")))))
      (asdf:system-relative-pathname system-name path))))

(defmacro with-asset ((asset path data &key (length-binding '#:length) (format :binary)) &body body)
  `(let ((,path (resolve-asset-path ,asset)))
     #+zed.release
     (with-pack-file (,data ,path :format ,format)
       (let ((,length-binding (util.ss::length ,data)))
         (declare (ignorable ,length-binding))
         ,@body))
     #-zed.release
     ,(ecase format
        (:binary
         `(u:with-binary-input (,data ,path)
            (let ((,length-binding (file-length ,data)))
              (declare (ignorable ,length-binding))
              ,@body)))
        (:string
         `(with-open-file (,data ,path)
            (u:read-stream-content-into-string ,data)))
        (:lisp
         `(with-open-file (,data ,path)
            (read ,data))))))
