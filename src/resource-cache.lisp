(in-package #:cl-user)

(defpackage #:%zed.resource-cache
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context))
  (:use #:cl)
  (:shadow
   #:delete
   #:find))

(in-package #:%zed.resource-cache)

(defun find (context type key)
  (u:href (ctx::resource-cache context) type key))

(defun delete (context type key)
  (remhash key (u:href (ctx::resource-cache context) type)))

(defmacro with-resource-cache ((context type key) &body body)
  (u:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (ctx::resource-cache ,context) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (u:ensure-gethash ,key ,table (progn ,@body)))))

(defun resolve-path (asset)
  (destructuring-bind (system-name path) asset
    (declare (ignorable system-name))
    (let ((path (uiop:merge-pathnames* path #.(make-pathname :directory '(:relative "data")))))
      #+zed.release
      (uiop:merge-pathnames* path (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))
      #-zed.release
      (asdf:system-relative-pathname system-name path))))
