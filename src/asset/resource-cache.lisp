(in-package #:zed)

(defun find-resource (core type key)
  (u:href (core-resource-cache core) type key))

(defun delete-resource (core type key)
  (remhash key (u:href (core-resource-cache core) type)))

(defmacro with-resource-cache ((core type key) &body body)
  (u:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (core-resource-cache ,core) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (u:ensure-gethash ,key ,table (progn ,@body)))))
