(in-package #:zed)

(defun find-resource (context type key)
  (u:href (context-resource-cache context) type key))

(defun delete-resource (context type key)
  (remhash key (u:href (context-resource-cache context) type)))

(defmacro with-resource-cache ((context type key) &body body)
  (u:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (context-resource-cache ,context) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (u:ensure-gethash ,key ,table (progn ,@body)))))
