(in-package #:cl-user)

(defpackage #:%zed.base.ordered-class
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.base.ordered-class)

(defclass ordered-class (standard-class)
  ((%order :reader order
           :initarg :order
           :initform nil)))

(defmethod c2mop:validate-superclass ((class ordered-class) (superclass standard-class))
  t)

(defmethod c2mop:compute-slots ((class ordered-class))
  (let* ((order (order class))
         (length (length order)))
    (sort (copy-list (call-next-method))
          (lambda (x y)
            (< (or (position x order) length)
               (or (position y order) length)))
          :key #'c2mop:slot-definition-name)))

(defun collect-accessor-names (slot-options accessor-type)
  (let ((filtered-keys (remove accessor-type (u:plist-keys slot-options))))
    (u:plist-values (apply #'u:plist-remove slot-options filtered-keys))))

(defun generate-fast-accessors (class-name slots order)
  (u:mappend
   (lambda (slot)
     (destructuring-bind (slot-name &rest slot-options &key type inline &allow-other-keys) slot
       (when (find slot-name order)
         (let ((access `(c2mop:standard-instance-access ,class-name ,(position slot-name order))))
           `(,@(mapcan
                (lambda (x)
                  `(,@(when inline `((declaim (inline ,x))))
                    ,@(when type `((u:fn-> ,x (,class-name) ,type)))
                    (defun ,x (,class-name) ,access)))
                (collect-accessor-names slot-options :reader))
             ,@(mapcan
                (lambda (x)
                  `(,@(when inline `((declaim (inline ,x))))
                    ,@(when type `((u:fn-> ,x (,type ,class-name) ,type)))
                    (defun ,x (value ,class-name) (setf ,access value))))
                (collect-accessor-names slot-options :writer))
             ,@(mapcan
                (lambda (x)
                  `(,@(when inline `((declaim (inline ,x))))
                    ,@(when type `((u:fn-> ,x (,class-name) ,type)))
                    (defun ,x (,class-name) ,access)
                    ,@(when inline `((declaim (inline (setf ,x)))))
                    ,@(when type `((u:fn-> (setf ,x) (,type ,class-name) ,type)))
                    (defun (setf ,x) (value ,class-name) (setf ,access value))))
                (collect-accessor-names slot-options :accessor)))))))
   slots))

(defun generate-slot-specifiers (slots order)
  (mapcar
   (lambda (x)
     (destructuring-bind (slot-name . slot-options) x
       (let ((to-remove (if (find slot-name order)
                            '(:inline :reader :writer :accessor)
                            '(:inline))))
         (cons slot-name (apply #'u:plist-remove slot-options to-remove)))))
   slots))

(defun generate-class-options (order options)
  `((:metaclass ordered-class)
    (:order ,@order)
    ,@(remove-if (lambda (x) (member x '(:metaclass :order))) options :key #'car)))

(defmacro define-ordered-class (name super-classes &body (slots . options))
  (let ((order (cadr (find :order options :key #'car))))
    `(progn
       (defclass ,name ,super-classes
         ,(generate-slot-specifiers slots order)
         ,@(generate-class-options order options))
       ,@(generate-fast-accessors name slots order))))
