(in-package #:zed)

(defstruct (curve-data
            (:constructor %make-curve-data)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (points nil :type list))

(glob:define-global-var =curves= (u:dict #'eq))

(defun update-curve-data (name points)
  (let ((spec (u:href =curves= name)))
    (setf (curve-data-points spec) points)))

(defun make-curve-data (name points)
  (let ((spec (%make-curve-data :name name)))
    (setf (u:href =curves= name) spec)
    (update-curve-data name points)
    spec))

(defmacro define-curve (name options &body body)
  (declare (ignore options))
  (let ((points `(list ,@(mapcar (lambda (x) `(v3:vec ,@x)) body))))
    `(progn
       (unless (curve:point-count-valid-p (length ,points))
         (error "Point count invalid for curve ~a." ',name))
       (if (u:href =curves= ',name)
           (update-curve-data ',name ,points)
           (make-curve-data ',name ,points)))))
