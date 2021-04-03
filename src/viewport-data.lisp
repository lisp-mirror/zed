(in-package #:zed)

(defstruct (viewport-data
            (:constructor %make-viewport-data)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (x 0f0 :type (u:f32 0.0 1.0))
  (y 0f0 :type (u:f32 0.0 1.0))
  (width 0f0 :type (u:f32 0.0 1.0))
  (height 0f0 :type (u:f32 0.0 1.0)))

(u:define-printer (viewport-data stream :type nil)
  (format stream "VIEWPORT-DATA: ~s" (viewport-data-name viewport-data)))

(glob:define-global-var =viewports= (u:dict #'eq))

(defun find-viewport-data (name)
  (or (u:href =viewports= name)
      (error "Viewport ~s is not defined." name)))

(defun update-viewport-data (name x y width height)
  (let ((data (u:href =viewports= name)))
    (setf (viewport-data-x data) (u:clamp (float x 1f0) 0.0 1.0)
          (viewport-data-y data) (u:clamp (float y 1f0) 0.0 1.0)
          (viewport-data-width data) (u:clamp (float width 1f0) 0.0 1.0)
          (viewport-data-height data) (u:clamp (float height 1f0) 0.0 1.0))
    (thread-pool-enqueue (list :viewport name))))

(defun make-viewport-data (name x y width height)
  (let ((data (%make-viewport-data :name name)))
    (setf (u:href =viewports= name) data)
    (update-viewport-data name x y width height)
    data))

(defmacro define-viewport (name () &body body)
  (destructuring-bind (&key (x 0) (y 0) (width 1) (height 1)) (car body)
    `(if (u:href =viewports= ',name)
         (update-viewport-data ',name ,x ,y ,width ,height)
         (make-viewport-data ',name ,x ,y ,width ,height))))

(define-viewport :default ()
  (:x 0 :y 0 :width 1 :height 1))
