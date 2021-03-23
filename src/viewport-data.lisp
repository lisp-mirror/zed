(in-package #:cl-user)

(defpackage #:%zed.viewport.data
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:tp #:%zed.thread-pool))
  (:use #:cl)
  (:shadow
   #:find))

(in-package #:%zed.viewport.data)

(glob:define-global-var =data= (u:dict #'eq))

(defstruct (data
            (:constructor %make-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (x 0f0 :type (u:f32 0.0 1.0))
  (y 0f0 :type (u:f32 0.0 1.0))
  (width 0f0 :type (u:f32 0.0 1.0))
  (height 0f0 :type (u:f32 0.0 1.0)))

(u:define-printer (data stream :type nil)
  (format stream "VIEWPORT-DATA: ~s" (name data)))

(defun find (name)
  (or (u:href =data= name)
      (error "Viewport ~s is not defined." name)))

(defun update (name x y width height)
  (let ((data (u:href =data= name)))
    (setf (x data) (u:clamp (float x 1f0) 0.0 1.0)
          (y data) (u:clamp (float y 1f0) 0.0 1.0)
          (width data) (u:clamp (float width 1f0) 0.0 1.0)
          (height data) (u:clamp (float height 1f0) 0.0 1.0))
    (tp::enqueue (list :viewport name))))

(defun make-data (name x y width height)
  (let ((data (%make-data :name name)))
    (setf (u:href =data= name) data)
    (update name x y width height)
    data))

(defmacro define-viewport (name () &body body)
  (destructuring-bind (&key (x 0) (y 0) (width 1) (height 1)) (car body)
    `(if (u:href =data= ',name)
         (update ',name ,x ,y ,width ,height)
         (make-data ',name ,x ,y ,width ,height))))

(define-viewport :default ()
  (:x 0 :y 0 :width 1 :height 1))
