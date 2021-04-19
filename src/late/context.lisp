(in-package #:zed)

(glob:define-global-var =data= (u:dict #'eq))

(defclass context ()
  ((%config :reader config
            :initarg :config
            :initform nil)))

(defmethod initialize-instance :after ((instance context) &key)
  (let ((config (apply #'make-config (config instance))))
    (v:info :zed "Started ~a" (config-window-title config))
    (with-core core (config)
      (start-game-loop core
                       :profile-p (config-profile-p config)
                       :frame-count (config-frame-count config)))))

(defun find-context (name)
  (or (u:href =data= name)
      (error "Context ~s is not defined." name)))

(defmacro define-context (name () &body body)
  (u:with-gensyms (class-name)
    `(u:eval-always
       (defclass ,class-name (context) ()
         (:default-initargs
          :config (list ,@(car body))))
       (setf (u:href =data= ',name) (find-class ',class-name)))))
