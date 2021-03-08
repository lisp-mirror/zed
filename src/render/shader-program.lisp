(in-package #:cl-user)

(defpackage #:%zed.render.shader-program
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:tp #:%zed.base.thread-pool))
  (:use #:cl))

(in-package #:%zed.render.shader-program)

(defun make-shader-modify-hook (program-names)
  (lambda (context)
    (declare (ignore context))
    (shadow:recompile-shaders program-names)
    (dolist (x program-names)
      (format t "Recompiled shader: ~s.~%" x))))

(defun register-shaders (thread-pool)
  (shadow:load-shaders
   (lambda (x)
     (tp::enqueue thread-pool (make-shader-modify-hook x)))))
