(in-package #:cl-user)

(defpackage #:%zed.shader-program
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:tp #:%zed.thread-pool))
  (:use #:cl))

(in-package #:%zed.shader-program)

(defun make-shader-modify-hook (program-names)
  (lambda ()
    (shadow:recompile-shaders program-names)
    (dolist (x program-names)
      (format t "Recompiled shader: ~s.~%" x))))

(defun register-shaders (thread-pool)
  (shadow:load-shaders
   (lambda (x)
     (tp::enqueue thread-pool (make-shader-modify-hook x)))))
