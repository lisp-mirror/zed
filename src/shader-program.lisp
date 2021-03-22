(in-package #:cl-user)

(defpackage #:%zed.shader-program
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:live #:%zed.live-coding)
   (#:tp #:%zed.thread-pool))
  (:use #:cl))

(in-package #:%zed.shader-program)

(defun register-shaders (thread-pool)
  (shadow:load-shaders
   (lambda (x)
     (tp::enqueue thread-pool (list :shader x)))))

(defmethod live::recompile ((type (eql :shader)) data)
  (shadow:recompile-shaders data)
  (dolist (x data)
    (format t "Recompiled shader: ~s.~%" x)))
