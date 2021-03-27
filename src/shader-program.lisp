(in-package #:cl-user)

(defpackage #:%zed.shader-program
  ;; Third-party aliases
  (:local-nicknames
   (#:log #:verbose)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:live #:%zed.live-coding)
   (#:tp #:%zed.thread-pool))
  (:use #:cl))

(in-package #:%zed.shader-program)

(defun register-shaders ()
  (let ((table (shadow:load-shaders (lambda (x) (tp::enqueue (list :shader x))))))
    (log:info :zed.shader-program "Loaded ~d shader programs" (hash-table-count table))
    table))
