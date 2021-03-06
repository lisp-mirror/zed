(in-package #:%zed.base)

;;; TODO: Add profiling code here

(defmacro debug-check (&body body)
  (unless (member :zed.release *features*)
    `(progn
       ,@(mapcar
          (lambda (x)
            `(unless ,x
               (error "Debug check failed: ~s" ',x)))
          body))))
