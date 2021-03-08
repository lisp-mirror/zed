(in-package #:cl-user)

(defpackage #:%zed.base.thread-pool
  (:local-nicknames
   (#:lp #:lparallel))
  (:use #:cl))

(in-package #:%zed.base.thread-pool)

(defun get-worker-count ()
  (cl-cpus:get-number-of-processors))

(defmacro with-thread-pool (&body body)
  `(let ((lp:*kernel* (lp:make-kernel (get-worker-count) :name "Zed")))
     (unwind-protect (progn ,@body)
       (lp:end-kernel :wait t))))
