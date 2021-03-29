(in-package #:cl-user)

(defpackage #:%zed.thread-pool
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:log #:%zed.logging)
   (#:lp #:lparallel)
   (#:lpq #:lparallel.queue)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.thread-pool)

(glob:define-global-var =thread-pool= nil)

(defstruct (thread-pool
            (:constructor %make-thread-pool)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (worker-count 1 :type u:ub8)
  (queue (lpq:make-queue) :type lpq:queue))

(u:define-printer (thread-pool stream :type nil)
  (format stream "THREAD-POOL: ~d workers" (worker-count thread-pool)))

(defun make-thread-pool ()
  (let* ((worker-count (cl-cpus:get-number-of-processors))
         (thread-pool (%make-thread-pool :worker-count worker-count)))
    (log::debug :zed.thread-pool "Initialized thread-pool with ~d workers" worker-count)
    thread-pool))

(defmacro with-thread-pool (() &body body)
  `(progn
     (setf =thread-pool= (make-thread-pool))
     (let ((lp:*kernel* (lp:make-kernel (worker-count =thread-pool=) :name "Zed")))
       (unwind-protect (progn ,@body)
         (setf =thread-pool= nil)
         (lp:end-kernel :wait t)))))

(u:fn-> enqueue (list) null)
(defun enqueue (data)
  (declare (optimize speed))
  (when =thread-pool=
    (lpq:push-queue data (queue =thread-pool=)))
  nil)

(u:fn-> process-queue (function) null)
(defun process-queue (func)
  (declare (optimize speed))
  (when =thread-pool=
    (let ((queue (queue =thread-pool=)))
      (u:until (lpq:queue-empty-p queue)
        (destructuring-bind (type data) (lpq:pop-queue queue)
          (funcall func type data))))))
