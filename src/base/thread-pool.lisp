(in-package #:cl-user)

(defpackage #:%zed.base.thread-pool
  ;; Third-party aliases
  (:local-nicknames
   (#:lp #:lparallel)
   (#:lpq #:lparallel.queue)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.base.thread-pool)

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
  (let ((worker-count (cl-cpus:get-number-of-processors)))
    (%make-thread-pool :worker-count worker-count)))

(defmacro with-thread-pool (thread-pool &body body)
  `(let ((lp:*kernel* (lp:make-kernel (worker-count ,thread-pool) :name "Zed")))
     (unwind-protect (progn ,@body)
       (lp:end-kernel :wait t))))

(defun enqueue (thread-pool func)
  (lpq:push-queue func (queue thread-pool)))

(defun process-queue (context thread-pool)
  (let ((queue (queue thread-pool)))
    (u:until (lpq:queue-empty-p queue)
      (funcall (lpq:pop-queue queue) context))))
