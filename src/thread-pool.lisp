(in-package #:zed)

(defstruct (thread-pool
            (:constructor %make-thread-pool)
            (:predicate nil)
            (:copier nil))
  (worker-count 1 :type u:ub8)
  (queue (lpq:make-queue) :type lpq:queue))

(u:define-printer (thread-pool stream :type nil)
  (format stream "THREAD-POOL: ~d workers" (thread-pool-worker-count thread-pool)))

(glob:define-global-var =thread-pool= nil)

(defun make-thread-pool ()
  (let* ((worker-count (cl-cpus:get-number-of-processors))
         (thread-pool (%make-thread-pool :worker-count worker-count)))
    (v:debug :zed "Initialized thread-pool with ~d workers" worker-count)
    thread-pool))

(defmacro with-thread-pool (() &body body)
  `(progn
     (setf =thread-pool= (make-thread-pool))
     (let ((lp:*kernel* (lp:make-kernel (thread-pool-worker-count =thread-pool=) :name "Zed")))
       (unwind-protect (progn ,@body)
         (setf =thread-pool= nil)
         (lp:end-kernel :wait t)))))

(u:fn-> thread-pool-enqueue (list) null)
(defun thread-pool-enqueue (data)
  (declare (optimize speed))
  (when =thread-pool=
    (lpq:push-queue data (thread-pool-queue =thread-pool=)))
  nil)

(u:fn-> process-thread-pool-queue (function) null)
(defun process-thread-pool-queue (func)
  (declare (optimize speed))
  (when =thread-pool=
    (let ((queue (thread-pool-queue =thread-pool=)))
      (u:until (lpq:queue-empty-p queue)
        (destructuring-bind (type data) (lpq:pop-queue queue)
          (funcall func type data))))))
