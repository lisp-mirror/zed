(in-package #:zed)

(defstruct (time-buffer
            (:constructor %make-time-buffer)
            (:predicate nil)
            (:copier nil))
  (data nil :type u:fixnum-array)
  (pointer 0 :type fixnum))

(glob:define-global-var =time-buffers= (u:dict #'eq))

(defun make-time-buffer (size)
  (%make-time-buffer :data (u:make-fixnum-array size)))

(defun insert-time-buffer (buffer time)
  (let ((pointer (time-buffer-pointer buffer))
        (data (time-buffer-data buffer)))
    (setf (time-buffer-pointer buffer) (mod (1+ pointer) (length data))
          (aref data pointer) time)
    buffer))

(defmacro with-time-buffer ((core buffer-name) &body body)
  (u:with-gensyms (clock buffer start-time)
    (if (member :zed.release *features*)
        `(progn ,@body)
        `(let* ((,clock (core-clock ,core))
                (,start-time (get-clock-time ,clock)))
           (unless (u:href =time-buffers= ',buffer-name)
             (setf (u:href =time-buffers= ',buffer-name) (make-time-buffer 1000)))
           (prog1 (progn ,@body)
             (let ((,buffer (u:href =time-buffers= ',buffer-name)))
               (insert-time-buffer ,buffer (- (get-clock-time ,clock) ,start-time))))))))

(defun print-time-buffers ()
  (let* ((clock (core-clock =core=))
         (ups (clock-units-per-second clock))
         (refresh-time-units (clock-refresh-time-units clock))
         (refresh-time-ms (* 1000 (/ refresh-time-units ups)))
         (refresh-rate (/ 1 (/ ups refresh-time-units) 1f0))
         (delta-time (/ (clock-delta-time clock) ups 1f0)))
    (format t "Refresh rate: ~,3f ms/f~%~%" refresh-time-ms)
    (u:do-hash (k v =time-buffers=)
      (let* ((data (time-buffer-data v))
             (seconds (/ (reduce #'+ data) ups)))
        (cond
          ((zerop (aref data (1- (length data))))
           (format t "~a: ?~%" k))
          ((eq k :physics-phase)
           (format t "~a: ~,3f ms/tick~%" k (* (/ refresh-rate delta-time) seconds)))
          (t
           (format t "~a: ~,3f ms/tick~%" k seconds)))))))
