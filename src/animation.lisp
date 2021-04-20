(in-package :zed)

(defstruct (animation
            (:constructor %make-animation)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (elapsed 0.0 :type u:f32)
  (duration 1.0 :type u:f32)
  (blocking-p nil :type boolean)
  (self-finishing-p nil :type boolean)
  (shape #'easing:linear :type function)
  (data (u:dict #'equalp) :type hash-table))

(defun register-animation (sequence animation &key where target)
  (let ((target (or target (util.dll:tail sequence))))
    (util.dll:insert sequence animation :where where :target target)))

(defun deregister-animation (sequence animation)
  (util.dll:delete sequence animation))

(defgeneric on-animate-start (game-object name data)
  (:method (game-object name data)))

(defgeneric on-animate-update (game-object name data)
  (:method (game-object name data)))

(defgeneric on-animate-finish (game-object name data)
  (:method (game-object name data)))

(defun step-animation (game-object clock sequence animation)
  (let ((name (animation-name animation))
        (duration (animation-duration animation))
        (elapsed (animation-elapsed animation))
        (self-finishing-p (animation-self-finishing-p animation))
        (data (animation-data animation)))
    (cond
      ((zerop elapsed)
       (setf (u:href data :previous-step) 0.0
             (u:href data :time) 0.0)
       (on-animate-start game-object name data))
      ((or (and self-finishing-p (u:href data :finished))
           (and (not self-finishing-p)
                (>= elapsed duration)))
       (on-animate-finish game-object name data)
       (deregister-animation sequence animation)))
    (let ((step (funcall (animation-shape animation)
                         (if (zerop duration)
                             0.0
                             (u:clamp (/ elapsed duration) 0.0 1.0)))))
      (incf (animation-elapsed animation) (get-frame-time clock))
      (setf (u:href data :step) step)
      (on-animate-update game-object name data)
      (setf (u:href data :previous-step) step
            (u:href data :time) (animation-elapsed animation)))))

(defun process-animations (game-object clock sequence)
  (loop :with sequence = (util.dll:list-values sequence)
        :for animation :in sequence
        :do (step-animation game-object clock sequence animation)
        :when (animation-blocking-p animation)
          :do (return)))

(defun enqueue-animation (sequence name
                          &key blocking-p self-finishing-p (duration 1.0) (shape #'easing:linear)
                            (where :after) target)
  (let ((animation (%make-animation :name name
                                    :duration (float duration 1f0)
                                    :blocking-p blocking-p
                                    :self-finishing-p self-finishing-p
                                    :shape shape)))
    (register-animation sequence animation :where where :target target)
    animation))

(defmacro define-animate-hook (hook (game-object name data) &body body)
  (let ((method (u:format-symbol :zed "ON-ANIMATE-~a" hook)))
    `(defmethod ,method (,game-object (name (eql ',name)) ,data)
       ,@body)))
