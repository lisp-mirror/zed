(in-package #:zed)

(defstruct (clock
            (:constructor %make-clock)
            (:predicate nil)
            (:copier nil))
  (vsync-p nil :type boolean)
  (accumulator 0d0 :type u:f64)
  (delta-buffer 0d0 :type u:f64)
  (fps/current 0d0 :type u:f64)
  (fps/average 0d0 :type u:f64)
  (fps/average/10s 0d0 :type u:f64)
  (fps/average/30s 0d0 :type u:f64)
  (fps/average/60s 0d0 :type u:f64)
  (frame-count 0 :type fixnum)
  (frame-time 0d0 :type u:f64)
  (init-time 0 :type fixnum)
  (delta-time 0f0 :type u:f32)
  (alpha 0f0 :type u:f32)
  (period-elapsed 0d0 :type u:f64)
  (period-interval 0.25d0 :type u:f64)
  (previous-time 0d0 :type u:f64)
  (running-time 0d0 :type u:f64)
  (pause-time 0d0 :type u:f64))

(u:define-printer (clock stream :type nil)
  (format stream "CLOCK: ~,1fs, ~,1f fps"
          (clock-running-time clock)
          (clock-fps/current clock)))

(defun make-clock (config refresh-rate vsync-p)
  (let ((delta-time (float (or (config-delta-time config) (/ refresh-rate)) 1f0)))
    (prog1 (%make-clock :init-time (get-internal-real-time)
                        :delta-time delta-time
                        :vsync-p vsync-p)
      (v:debug :zed "Initialized game clock: delta: ~,3f ms/frame" (* delta-time 1000f0)))))

(declaim (inline get-clock-time))
(defun get-clock-time (clock)
  (* (- (get-internal-real-time) (clock-init-time clock))
     #.(/ (float internal-time-units-per-second 1d0))))

;; Adjust the clock's pause time when live coding interferes with clock's running time. This is
;; called when exiting the debugger, or after evaluating REPL forms, in order to subtract any time
;; spent in the debugger or evaluating REPL functions from the running time of the game clock.
(u:fn-> adjust-clock-pause-time (clock u:f64) u:f32)
(declaim (inline adjust-clock-pause-time))
(defun adjust-clock-pause-time (clock time)
  (declare (optimize speed))
  (let ((adjusted (- (get-clock-time clock) time)))
    (setf (clock-pause-time clock) adjusted)
    (float adjusted 1f0)))

;; Perform delta-time smoothing, as per https://frankforce.com/frame-rate-delta-buffering/
(u:fn-> smooth-delta-time (clock u:ub8) null)
(defun smooth-delta-time (clock refresh-rate)
  (incf (clock-frame-time clock) (clock-delta-buffer clock))
  (let* ((frame-time (clock-frame-time clock))
         (frame-count (max 1 (truncate (1+ (* frame-time refresh-rate)))))
         (previous frame-time))
    (setf (clock-frame-time clock) (* frame-count (/ 1d0 refresh-rate))
          (clock-delta-buffer clock) (- previous (clock-frame-time clock)))
    nil))

(defun calculate-frame-rate (clock)
  (declare (optimize speed))
  (let* ((time (clock-frame-time clock))
         (fps (if (zerop time) 0d0 (/ 1d0 time)))
         (alpha10 (- 1 (exp (* time #.(/ -10d0)))))
         (alpha30 (- 1 (exp (* time #.(/ -30d0)))))
         (alpha60 (- 1 (exp (* time #.(/ -60d0)))))
         (frame-count (clock-frame-count clock)))
    (setf (clock-fps/current clock) fps)
    (if (> (clock-running-time clock) 3)
        (setf (clock-fps/average/10s clock) (+ (* alpha10 fps)
                                               (* (- 1 alpha10) (clock-fps/average/10s clock)))
              (clock-fps/average/30s clock) (+ (* alpha30 fps)
                                               (* (- 1 alpha30) (clock-fps/average/30s clock)))
              (clock-fps/average/60s clock) (+ (* alpha60 fps)
                                               (* (- 1 alpha60) (clock-fps/average/60s clock)))
              (clock-fps/average clock) (/ (+ fps (* (1- frame-count) (clock-fps/average clock)))
                                           frame-count))
        (setf (clock-fps/average/10s clock) fps
              (clock-fps/average/30s clock) fps
              (clock-fps/average/60s clock) fps
              (clock-fps/average clock) fps))
    nil))

;; Advance the physics simulation forward: https://gafferongames.com/post/fix_your_timestep/
(u:fn-> advance-clock-physics (clock function) null)
(declaim (inline advance-clock-physics))
(defun advance-clock-physics (clock func)
  (declare (optimize speed))
  (let ((delta-time (clock-delta-time clock)))
    (incf (clock-accumulator clock) (clock-frame-time clock))
    (when (zerop (clock-frame-count clock))
      (funcall func))
    (u:while (>= (clock-accumulator clock) delta-time)
      (funcall func)
      (decf (clock-accumulator clock) delta-time))
    (setf (clock-alpha clock) (float (/ (clock-accumulator clock) delta-time) 1f0))
    nil))

(u:fn-> tick-clock (clock u:ub8 function function) (values))
(defun tick-clock (clock refresh-rate update-func periodic-func)
  (declare (optimize speed)
           (ignorable periodic-func))
  (let* ((pause (clock-pause-time clock))
         (previous (+ (clock-running-time clock) pause))
         (current (- (get-clock-time clock) pause)))
    (setf (clock-previous-time clock) previous
          (clock-running-time clock) current
          (clock-pause-time clock) 0d0)
    ;; Calculate the difference in time from the last tick. The first frame always has a difference
    ;; of delta-time to prevent interpolation issues when first rendering the scene.
    (if (zerop (clock-frame-count clock))
        (setf (clock-frame-time clock) (float (clock-delta-time clock) 1d0))
        (setf (clock-frame-time clock) (- current previous)))
    ;; Smooth delta time
    (when (clock-vsync-p clock)
      (smooth-delta-time clock refresh-rate))
    ;; Advance N physics steps according to how much time is in the accumulator.
    (advance-clock-physics clock update-func)
    ;; Run the periodic updates if it is time to do so, but only during debug mode.
    #-zed.release
    (when (>= (- current (clock-period-elapsed clock)) (clock-period-interval clock))
      (funcall periodic-func)
      (setf (clock-period-elapsed clock) current))
    ;; Calculate the frame rate on every frame except the first.
    (when (plusp (clock-frame-count clock))
      (calculate-frame-rate clock))
    (values)))

;; Increment the number of frames that have been drawn. This is always the very last thing that
;; occurs each frame, directly after swapping the front and back buffers.
(u:fn-> count-clock-frame (clock) null)
(declaim (inline count-clock-frame))
(defun count-clock-frame (clock)
  (declare (optimize speed))
  (incf (clock-frame-count clock))
  nil)
