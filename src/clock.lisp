(in-package #:zed)

(defstruct (clock
            (:constructor %make-clock)
            (:predicate nil)
            (:copier nil))
  ;; A flag specifying if the engine has been started with vertical refresh rate synced. If true,
  ;; each tick of the clock we perform delta-time smoothing.
  (vsync-p nil :type boolean)
  ;; The number of time units per second. This is platform-specific, provided by
  ;; SDL_GetPerformanceFrequency. On Linux, this is typically 1e9, the timer having a resolution in
  ;; nanoseconds.
  (units-per-second 0 :type u:non-negative-fixnum)
  ;; Absolute timestamp of when the clock was first initialized.
  (init-time 0 :type (u:positive-fixnum))
  ;; The Gaffer "Fix Your Timestep" accumulation buffer.
  (accumulator 0 :type u:non-negative-fixnum)
  ;; The number of units that have elapsed since the clock was initialized. units-per-second /
  ;; elapsed-time gives the number of seconds the engine has be running.
  (elapsed-time 0 :type u:non-negative-fixnum)
  ;; The number of units of time it took to render the last frame.
  (frame-time 0 :type fixnum)
  ;; The number of frames that have been rendered.
  (frame-count 0 :type (u:non-negative-fixnum))
  ;; The smoothing buffer used for time delta smoothing.
  (smoothing-buffer 0 :type fixnum)
  ;; The frequency physics simulations run at, in time units. delta-time / units-per-second gives
  ;; the physics update rate in seconds.
  (delta-time 0 :type (u:positive-fixnum))
  ;; The interpolation factor for game object transformations.
  (interpolation-factor 0f0 :type u:f32)
  ;; The number of units of time that the debugger has been open. This is reset to 0 when the
  ;; debugger is closed. This value is used to reverse the game clock when the dbugger is closed, so
  ;; that physics do not advance far into the future upon the simulation continuing.
  (debug-time 0 :type u:non-negative-fixnum)
  ;; The number of units of time that has elapsed since the last periodic update. Periodic updates
  ;; occur every 0.25 seconds, and only when not running in release mode. The purpose of periodic
  ;; updates is to process the thread-safe recompilation queue to apply any live recompilations that
  ;; have been invoked by the game developer.
  (period-elapsed 0 :type u:non-negative-fixnum)
  ;; The monitor refresh rate stored in time units. units-per-second / refresh-rate gives the number
  ;; of cycles per second.
  (refresh-rate 0 :type (u:non-negative-fixnum))
  ;; The current frame rate in frames per second.
  (fps/current 0.0 :type u:f32)
  ;; The overall average frame rate.
  (fps/average 0.0 :type u:f32)
  ;; The weighted average frame rate over the last 10 seconds.
  (fps/average/10s 0.0 :type u:f32)
  ;; The weighted average frame rate over the last 30 seconds.
  (fps/average/30s 0.0 :type u:f32)
  ;; The weighted average frame rate over the last 60 seconds.
  (fps/average/60s 0.0 :type u:f32))

(u:define-printer (clock stream :type nil)
  (format stream "CLOCK: ~,1fs, ~,1f fps"
          (/ (clock-elapsed-time clock) (clock-units-per-second clock))
          (clock-fps/current clock)))

(defun make-clock (config refresh-rate vsync-p)
  (let* ((units-per-second (sdl2:get-performance-frequency))
         (delta-time (round (* (or (config-delta-time config) (/ refresh-rate)) units-per-second)))
         (refresh-rate (round units-per-second refresh-rate)))
    (prog1 (%make-clock :init-time (sdl2:get-performance-counter)
                        :units-per-second units-per-second
                        :refresh-rate refresh-rate
                        :delta-time delta-time
                        :vsync-p vsync-p)
      (v:debug :zed "Initialized game clock: delta: ~,3f ms/frame"
               (/ (* delta-time 1000) units-per-second)))))

(u:fn-> get-clock-time (clock) (u:positive-fixnum))
(declaim (inline get-clock-time))
(defun get-clock-time (clock)
  (declare (optimize speed))
  (- (the fixnum (sdl2:get-performance-counter)) (clock-init-time clock)))

;; Adjust the clock's debug time when live coding interferes with clock's running time. This is
;; called when exiting the debugger in order to subtract any time spent in the debugger from the
;; running time of the game clock.
(u:fn-> adjust-clock-debug-time (clock u:positive-fixnum) u:f32)
(declaim (inline adjust-clock-debug-time))
(defun adjust-clock-debug-time (clock time)
  (declare (optimize speed))
  (let ((adjusted (- (get-clock-time clock) time)))
    (setf (clock-debug-time clock) adjusted)
    (/ (float adjusted 1f0) (clock-units-per-second clock))))

;; Perform delta-time smoothing, as per https://frankforce.com/frame-rate-delta-buffering/
(u:fn-> smooth-delta-time (clock) null)
(defun smooth-delta-time (clock)
  (let* ((refresh-rate (clock-refresh-rate clock))
         (frame-time (+ (clock-frame-time clock) (clock-smoothing-buffer clock)))
         (cycles (max 1 (truncate (1+ (* frame-time (/ refresh-rate)))))))
    (setf (clock-frame-time clock) (* cycles refresh-rate)
          (clock-smoothing-buffer clock) (- frame-time (clock-frame-time clock)))
    nil))

(defun calculate-frame-rate (clock)
  (declare (optimize speed))
  (let* ((time (/ (float (clock-frame-time clock) 1f0)
                  (clock-units-per-second clock)))
         (fps (if (zerop time) 0.0 (/ time)))
         (alpha10 (- 1 (exp (* time #.(/ -10.0)))))
         (alpha30 (- 1 (exp (* time #.(/ -30.0)))))
         (alpha60 (- 1 (exp (* time #.(/ -60.0)))))
         (frame-count (clock-frame-count clock)))
    (setf (clock-fps/current clock) fps)
    (if (> (clock-elapsed-time clock)
           (* (clock-units-per-second clock) 3))
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
    (setf (clock-interpolation-factor clock) (/ (float (clock-accumulator clock) 1f0) delta-time))
    nil))

(u:fn-> tick-clock (clock function) (values))
(defun tick-clock (clock func)
  (declare (optimize speed))
  (let* ((debug (clock-debug-time clock))
         (previous (+ (clock-elapsed-time clock) debug))
         (current (- (get-clock-time clock) debug)))
    (setf (clock-elapsed-time clock) current
          (clock-debug-time clock) 0)
    ;; Calculate the difference in time from the last tick. The first frame always has a difference
    ;; of delta-time to prevent interpolation issues when first rendering the scene.
    (if (zerop (clock-frame-count clock))
        (setf (clock-frame-time clock) (clock-delta-time clock))
        (setf (clock-frame-time clock) (- current previous)))
    ;; Smooth delta time
    (when (clock-vsync-p clock)
      (smooth-delta-time clock))
    ;; Advance N physics steps according to how much time is in the accumulator.
    (advance-clock-physics clock func)
    ;; Run the periodic updates if it is time to do so, but only during debug mode.
    #-zed.release
    (when (>= (- current (clock-period-elapsed clock))
              (clock-units-per-second clock))
      (process-thread-pool-queue #'recompile)
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

(u:fn-> get-frame-time (clock) u:f32)
(defun get-frame-time (clock)
  (declare (optimize speed))
  (/ (float (clock-frame-time clock) 1f0) (clock-units-per-second clock)))
