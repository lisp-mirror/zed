(in-package #:cl-user)

(defpackage #:%zed.base.clock
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.base.clock)

(defstruct (clock
            (:constructor %make-clock)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
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

(defun make-clock (delta-time)
  (%make-clock :init-time (get-internal-real-time)
               :delta-time (float delta-time 1f0)))

(declaim (inline get-time))
(defun get-time (clock)
  (* (- (get-internal-real-time) (init-time clock))
     #.(/ (float internal-time-units-per-second 1d0))))

;; Perform delta-time smoothing, as per https://frankforce.com/frame-rate-delta-buffering/
(u:fn-> smooth-delta-time (clock u:ub8) null)
(defun smooth-delta-time (clock refresh-rate)
  (incf (frame-time clock) (delta-buffer clock))
  (let* ((frame-time (frame-time clock))
         (frame-count (max 1 (truncate (1+ (* frame-time refresh-rate)))))
         (previous frame-time))
    (setf (frame-time clock) (* frame-count (/ 1d0 refresh-rate))
          (delta-buffer clock) (- previous (frame-time clock)))
    nil))

(defun calculate-frame-rate (clock)
  (declare (optimize speed))
  (let* ((time (frame-time clock))
         (fps (/ 1d0 time))
         (alpha10 (- 1 (exp (* time #.(/ -10d0)))))
         (alpha30 (- 1 (exp (* time #.(/ -30d0)))))
         (alpha60 (- 1 (exp (* time #.(/ -60d0)))))
         (frame-count (frame-count clock)))
    (setf (fps/current clock) fps)
    (if (> (running-time clock) 3)
        (setf (fps/average/10s clock) (+ (* alpha10 fps) (* (- 1 alpha10) (fps/average/10s clock)))
              (fps/average/30s clock) (+ (* alpha30 fps) (* (- 1 alpha30) (fps/average/30s clock)))
              (fps/average/60s clock) (+ (* alpha60 fps) (* (- 1 alpha60) (fps/average/60s clock)))
              (fps/average clock) (/ (+ fps (* (1- frame-count) (fps/average clock))) frame-count))
        (setf (fps/average/10s clock) fps
              (fps/average/30s clock) fps
              (fps/average/60s clock) fps
              (fps/average clock) fps))
    nil))

;; Advanced the physics simulation forward: https://gafferongames.com/post/fix_your_timestep/
(u:fn-> advance-physics (clock function) null)
(declaim (inline advance-physics))
(defun advance-physics (clock func)
  (declare (optimize speed))
  (let ((delta-time (delta-time clock)))
    (incf (accumulator clock) (frame-time clock))
    (when (zerop (frame-count clock))
      (funcall func))
    (u:while (>= (accumulator clock) delta-time)
      (funcall func)
      (decf (accumulator clock) delta-time))
    (setf (alpha clock) (float (/ (accumulator clock) delta-time) 1f0))
    nil))

(u:fn-> tick (clock u:ub8 function function) (values))
(defun tick (clock refresh-rate update-func periodic-func)
  (declare (optimize speed))
  (let* ((pause (pause-time clock))
         (previous (+ (running-time clock) pause))
         (current (- (get-time clock) pause)))
    (setf (previous-time clock) previous
          (running-time clock) current
          (pause-time clock) 0d0)
    ;; Calculate the difference in time from the last tick. The first frame always has a difference
    ;; of delta-time to prevent interpolation issues when first rendering the scene.
    (if (zerop (frame-count clock))
        (setf (frame-time clock) (float (delta-time clock) 1d0))
        (setf (frame-time clock) (- current previous)))
    ;; Smooth delta time
    (smooth-delta-time clock refresh-rate)
    ;; Advance N physics steps according to how much time is in the accumulator.
    (advance-physics clock update-func)
    ;; Run the periodic updates if it is time to do so.
    (when (>= (- current (period-elapsed clock)) (period-interval clock))
      (funcall periodic-func)
      (setf (period-elapsed clock) current))
    ;; Calculate the frame rate on every frame except the first.
    (when (plusp (frame-count clock))
      (calculate-frame-rate clock))
    (values)))

;; Increment the number of frames that have been drawn. This is always the very last thing that
;; occurs each frame, directly after swapping the front and back buffers.
(u:fn-> count-frame (clock) null)
(declaim (inline count-frame))
(defun count-frame (clock)
  (declare (optimize speed))
  (incf (frame-count clock))
  nil)
