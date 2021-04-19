(in-package #:zed)

;; Create a function that is called every clock tick to update the transform state of each game
;; object.
(u:fn-> make-physics-phase-function (core) function)
(defun make-physics-phase-function (core)
  (declare (optimize speed))
  (let* ((scene-tree (core-scene-tree core))
         (clock (core-clock core))
         (delta-time (/ (float (clock-delta-time clock) 1f0) (clock-units-per-second clock))))
    (lambda ()
      (declare (optimize speed))
      (with-time-buffer (core :physics-phase)
        (with-scope (:physics-phase)
          (invoke-trait-hook core :physics)
          (walk-game-object-tree (game-object scene-tree)
            (transform-game-object game-object delta-time))
          (compute-collisions core))))))

(u:fn-> run-update-phase (core) null)
(defun run-update-phase (core)
  (declare (optimize speed))
  (with-time-buffer (core :update-phase)
    (with-scope (:update-phase)
      (let ((scene-tree (core-scene-tree core))
            (clock (core-clock core)))
        (activate-traits core)
        (deactivate-traits core)
        (invoke-trait-hook core :update)
        (walk-game-object-tree (game-object scene-tree)
          (resolve-world-matrix game-object (clock-interpolation-factor clock)))
        nil))))

(u:fn-> start-game-loop (core &key (:profile-p boolean) (:frame-count (or fixnum null))) null)
(defun start-game-loop (core &key profile-p frame-count)
  (declare (optimize speed))
  ;; Hold on to some variables before we enter the main game loop, since they are needed each
  ;; iteration and won't change. This way we won't have to look them up in a busy loop.
  (let* ((clock (core-clock core))
         (window (core-window core))
         (input-manager (core-input-manager core))
         (viewport-manager (core-viewports core))
         (physics-phase-func (make-physics-phase-function core)))
    ;; Request the Lisp implementation to perform a full garbage collection immediately before we
    ;; start the main game loop, to mitigate any large amounts of data from initialization or the
    ;; last run from being cleaned up at runtime causing frame drops.
    (tg:gc :full t)
    ;; Profile the game loop if the user requested to do so.
    (v:debug :zed "Entered game loop")
    (with-profiling (profile-p)
      ;; Actually start the main game loop.
      (u:while (core-running-p core)
        (with-continuable (clock)
          (handle-input-events input-manager window viewport-manager)
          ;; Perform one clock tick.
          (tick-clock clock physics-phase-func)
          ;; Perform update logic that needs to occur each frame.
          (run-update-phase core)
          ;; Draw all game objects with a render trait attached.
          (zed.trait.render::render-frame core)
          ;; Draw this frame to the window.
          (draw-window window)
          ;; Increment the frame counter at the end of the frame.
          (count-clock-frame clock)
          ;; In debug mode only, quit the game after the user-supplied frame count (useful for
          ;; profiling).
          #-zed.release
          (when (and frame-count (>= (clock-frame-count clock) frame-count))
            (shutdown-core core)))))))
