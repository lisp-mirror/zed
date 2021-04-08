(in-package #:zed)

;; Create a function that is called every clock tick to update the transform state of each game
;; object.
(u:fn-> make-physics-phase-function (context) function)
(defun make-physics-phase-function (context)
  (let ((scene-tree (context-scene-tree context))
        (delta-time (clock-delta-time (context-clock context))))
    (lambda ()
      (declare (optimize speed))
      (with-scope (:physics-phase)
        (invoke-trait-hook context :physics)
        (walk-game-object-tree (game-object scene-tree)
          (transform-game-object game-object delta-time))
        (compute-collisions context)))))

(u:fn-> run-update-phase (context) null)
(defun run-update-phase (context)
  (declare (optimize speed))
  (with-scope (:update-phase)
    (let ((scene-tree (context-scene-tree context))
          (clock (context-clock context)))
      (activate-traits context)
      (deactivate-traits context)
      (walk-game-object-tree (game-object scene-tree)
        (resolve-world-matrix game-object (clock-alpha clock)))
      (invoke-trait-hook context :update)
      nil)))

(u:fn-> start-game-loop (context &key (:profile-p boolean) (:frame-count (or fixnum null))) null)
(defun start-game-loop (context &key profile-p frame-count)
  (declare (optimize speed))
  ;; Hold on to some variables before we enter the main game loop, since they are needed each
  ;; iteration and won't change. This way we won't have to look them up in a busy loop.
  (let* ((clock (context-clock context))
         (window (context-window context))
         (input-manager (context-input-manager context))
         (viewport-manager (context-viewports context))
         (refresh-rate (get-monitor-refresh-rate (window-monitor window)))
         (physics-phase-func (make-physics-phase-function context)))
    ;; Emulate this function returning by sending the context value to the REPL. This only works on
    ;; Sly, and only if it is configured to allow sending code to the REPL. It is a no-op on other
    ;; environments. See: https://joaotavora.github.io/sly/#Controlling-SLY-from-outside-Emacs
    (send-to-repl (list context) :comment "")
    ;; Request the Lisp implementation to perform a full garbage collection immediately before we
    ;; start the main game loop, to mitigate any large amounts of data from initialization or the
    ;; last run from being cleaned up at runtime causing frame drops.
    (tg:gc :full t)
    ;; Profile the game loop if the user requested to do so.
    (v:debug :zed "Entered game loop")
    (with-profiling (profile-p)
      ;; Actually start the main game loop.
      (u:while (context-running-p context)
        (with-continuable (clock)
          (handle-input-events input-manager window viewport-manager)
          ;; Perform one clock tick.
          (tick-clock clock refresh-rate physics-phase-func)
          ;; Perform update logic that needs to occur each frame.
          (run-update-phase context)
          ;; Draw all game objects with a render trait attached.
          (zed.trait.render::render-frame context)
          ;; Draw this frame to the window.
          (draw-window window)
          ;; Increment the frame counter at the end of the frame.
          (count-clock-frame clock)
          ;; In debug mode only, quit the game after the user-supplied frame count (useful for
          ;; profiling).
          #-zed.release
          (when (and frame-count (>= (clock-frame-count clock) frame-count))
            (shutdown-context context)))))))
