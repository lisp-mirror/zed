(in-package #:cl-user)

(defpackage #:%zed.game-loop
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:clock #:%zed.clock)
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:in #:%zed.input)
   (#:jobs #:%zed.jobs)
   (#:live #:%zed.live-coding)
   (#:log #:%zed.logging)
   (#:mon #:%zed.monitor)
   (#:tfm #:%zed.transform)
   (#:tm #:%zed.trait.manager)
   (#:tp #:%zed.thread-pool)
   (#:tr #:%zed.trait)
   (#:tr.ren #:zed.trait.render)
   (#:tree #:%zed.tree)
   (#:util #:%zed.util)
   (#:win #:%zed.window)
   (#:wl #:%zed.whitelist))
  (:use #:cl))

(in-package #:%zed.game-loop)

;; Create a function that is called periodically to perform necessary book-keeping that does not
;; need to run every frame.
(u:fn-> make-periodic-phase-function (ctx::context) function)
(defun make-periodic-phase-function (context)
  (lambda ()
    (declare (optimize speed))
    (live::update-repl (ctx::clock context))
    (tp::process-queue #'live::recompile)))

;; Create a function that is called every clock tick to update the transform state of each game
;; object.
(u:fn-> make-physics-phase-function (ctx::context) function)
(defun make-physics-phase-function (context)
  (let ((scene-tree (ctx::scene-tree context))
        (delta-time (clock::delta-time (ctx::clock context))))
    (lambda ()
      (declare (optimize speed))
      (wl::with-scope (:physics-phase)
        (tree::walk-tree (game-object scene-tree)
          (dolist (trait (tm::order (gob::traits game-object)))
            (tr::call-hook trait :physics)))
        (tree::walk-tree (game-object scene-tree)
          (tfm::transform-game-object game-object delta-time))))))

(u:fn-> run-update-phase (ctx::context) null)
(defun run-update-phase (context)
  (declare (optimize speed))
  (wl::with-scope (:update-phase)
    (let ((scene-tree (ctx::scene-tree context))
          (clock (ctx::clock context))
          (jobs (ctx::jobs context)))
      (jobs::update-enabled-traits jobs)
      (jobs::update-disabled-traits jobs)
      (tree::walk-tree (game-object scene-tree)
        (tfm::resolve-world-matrix game-object (clock::alpha clock)))
      (tree::walk-tree (game-object scene-tree)
        (dolist (trait (tm::order (gob::traits game-object)))
          (tr::call-hook trait :update)))
      nil)))

(u:fn-> start (ctx::context &key (:profile-p boolean) (:frame-count (or fixnum null))) null)
(defun start (context &key profile-p frame-count)
  (declare (optimize speed))
  ;; Hold on to some variables before we enter the main game loop, since they are needed each
  ;; iteration and won't change. This way we won't have to look them up in a busy loop.
  (let* ((clock (ctx::clock context))
         (window (ctx::window context))
         (input-manager (ctx::input-manager context))
         (viewport-manager (ctx::viewports context))
         (refresh-rate (mon::get-refresh-rate (win::monitor window)))
         (physics-phase-func (make-physics-phase-function context))
         (periodic-phase-func (make-periodic-phase-function context)))
    ;; Emulate this function returning by sending the context value to the REPL. This only works on
    ;; Sly, and only if it is configured to allow sending code to the REPL. It is a no-op on other
    ;; environments. See: https://joaotavora.github.io/sly/#Controlling-SLY-from-outside-Emacs
    (live::send-to-repl (list context) :comment "")
    ;; Request the Lisp implementation to perform a full garbage collection immediately before we
    ;; start the main game loop, to mitigate any large amounts of data from initialization or the
    ;; last run from being cleaned up at runtime causing frame drops.
    (tg:gc :full t)
    ;; Profile the game loop if the user requested to do so.
    (log::debug :zed.game-loop "Entered game loop")
    (util::with-profiling (profile-p)
      ;; Actually start the main game loop.
      (u:while (ctx::running-p context)
        (live::with-continuable (clock)
          (in::handle-events input-manager window viewport-manager)
          ;; Perform one clock tick.
          (clock::tick clock refresh-rate physics-phase-func periodic-phase-func)
          ;; Perform update logic that needs to occur each frame.
          (run-update-phase context)
          ;; Draw all game objects with a render trait attached.
          (tr.ren::render-frame context)
          ;; Draw this frame to the window.
          (win::draw window)
          ;; Increment the frame counter at the end of the frame.
          (clock::count-frame clock)
          ;; In debug mode only, quit the game after the user-supplied frame count (useful for
          ;; profiling).
          #-release
          (when (and frame-count (>= (clock::frame-count clock) frame-count))
            (ctx::shutdown context)))))))
