(in-package #:cl-user)

(defpackage #:%zed.core.game-loop
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:clock #:%zed.base.clock)
   (#:ctx #:%zed.core.context)
   (#:in #:%zed.input)
   (#:jobs #:%zed.game-object.jobs)
   (#:live #:%zed.base.live-coding)
   (#:mon #:%zed.render.monitor)
   (#:tp #:%zed.base.thread-pool)
   (#:tr #:%zed.core.transform)
   (#:tree #:%zed.core.tree)
   (#:win #:%zed.render.window))
  (:use #:cl))

(in-package #:%zed.core.game-loop)

;; Create a function that is called periodically to perform necessary book-keeping that does not
;; need to run every frame.
(defun make-periodic-update-function (context)
  (lambda ()
    (live::update-repl (ctx::clock context))
    (tp::process-queue context (ctx::thread-pool context))))

;; Create a function that is called every clock tick to update the transform state of each game
;; object.
(defun make-physics-update-function (context)
  (let ((scene-tree (ctx::scene-tree context))
        (delta-time (clock::delta-time (ctx::clock context))))
    (lambda ()
      (tree::walk-tree (x scene-tree)
        (tr::transform-game-object x delta-time)))))

(u:fn-> update (ctx::context) null)
(defun update (context)
  (declare (optimize speed))
  (let ((jobs (ctx::jobs context)))
    (jobs::update-enabled-traits jobs)
    (jobs::update-disabled-traits jobs)
    nil))

(defun start (context)
  (declare (optimize speed))
  ;; Hold on to some variables before we enter the main game loop, since they are needed each
  ;; iteration and won't change. This way we won't have to look them up in a busy loop.
  (let* ((clock (ctx::clock context))
         (window (ctx::window context))
         (input-manager (ctx::input-manager context))
         (refresh-rate (mon::get-refresh-rate (win::monitor window)))
         (physics-func (make-physics-update-function context))
         (periodic-func (make-periodic-update-function context)))
    ;; Request the Lisp implementation to perform a full garbage collection immediately before we
    ;; start the main game loop, to mitigate any large amounts of data from initialization or the
    ;; last run from being cleaned up at runtime causing frame drops.
    (tg:gc :full t)
    ;; Actually start the main game loop.
    (tp::with-thread-pool (ctx::thread-pool context)
      (u:while (ctx::running-p context)
        (live::with-continuable (clock)
          (in::handle-events input-manager window)
          ;; HACK: Remove this later when possible. This is just so we can easily stop the engine with
          ;; the Escape key.
          (when (in::on-button-enter input-manager :key :escape)
            (ctx::shutdown context))
          ;; Perform one clock tick.
          (clock::tick clock refresh-rate physics-func periodic-func)
          ;; Perform update logic that needs to occur each frame.
          (update context)
          ;; Draw this frame to the window.
          (win::draw window)
          ;; Increment the frame counter at the end of the frame.
          (clock::count-frame clock))))))
