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
   (#:mon #:%zed.monitor)
   (#:tp #:%zed.thread-pool)
   (#:tr #:%zed.transform)
   (#:trait #:%zed.trait)
   (#:tr.ren #:zed.trait.render)
   (#:tree #:%zed.tree)
   (#:win #:%zed.window))
  (:use #:cl))

(in-package #:%zed.game-loop)

;; Create a function that is called periodically to perform necessary book-keeping that does not
;; need to run every frame.
(u:fn-> make-periodic-update-function (ctx::context) function)
(defun make-periodic-update-function (context)
  (lambda ()
    (declare (optimize speed))
    (live::update-repl (ctx::clock context))
    (tp::process-queue (ctx::thread-pool context))))

;; Create a function that is called every clock tick to update the transform state of each game
;; object.
(u:fn-> make-physics-update-function (ctx::context) function)
(defun make-physics-update-function (context)
  (let ((scene-tree (ctx::scene-tree context))
        (delta-time (clock::delta-time (ctx::clock context))))
    (lambda ()
      (declare (optimize speed))
      (tree::walk-tree (x scene-tree)
        (tr::transform-game-object x delta-time)))))

(u:fn-> update (ctx::context) null)
(defun update (context)
  (declare (optimize speed))
  (let ((scene-tree (ctx::scene-tree context))
        (clock (ctx::clock context))
        (jobs (ctx::jobs context)))
    (jobs::update-enabled-traits jobs)
    (jobs::update-disabled-traits jobs)
    (tree::walk-tree (x scene-tree)
      (tr::resolve-world-matrix x (clock::alpha clock)))
    (tree::walk-tree (x scene-tree)
      (dolist (c (gob::traits x))
        (funcall (fdefinition (trait::update-hook c)) c)))
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
    ;; Emulate this function returning by sending the context value to the REPL. This only works on
    ;; Sly, and only if it is configured to allow sending code to the REPL. It is a no-op on other
    ;; environments. See: https://joaotavora.github.io/sly/#Controlling-SLY-from-outside-Emacs
    (live::send-to-repl (list context) :comment "")
    ;; Request the Lisp implementation to perform a full garbage collection immediately before we
    ;; start the main game loop, to mitigate any large amounts of data from initialization or the
    ;; last run from being cleaned up at runtime causing frame drops.
    (tg:gc :full t)
    ;; Actually start the main game loop.
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
        ;; Draw all game objects with a render trait attached.
        (tr.ren::render-frame context)
        ;; Draw this frame to the window.
        (win::draw window)
        ;; Increment the frame counter at the end of the frame.
        (clock::count-frame clock)))))
