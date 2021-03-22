(in-package #:zed)

;; The entry point of the engine. This constructs a context using the optional user-supplied
;; arguments, and then enters the main game loop.
(defun start-game (&rest options)
  (let ((config (apply #'cfg::make-config options)))
    (ctx::with-context context (config)
      (loop::start context
                   :profile-p (cfg::profile-p config)
                   :frame-count (cfg::frame-count config)))))

;; Stop the game associated with the given context.
(defun stop-game (context)
  (ctx::shutdown context))
