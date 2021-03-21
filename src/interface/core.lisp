(in-package #:zed)

;; The entry point of the engine. This constructs a context using the optional user-supplied
;; arguments, and then enters the main game loop.
(defun start-game (&rest options)
  ;; Create the context, initialized with any user-supplied options.
  (let* ((config (apply #'cfg::make-config options))
         (context (ctx::make-context config))
         (ctx::*context* context))
    (tp::with-thread-pool (ctx::thread-pool context)
      (unwind-protect
           (progn
             ;; Run the user-supplied prelude function. This can be used to run arbitrary code,
             ;; including loading any initial prefabs.
             (funcall (cfg::prelude config) context)
             ;; Start the main game loop.
             (loop::start context
                          :profile-p (cfg::profile-p config)
                          :frame-count (cfg::frame-count config)))
        ;; If we reached this point it means the main game loop has terminated, so clean up the state
        ;; and shut everything down.
        (ctx::destroy context)))))

;; Stop the game associated with the given context.
(defun stop-game (context)
  (ctx::shutdown context))
