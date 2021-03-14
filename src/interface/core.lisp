(in-package #:zed)

;; The entry point of the engine. This constructs a context using the optional user-supplied
;; arguments, and then enters the main game loop.
(defun start-game (&rest options)
  ;; Create the context, initialized with any user-supplied options.
  (let* ((context (ctx::make-context (apply #'cfg::make-config options)))
         (ctx::*context* context))
    ;; Start the main game loop.
    (unwind-protect (loop::start context)
      ;; If we reached this point it means the main game loop has terminated, so clean up the state
      ;; and shut everything down.
      (ctx::destroy context))))

;; Stop the game associated with the given context.
(defun stop-game (context)
  (ctx::shutdown context))
