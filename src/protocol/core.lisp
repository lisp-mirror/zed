(in-package #:cl-user)

(defpackage #:%zed.protocol.core
  (:local-nicknames
   (#:cfg #:%zed.base.config)
   (#:ctx #:%zed.core.context)
   (#:loop #:%zed.core.game-loop))
  (:use #:cl)
  (:export
   #:start
   #:stop))

(in-package #:%zed.protocol.core)

;; The entry point of the engine. This constructs a context using any of the optional user-supplied
;; arguments, and then enters the main game loop.
(defun start (&rest options)
  ;; Create the context, initialized with any user-supplied options.
  (let* ((context (ctx::make-context (apply #'cfg::make-config options)))
         (ctx::*context* context))
    ;; Start the main game loop.
    (unwind-protect (loop::start context)
      ;; If we reached this point it means the main game loop has terminated, so clean up the state
      ;; and shut everything down.
      (ctx::destroy context))))

;; Stop the currently running context.
(defun stop (context)
  (ctx::shutdown context))
