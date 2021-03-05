(in-package #:cl-user)

;;;; This package contains the infrastructure for starting and stopping the engine.
(defpackage #:%zed.core
  (:local-nicknames
   (#:cfg #:%zed.core.config)
   (#:ctx #:%zed.core.context)
   (#:loop #:%zed.core.game-loop))
  (:use #:cl)
  (:export
   #:start))

(in-package #:%zed.core)

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

(defun stop (context)
  ;; The main game loop iterates continuously as long as `running-p` is non-NIL, so setting this
  ;; to NIL will cause the engine to gracefully shutdown on it's own.
  (setf (ctx::running-p context) nil))
