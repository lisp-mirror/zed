(in-package #:cl-user)

(defpackage #:%zed.protocol.core
  (:local-nicknames
   (#:cfg #:%zed.base.config)
   (#:ctx #:%zed.core.context)
   (#:gob #:%zed.game-object)
   (#:live #:%zed.base.live-coding)
   (#:loop #:%zed.core.game-loop)
   (#:tree #:%zed.core.tree))
  (:use #:cl)
  (:export
   #:pause-game
   #:start-game
   #:stop-game
   #:unpause-game))

(in-package #:%zed.protocol.core)

;; The entry point of the engine. This constructs a context using the optional user-supplied
;; arguments, and then enters the main game loop.
(defun start-game (&rest options)
  ;; Create the context, initialized with any user-supplied options.
  (let* ((context (ctx::make-context (apply #'cfg::make-config options)))
         (ctx::*context* context))
    ;; Emulate this function returning by sending the context value to the REPL. This only works on
    ;; Sly, and only if it is configured to allow sending code to the REPL. See:
    ;; https://joaotavora.github.io/sly/#Controlling-SLY-from-outside-Emacs
    (live::send-to-repl (list context) :comment "")
    ;; Start the main game loop.
    (unwind-protect (loop::start context)
      ;; If we reached this point it means the main game loop has terminated, so clean up the state
      ;; and shut everything down.
      (ctx::destroy context))))

;; Stop the game associated with the given context.
(defun stop-game (context)
  (ctx::shutdown context))

;; Pause the game associated with the given context. Only game objects that are marked as pausable
;; stop updating, allowing menus and any other game objects that wish to be interactive to be so.
(defun pause-game (context)
  (tree::walk-tree (x (ctx::scene-tree context))
    (when (eq (gob::pause-mode x) :pause)
      (setf (gob::paused-p x) t))))

;; Un-pauses the game associated with the given context.
(defun unpause-game (context)
  (tree::walk-tree (x (ctx::scene-tree context) :paused-p t)
    (when (eq (gob::pause-mode x) :pause)
      (setf (gob::paused-p x) nil))))
