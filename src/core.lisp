(in-package #:zed)

;; The entry point of the engine. This constructs a context using the optional user-supplied
;; arguments, and then enters the main game loop.
(defun start-game (&rest options)
  (let ((config (apply #'make-config options)))
    (v:info :zed "Started ~a" (config-window-title config))
    (with-context context (config)
      (start-game-loop context
                       :profile-p (config-profile-p config)
                       :frame-count (config-frame-count config))))
  (values))

;; Stop the game associated with the given context.
(defun stop-game (context)
  (shutdown-context context))

;; Pause the game associated with the given context. Only game objects that are marked as pausable
;; stop updating, allowing menus and any other game objects that wish to be interactive to be so.
(u:fn-> pause-game (context) null)
(defun pause-game (context)
  (declare (optimize speed))
  (walk-game-object-tree (x (context-scene-tree context))
    (when (eq (game-object-pause-mode x) :pause)
      (setf (game-object-%paused-p x) t))
    nil))

;; Un-pauses the game associated with the given context.
(u:fn-> unpause-game (context) null)
(defun unpause-game (context)
  (declare (optimize speed))
  (walk-game-object-tree (x (context-scene-tree context) :paused-p t)
    (when (eq (game-object-pause-mode x) :pause)
      (setf (game-object-%paused-p x) nil))
    nil))
