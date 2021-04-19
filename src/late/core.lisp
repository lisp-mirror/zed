(in-package #:zed)

;; The entry point of the engine. This constructs a core using the optional user-supplied
;; arguments, and then enters the main game loop.
(defun start-game (context)
  (make-instance (find-context context))
  (values))

;; Stop the game associated with the given core.
(defun stop-game (core)
  (shutdown-core core))

;; Pause the game associated with the given core. Only game objects that are marked as pausable
;; stop updating, allowing menus and any other game objects that wish to be interactive to be so.
(u:fn-> pause-game (core) null)
(defun pause-game (core)
  (declare (optimize speed))
  (with-scope (:pause-game)
    (walk-game-object-tree (x (core-scene-tree core))
      (when (eq (game-object-pause-mode x) :pause)
        (setf (game-object-%paused-p x) t))
      nil)))

;; Un-pauses the game associated with the given core.
(u:fn-> unpause-game (core) null)
(defun unpause-game (core)
  (declare (optimize speed))
  (with-scope (:pause-game)
    (walk-game-object-tree (x (core-scene-tree core) :paused-p t)
      (when (eq (game-object-pause-mode x) :pause)
        (setf (game-object-%paused-p x) nil))
      nil)))
