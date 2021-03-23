(in-package #:cl-user)

(defpackage #:%zed.core
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:cfg #:%zed.config)
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:loop #:%zed.game-loop)
   (#:tree #:%zed.tree))
  (:use #:cl)
  (:export
   #:pause-game
   #:start-game
   #:stop-game
   #:unpause-game))

(in-package #:%zed.core)

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

;; Pause the game associated with the given context. Only game objects that are marked as pausable
;; stop updating, allowing menus and any other game objects that wish to be interactive to be so.
(u:fn-> pause-game (ctx::context) null)
(defun pause-game (context)
  (declare (optimize speed))
  (tree::walk-tree (x (ctx::scene-tree context))
    (when (eq (gob::pause-mode x) :pause)
      (setf (gob::paused-p x) t))
    nil))

;; Un-pauses the game associated with the given context.
(u:fn-> unpause-game (ctx::context) null)
(defun unpause-game (context)
  (declare (optimize speed))
  (tree::walk-tree (x (ctx::scene-tree context) :paused-p t)
    (when (eq (gob::pause-mode x) :pause)
      (setf (gob::paused-p x) nil))
    nil))
