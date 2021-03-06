(in-package #:cl-user)

(defpackage #:%zed.protocol.actor
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.core.context)
   (#:actor #:%zed.game-object.actor))
  (:use #:cl)
  (:export
   #:actor-enabled-p
   #:actor-paused-p
   #:insert-actor
   #:make-actor
   #:pause-actor
   #:reparent-actor
   #:unpause-actor))

(in-package #:%zed.protocol.actor)

;; Create a new actor. This actor is not yet rooted and thus does not exist in the scene until
;; #'insert is called on it. `:disabled-p`, if non-NIL, marks the actor to not be processed in the
;; #game loop. This can be used to implement object pooling and other tasks. This causes the scene
;; #tree traversal to stop at a disabled actor, so its children are also disabled, even if not
;; #explicitly marked as such. `:pause-mode` may be one of `:pause`, `:ignore`, or `:inherit`, and
;; #their behavior is documented in the struct definition at the top of this file.
(u:fn-> make-actor
        (&key (:label string) (:disabled-p boolean) (:pause-mode (member :pause :ignore :inherit)))
        actor::actor)
(defun make-actor (&key (label "[NO-LABEL]") disabled-p (pause-mode :inherit))
  (declare (optimize speed))
  (actor::make-actor :label label :enabled-p (not disabled-p) :pause-mode pause-mode))

(defun reparent-actor (context actor &optional parent)
  (let ((parent (or parent (ctx::scene-tree context))))
    (actor::reparent actor parent)))

(defun insert-actor (context actor &optional parent)
  (let ((parent (or parent (ctx::scene-tree context))))
    (actor::insert actor parent)))

(defun actor-enabled-p (actor)
  (actor::enabled-p actor))

(defun pause-actor (actor)
  ;; TODO: Add debug mode check for root actor.
  (setf (actor::paused-p actor) t))

(defun unpause-actor (actor)
  (setf (actor::paused-p actor) nil))

(defun actor-paused-p (actor)
  (actor::paused-p actor))
