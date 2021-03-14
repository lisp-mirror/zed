(in-package #:zed)

;; Create a new game object. This game object is not yet rooted and thus does not exist in the scene
;; until #'insert is called on it. `:disabled-p`, if non-NIL, marks the game object to not be
;; processed in the #game loop. This can be used to implement object pooling and other tasks. This
;; causes the scene #tree traversal to stop at a disabled game object, so its children are also
;; disabled, even if not #explicitly marked as such. `:pause-mode` may be one of `:pause`,
;; `:ignore`, or `:inherit`, and #their behavior is documented in the struct definition at the top
;; of this file.
(u:fn-> make-game-object
        (&key (:label string) (:disabled-p boolean) (:pause-mode gob::pause-mode))
        gob::game-object)
(defun make-game-object (&key (label "[NO-LABEL]") disabled-p (pause-mode :inherit))
  (declare (optimize speed))
  (gob::make-game-object :label label :enabled-p (not disabled-p) :pause-mode pause-mode))

(defun reparent-game-object (context game-object &optional parent)
  (declare (optimize speed))
  (let ((parent (or parent (ctx::scene-tree context))))
    (tree::reparent context game-object parent)))

(defun insert-game-object (context game-object &optional parent)
  (declare (optimize speed))
  (let ((parent (or parent (ctx::scene-tree context))))
    (tree::insert context game-object parent)))

(defun game-object-enabled-p (game-object)
  (declare (optimize speed))
  (gob::enabled-p game-object))

(defun pause-game-object (game-object)
  (declare (optimize speed))
  (dbg::check (not (gob::root-p game-object)))
  (setf (gob::paused-p game-object) t))

(defun unpause-game-object (game-object)
  (declare (optimize speed))
  (setf (gob::paused-p game-object) nil))

(defun game-object-paused-p (game-object)
  (declare (optimize speed))
  (gob::paused-p game-object))
