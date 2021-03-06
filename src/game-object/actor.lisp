(in-package #:cl-user)

(defpackage #:%zed.game-object.actor
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:base #:%zed.base))
  (:use #:cl))

(in-package #:%zed.game-object.actor)

(declaim (inline make-actor))
(defstruct (actor
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; A label to identify this actor. This is used to build the actor's printed representation. Does
  ;; not need to be unique.
  (label "[NO-LABEL]" :type string)
  ;; A string denoting the path into the scene tree for this actor. This is built by appending the
  ;; labels of each actor from the root, and is used for the actor's printed representation.
  (path "[UN-ROOTED]" :type string)
  ;; Whether the actor is the root of the scene tree or not.
  (root-p nil :type boolean)
  ;; A reference to this actor's parent actor, or NIL if it is the root actor.
  (parent nil :type (or actor null))
  ;; A list of references of this actor's children.
  (children nil :type list)
  ;; The depth this actor exists into the scene tree
  (depth 1 :type u:ub16)
  ;; Whether the actor is currently enabled.
  (enabled-p t :type boolean)
  ;; Whether the actor is currently paused.
  (paused-p nil :type boolean)
  ;; Determines if the actor and its children should be paused when the game is paused.
  ;; :pause - The actor and its children are paused when the game is paused.
  ;; :ignore - The actor and its children continue to update when the game is paused.
  ;; :inherit - The pause mode for the actor is the pause mode of its parent actor.
  (pause-mode :inherit :type (member :pause :ignore :inherit)))

(u:define-printer (actor stream :type nil)
  (format stream "ACTOR ~a" (path actor)))

(defun make-root ()
  (make-actor :label "[ROOT]" :path "/" :root-p t :depth 0 :pause-mode :pause))

;; Walk down the scene tree starting at the supplied actor, executing the arbitrary code in the body
;; for each actor reached, with the supplied `binding` symbol bound to that actor. Any actors that
;; are disabled, and their children, will be skipped over unless `:include-disabled-p` is non-NIL.
;; The same occurs for paused actors, overriding with `:include-paused-p`. NOTE: If the passed in
;; actor is the root of the scene tree, only its children will be walked, since the root is an
;; implementation detail and should never be modified, nor does it contain any useful information to
;; a user.
(defmacro walk-tree ((binding actor &key include-disabled-p include-paused-p) &body body)
  `(%walk-tree
    ,actor
    (lambda (,binding)
      (declare (ignorable ,binding))
      ,@body)
    ,include-disabled-p
    ,include-paused-p))

;; Walk the scene tree from an actor and traversing up towards the root, following the parent links.
;; The arbitrary code in the body is executed for each actor that is mapped over, with that actor
;; bound to the supplied `binding` symbol. NOTE: The final root of the scene tree is not acted upon,
;; since it is an implementation detail and should never be modified, nor does it contain any useful
;; information to a user.
(defmacro walk-parents ((binding actor) &body body)
  `(%walk-parents
    ,actor
    (lambda (,binding)
      (declare (ignorable ,binding))
      ,@body)))

;; Helper function for the walk-tree macro.
(u:fn-> %walk-tree (actor function boolean boolean) null)
(defun %walk-tree (actor func include-disabled-p include-paused-p)
  (declare (optimize speed))
  (labels ((recurse (actor)
             (when (and (or include-disabled-p (enabled-p actor))
                        (or include-paused-p (not (paused-p actor))))
               (funcall func actor)
               (dolist (child (children actor))
                 (recurse child)))))
    (if (root-p actor)
        (dolist (child (children actor))
          (recurse child))
        (recurse actor))))

;; Helper function for the walk-parents macro.
(defun %walk-parents (actor func)
  (labels ((recurse (actor)
             (u:when-let ((parent (parent actor)))
               (funcall func actor)
               (recurse parent))))
    (u:when-let ((parent (parent actor)))
      (recurse parent))))

;; Walk up the parents of an actor, stopping and signalling an error if any traversed actor is
;; identical to the passed in parent actor. This is called when inserting and moving actors around
;; in the scene tree, and only in debug mode. This ensures that the scene tree always forms a valid
;; tree structure.
(defun %check-reparent-target (actor parent)
  (when (or (eq actor parent)
            (block walk
              (walk-parents (x parent)
                (when (eq x actor)
                  (return-from walk t)))))
    (error "New parent cannot be part of the actor's sub-tree.")))

;; Walk the sub-tree rooted at an actor, modifying each actor's depth property. This simply records
;; an integral depth for each actor that is mapped over. This is called whenever an actor is
;; inserted or moved around in the scene tree.
(u:fn-> %recalculate-sub-tree-depths (actor) null)
(defun %recalculate-sub-tree-depths (actor)
  (declare (optimize speed))
  (walk-tree (x actor)
    (setf (depth x) (1+ (depth (parent x))))))

;; Update the actor's path string after it has been inserted or moved in the scene tree. This is
;; done by appending the actor's label to the resolved path of its parent actor, separated by a
;; slash (/) character. This path is solely for human identification purposes.
(u:fn-> %resolve-path (actor actor) null)
(defun %resolve-path (actor parent)
  (declare (optimize speed))
  (let ((parent-path (cond
                       ((and parent (string-equal (path parent) "[UN-ROOTED]"))
                        (label parent))
                       ((parent parent)
                        (path parent))
                       (t ""))))
    (setf (path actor) (format nil "~a/~a" parent-path (label actor)))
    nil))

;; Re-parent an actor to be a child of some new parent actor. This is called by #'insert to do all
;; the book-keeping of parenting when inserting a new actor, but it can also be called directly to
;; move an actor and its sub-tree around in the scene tree. NOTE: In debug mode, it is checked and
;; an error is signalled if attempting to re-parent an actor under itslef or one of its children, as
;; the result would not be a tree structure. This safeguard is not checked in release mode for
;; performance reasons, so be careful.
(u:fn-> reparent (actor actor) actor)
(declaim (inline reparent))
(defun reparent (actor new-parent)
  (declare (optimize speed))
  ;; Ensure the root is not moved when in debug mode.
  (base::debug-check (not (root-p actor)))
  ;; Only in debug mode, error if the new parent is within the sub-tree rooted at the actor.
  #-zed.release (%check-reparent-target actor new-parent)
  ;; If the actor currently has a parent, remove the actor from the parent's list of children.
  (u:when-let ((current-parent (parent actor)))
    (u:deletef (children current-parent) actor))
  ;; Set the new parent reference for the actor.
  (setf (parent actor) new-parent)
  ;; Make the actor a child of the new parent actor.
  (push actor (children new-parent))
  ;; Update the tree depth of the moved actor and all of its children.
  (%recalculate-sub-tree-depths actor)
  ;; Set the actor's new path used for printing.
  (%resolve-path actor new-parent)
  ;; Set the actor's pause mode to be that of the new parent if it has a pause mode of :inherit.
  (when (eq (pause-mode actor) :inherit)
    (setf (pause-mode actor) (pause-mode new-parent)))
  ;; Return the updated actor.
  actor)

;; Insert an actor into the sub-tree rooted at some parent actor. It is possible that parent is not
;; rooted in the scene tree, in which case the inserted actor is part of that parent's disjoint tree
;; until it is connected to the main scene tree. This allows building up trees of actors that are
;; not yet registered with the scene.
(u:fn-> insert (actor actor) actor)
(defun insert (actor parent)
  (declare (optimize speed))
  (reparent actor parent)
  (dolist (child (children actor))
    (insert child actor))
  actor)
