(in-package #:cl-user)

(defpackage #:%zed.tree
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:clock #:%zed.clock)
   (#:ctx #:%zed.context)
   (#:do #:%zed.draw-order)
   (#:gob #:%zed.game-object)
   (#:tfm #:%zed.transform)
   (#:trait #:%zed.trait)
   (#:util #:%zed.util)
   (#:wl #:%zed.whitelist))
  (:use #:cl)
  (:shadow
   #:delete)
  (:export
   #:%zed.tree
   #:destroy-game-object
   #:reparent-game-object
   #:spawn-game-object))

(in-package #:%zed.tree)

;; Walk down the scene tree starting at the supplied game object, executing the arbitrary code in
;; the body for each game object reached, with the supplied `binding` symbol bound to that game
;; object. Any game objects that are disabled, and their children, will be skipped over unless
;; `:disabled-p` is non-NIL. The same occurs for paused game objects, overriding with `:paused-p`.
;; NOTE: If the passed in game object is the root of the scene tree, only its children will be
;; walked, since the root is an implementation detail and should never be modified, nor does it
;; contain any useful information to a user.
(defmacro walk-tree ((binding game-object &key disabled-p paused-p) &body body)
  `(%walk-tree
    ,game-object
    (lambda (,binding)
      (declare (ignorable ,binding))
      ,@body)
    ,disabled-p
    ,paused-p))

;; Walk the scene tree from a game object and traversing up towards the root, following the parent
;; links. The arbitrary code in the body is executed for each game object that is mapped over, with
;; that game object bound to the supplied `binding` symbol. NOTE: The final root of the scene tree
;; is not acted upon, since it is an implementation detail and should never be modified, nor does it
;; contain any useful information to a user.
(defmacro walk-parents ((binding game-object) &body body)
  (u:with-gensyms (walk-func)
    `(flet ((,walk-func (,binding) ,@body))
       (declare (dynamic-extent #',walk-func))
       (%walk-parents ,game-object #',walk-func))))

;; Helper function for the walk-tree macro.
(u:fn-> %walk-tree (gob::game-object function boolean boolean) null)
(defun %walk-tree (game-object func disabled-p paused-p)
  (declare (optimize speed))
  (wl::with-allowed-scopes walk-tree
      (:prelude :prefab-instantiate :physics-phase :update-phase
       :trait-setup-hook :trait-destroy-hook :trait-attach-hook :trait-detach-hook)
    (labels ((recurse (game-object)
               (when (and (or disabled-p (gob::enabled-p game-object))
                          (or paused-p (not (gob::paused-p game-object))))
                 (funcall func game-object)
                 (dolist (child (gob::children game-object))
                   (recurse child)))))
      (if (gob::root-p game-object)
          (dolist (child (gob::children game-object))
            (recurse child))
          (recurse game-object)))))

;; Helper function for the walk-parents macro.
(u:fn-> %walk-parents (gob::game-object function) null)
(defun %walk-parents (game-object func)
  (declare (optimize speed))
  (wl::with-allowed-scopes walk-parents
      (:prelude :prefab-instantiate :trait-setup-hook :trait-destroy-hook
       :trait-attach-hook :trait-detach-hook)
    (labels ((recurse (game-object)
               (u:when-let ((parent (gob::parent game-object)))
                 (funcall func game-object)
                 (recurse parent))))
      (u:when-let ((parent (gob::parent game-object)))
        (recurse parent)))))

;; Walk up the parents of a game object, stopping and signalling an error if any traversed game
;; object is identical to the passed in parent game object. This is called when spawning and moving
;; game objects around in the scene tree, and only in debug mode. This ensures that the scene tree
;; always forms a valid tree structure.
(u:fn-> %check-reparent-target (gob::game-object gob::game-object) boolean)
(defun %check-reparent-target (game-object parent)
  (declare (optimize speed))
  (when (or (eq game-object parent)
            (block walk
              (walk-parents (x parent)
                (when (eq x game-object)
                  (return-from walk t)))))
    (error "New parent cannot be part of the objects's sub-tree.")))

;; Walk the sub-tree rooted at a game object, modifying each game object's depth property. This
;; simply records an integral depth for each game object that is mapped over. This is called
;; whenever a game object is spawned or moved around in the scene tree.
(u:fn-> %recalculate-sub-tree-depths (ctx::context gob::game-object) null)
(defun %recalculate-sub-tree-depths (context game-object)
  (declare (optimize speed))
  (walk-tree (x game-object)
    (setf (gob::depth x) (1+ (gob::depth (gob::parent x))))
    ;; When a game object's depth changes, we also have to resort its draw call in the draw order
    ;; manager.
    (do::resort context game-object)))

;; Update the game object's path string after it has been spawned or moved in the scene tree. This
;; is done by appending the game object's label to the resolved path of its parent game object,
;; separated by a slash (/) character. This path is solely for human identification purposes.
(u:fn-> %resolve-path (gob::game-object gob::game-object) null)
(defun %resolve-path (game-object parent)
  (declare (optimize speed))
  (let ((parent-path (cond
                       ((and parent (string-equal (gob::path parent) "[UN-ROOTED]"))
                        (gob::label parent))
                       ((gob::parent parent)
                        (gob::path parent))
                       (t ""))))
    (setf (gob::path game-object) (format nil "~a/~a" parent-path (gob::label game-object)))
    nil))

;; Re-parent a game object to be a child of some new parent game object. This is called by
;; #'spawn-game-object to do all the book-keeping of parenting when spawning a new game object, but
;; it can also be called directly to move a game object and its sub-tree around in the scene tree.
;; NOTE: In debug mode, it is checked and an error is signalled if attempting to re-parent a game
;; object under itslef or one of its children, as the result would not be a tree structure. This
;; safeguard is not checked in release mode for performance reasons, so be careful.
(u:fn-> reparent-game-object (ctx::context gob::game-object gob::game-object) gob::game-object)
(declaim (inline reparent-game-object))
(defun reparent-game-object (context game-object new-parent)
  (declare (optimize speed))
  (wl::with-allowed-scopes reparent-game-object
      (:prefab-instantiate :trait-setup-hook :trait-destroy-hook :trait-attach-hook
       :trait-detach-hook :trait-update-hook)
    (let (;; We need the clock so we can resolve the world matrix of the newly placed game object
          ;; the correct interpolation factor.
          (clock (ctx::clock context)))
      ;; Ensure the root is not moved when in debug mode.
      (util::check (not (gob::root-p game-object)))
      ;; Only in debug mode, error if the new parent is within the sub-tree rooted at the game
      ;; object.
      #-zed.release (%check-reparent-target game-object new-parent)
      ;; If the game object currently has a parent, remove the game object from the parent's list of
      ;; children.
      (u:when-let ((current-parent (gob::parent game-object)))
        (u:deletef (gob::children current-parent) game-object))
      ;; Set the new parent reference for the game object.
      (setf (gob::parent game-object) new-parent)
      ;; Make the game object a child of the new parent game object.
      (push game-object (gob::children new-parent))
      ;; Update the tree depth of the moved game object and all of its children.
      (%recalculate-sub-tree-depths context game-object)
      ;; Set the game object's new path used for printing.
      (%resolve-path game-object new-parent)
      ;; Set the game object's pause mode to be that of the new parent if it has a pause mode of
      ;; :inherit.
      (when (eq (gob::pause-mode game-object) :inherit)
        (setf (gob::pause-mode game-object) (gob::pause-mode new-parent)))
      ;; Resolve the new world matrix for the moved game object.
      (tfm::resolve-world-matrix game-object (clock::alpha clock))
      ;; Return the updated game object.
      game-object)))

;; Insert a game object into the sub-tree rooted at some parent game object. It is possible that
;; parent is not rooted in the scene tree, in which case the spawned game object is part of that
;; parent's disjoint tree until it is connected to the main scene tree. This allows building up
;; trees of game objects that are not yet registered with the scene.
(u:fn-> spawn-game-object (ctx::context gob::game-object &optional gob::game-object)
        gob::game-object)
(defun spawn-game-object (context game-object &optional parent)
  (declare (optimize speed))
  (wl::with-allowed-scopes spawn-game-object
      (:prelude :prefab-instantiate :trait-setup-hook :trait-destroy-hook
       :trait-attach-hook :trait-detach-hook :trait-update-hook)
    (reparent-game-object context game-object (or parent (ctx::scene-tree context)))
    (dolist (child (gob::children game-object))
      (spawn-game-object context child game-object))
    game-object))

(u:fn-> destroy-game-object (ctx::context gob::game-object &key (:reparent-p boolean)) null)
(defun destroy-game-object (context game-object &key reparent-p)
  (declare (optimize speed))
  (wl::with-allowed-scopes destroy-game-object
      (:prefab-recompile :trait-setup-hook :trait-destroy-hook :trait-attach-hook
       :trait-detach-hook :trait-update-hook)
    (flet ((deregister-prefab (context game-object)
             (u:when-let ((prefab-name (gob::prefab-name game-object))
                          (table (ctx::prefabs context)))
               (u:deletef (the list (u:href table prefab-name)) game-object)
               (unless (u:href table prefab-name)
                 (remhash prefab-name table))
               nil)))
      #-zed.release
      (util::check (not (gob::root-p game-object)))
      (let ((parent (gob::parent game-object)))
        (dolist (child (gob::children game-object))
          (if reparent-p
              (reparent-game-object context child parent)
              (destroy-game-object context child)))
        (trait::destroy-all-traits game-object)
        (deregister-prefab context game-object)
        (u:deletef (gob::children parent) game-object)
        nil))))
