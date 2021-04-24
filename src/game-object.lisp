(in-package #:zed)

(deftype pause-mode () '(member :pause :ignore :inherit))

(declaim (inline %make-game-object))
(defstruct (game-object
            (:constructor %make-game-object)
            (:predicate nil)
            (:copier nil))
  ;; A label to identify this game object. This is used to build the game object's printed
  ;; representation. Does not need to be unique.
  (label "[NO-LABEL]" :type string)
  ;; A string denoting the path into the scene tree for this game object. This is built by appending
  ;; the labels of each game object from the root, and is used for the game object's printed
  ;; representation.
  (path "[UN-ROOTED]" :type string)
  ;; Whether the game object is the root of the scene tree or not.
  (root-p nil :type boolean)
  ;; A reference to this game object's parent game object, or NIL if it is the root game object.
  (parent nil :type (or game-object null))
  ;; A list of references of this game object's children.
  (children nil :type list)
  ;; The depth this game object exists into the scene tree.
  (depth 1 :type u:ub16)
  ;; Whether the game object is currently enabled.
  (%enabled-p t :type boolean)
  ;; Whether the game object is currently paused.
  (%paused-p nil :type boolean)
  ;; Determines if the game object and its children should be paused when the game is paused.
  ;; :pause - The game object and its children are paused when the game is paused.
  ;; :ignore - The game object and its children continue to update when the game is paused.
  ;; :inherit - The pause mode for the game object is the pause mode of its parent game object.
  (pause-mode :inherit :type pause-mode)
  ;; The transform state of the game object.
  (transform (make-transform-state) :type transform-state)
  ;; An object that manages the currently attached traits this game object has.
  (traits-by-id (u:dict #'eq) :type hash-table)
  (traits-by-type (u:dict #'eq) :type hash-table)
  ;; The name of the root prefab node this game object was created from, or NIL if it was created
  ;; from a child of a prefab, or manually
  (prefab-name nil :type symbol)
  ;; The name of the viewport this game object and all of its children are rendered to, if they have
  ;; a render trait.
  (viewport :default :type symbol))

(u:define-printer (game-object stream :type nil)
  (format stream "GAME-OBJECT: ~a" (game-object-path game-object)))

;; Create a new game object. This game object is not yet rooted and thus does not exist in the scene
;; until #'insert is called on it. `:disabled-p`, if non-NIL, marks the game object to not be
;; processed in the #game loop. This can be used to implement object pooling and other tasks. This
;; causes the scene #tree traversal to stop at a disabled game object, so its children are also
;; disabled, even if not #explicitly marked as such. `:pause-mode` may be one of `:pause`,
;; `:ignore`, or `:inherit`, and #their behavior is documented in the struct definition at the top
;; of this file.
(u:fn-> make-game-object
        (&key (:label string) (:disabled-p boolean) (:pause-mode pause-mode) (:viewport symbol))
        game-object)
(declaim (inline make-game-object))
(defun make-game-object (&key (label "[NO-LABEL]") disabled-p (pause-mode :inherit)
                           (viewport :default))
  (declare (optimize speed))
  (%make-game-object :label label
                     :%enabled-p (not disabled-p)
                     :pause-mode pause-mode
                     :viewport viewport))

(defun make-root-game-object ()
  (%make-game-object :label "[ROOT]" :path "/" :root-p t :depth 0 :pause-mode :pause))

(u:fn-> game-object-enabled-p (game-object) boolean)
(declaim (inline game-object-enabled-p))
(defun game-object-enabled-p (game-object)
  (declare (optimize speed))
  (game-object-%enabled-p game-object))

(u:fn-> game-object-paused-p (game-object) boolean)
(declaim (inline game-object-paused-p))
(defun game-object-paused-p (game-object)
  (declare (optimize speed))
  (game-object-%paused-p game-object))

(u:fn-> pause-game-object (game-object) null)
(declaim (inline pause-game-object))
(defun pause-game-object (game-object)
  (declare (optimize speed))
  (debug-check (not (game-object-root-p game-object)))
  (setf (game-object-%paused-p game-object) t)
  nil)

(u:fn-> unpause-game-object (game-object) null)
(declaim (inline unpause-game-object))
(defun unpause-game-object (game-object)
  (declare (optimize speed))
  (setf (game-object-%paused-p game-object) nil)
  nil)

;; Walk down the scene tree starting at the supplied game object, executing the arbitrary code in
;; the body for each game object reached, with the supplied `binding` symbol bound to that game
;; object. Any game objects that are disabled, and their children, will be skipped over unless
;; `:disabled-p` is non-NIL. The same occurs for paused game objects, overriding with `:paused-p`.
;; NOTE: If the passed in game object is the root of the scene tree, only its children will be
;; walked, since the root is an implementation detail and should never be modified, nor does it
;; contain any useful information to a user.
(defmacro walk-game-object-tree ((binding game-object &key disabled-p paused-p) &body body)
  `(%walk-game-object-tree
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
(defmacro walk-game-object-parents ((binding game-object) &body body)
  (u:with-gensyms (walk-func)
    `(flet ((,walk-func (,binding) ,@body))
       (declare (dynamic-extent #',walk-func))
       (%walk-game-object-parents ,game-object #',walk-func))))

;; Helper function for the walk-tree macro.
(u:fn-> %walk-game-object-tree (game-object function boolean boolean) null)
(defun %walk-game-object-tree (game-object func disabled-p paused-p)
  (declare (optimize speed))
  (with-allowed-scopes walk-tree
      (:pause-game :prelude :prefab-instantiate :physics-phase
       :update-phase :trait-setup-hook :trait-destroy-hook :trait-attach-hook :trait-detach-hook)
    (labels ((recurse (game-object)
               (when (and (or disabled-p (game-object-%enabled-p game-object))
                          (or paused-p (not (game-object-%paused-p game-object))))
                 (funcall func game-object)
                 (dolist (child (game-object-children game-object))
                   (recurse child)))))
      (if (game-object-root-p game-object)
          (dolist (child (game-object-children game-object))
            (recurse child))
          (recurse game-object)))))

;; Helper function for the walk-parents macro.
(u:fn-> %walk-game-object-parents (game-object function) null)
(defun %walk-game-object-parents (game-object func)
  (declare (optimize speed))
  (with-allowed-scopes walk-parents
      (:prelude :prefab-instantiate :trait-setup-hook :trait-destroy-hook
       :trait-attach-hook :trait-detach-hook)
    (labels ((recurse (game-object)
               (u:when-let ((parent (game-object-parent game-object)))
                 (funcall func game-object)
                 (recurse parent))))
      (u:when-let ((parent (game-object-parent game-object)))
        (recurse parent)))))

;; Walk up the parents of a game object, stopping and signalling an error if any traversed game
;; object is identical to the passed in parent game object. This is called when spawning and moving
;; game objects around in the scene tree, and only in debug mode. This ensures that the scene tree
;; always forms a valid tree structure.
(u:fn-> %check-game-object-reparent-target (game-object game-object) boolean)
(defun %check-game-object-reparent-target (game-object parent)
  (declare (optimize speed))
  (when (or (eq game-object parent)
            (block walk
              (walk-game-object-parents (x parent)
                (when (eq x game-object)
                  (return-from walk t)))))
    (error "New parent cannot be part of the objects's sub-tree.")))

;; Walk the sub-tree rooted at a game object, modifying each game object's depth property. This
;; simply records an integral depth for each game object that is mapped over. This is called
;; whenever a game object is spawned or moved around in the scene tree.
(u:fn-> %recalculate-game-object-sub-tree-depths (core game-object) null)
(defun %recalculate-game-object-sub-tree-depths (core game-object)
  (declare (optimize speed))
  (walk-game-object-tree (x game-object)
    (setf (game-object-depth x) (1+ (game-object-depth (game-object-parent x))))
    ;; When a game object's depth changes, we also have to resort its draw call in the draw order
    ;; manager.
    (resort-draw-order core game-object)))

;; Update the game object's path string after it has been spawned or moved in the scene tree. This
;; is done by appending the game object's label to the resolved path of its parent game object,
;; separated by a slash (/) character. This path is solely for human identification purposes.
(u:fn-> %resolve-game-object-path (game-object game-object) null)
(defun %resolve-game-object-path (game-object parent)
  (declare (optimize speed))
  (let* ((parent-path (cond
                        ((and parent (string-equal (game-object-path parent) "[UN-ROOTED]"))
                         (game-object-label parent))
                        ((game-object-parent parent)
                         (game-object-path parent))
                        (t "")))
         (path (format nil "~a/~a" parent-path (game-object-label game-object))))
    (setf (game-object-path game-object) path)
    nil))

;; Re-parent a game object to be a child of some new parent game object. This is called by
;; #'spawn-game-object to do all the book-keeping of parenting when spawning a new game object, but
;; it can also be called directly to move a game object and its sub-tree around in the scene tree.
;; NOTE: In debug mode, it is checked and an error is signalled if attempting to re-parent a game
;; object under itslef or one of its children, as the result would not be a tree structure. This
;; safeguard is not checked in release mode for performance reasons, so be careful.
(u:fn-> reparent-game-object (core game-object game-object) game-object)
(declaim (inline reparent-game-object))
(defun reparent-game-object (core game-object new-parent)
  (declare (optimize speed))
  (with-allowed-scopes reparent-game-object
      (:prefab-instantiate :trait-setup-hook :trait-destroy-hook :trait-attach-hook
       :trait-detach-hook :trait-physics-hook :trait-update-hook)
    (let (;; We need the clock so we can resolve the world matrix of the newly placed game object
          ;; the correct interpolation factor.
          (clock (core-clock core)))
      ;; Ensure the root is not moved when in debug mode.
      (debug-check (not (game-object-root-p game-object)))
      ;; Only in debug mode, error if the new parent is within the sub-tree rooted at the game
      ;; object.
      #-zed.release (%check-game-object-reparent-target game-object new-parent)
      ;; If the game object currently has a parent, remove the game object from the parent's list of
      ;; children.
      (u:when-let ((current-parent (game-object-parent game-object)))
        (u:deletef (game-object-children current-parent) game-object))
      ;; Set the new parent reference for the game object.
      (setf (game-object-parent game-object) new-parent)
      ;; Make the game object a child of the new parent game object.
      (push game-object (game-object-children new-parent))
      ;; Update the tree depth of the moved game object and all of its children.
      (%recalculate-game-object-sub-tree-depths core game-object)
      ;; Set the game object's new path used for printing.
      (%resolve-game-object-path game-object new-parent)
      ;; Set the game object's pause mode to be that of the new parent if it has a pause mode of
      ;; :inherit.
      (when (eq (game-object-pause-mode game-object) :inherit)
        (setf (game-object-pause-mode game-object) (game-object-pause-mode new-parent)))
      ;; Resolve the new world matrix for the moved game object.
      (resolve-world-matrix game-object (clock-interpolation-factor clock))
      ;; Return the updated game object.
      game-object)))

;; Insert a game object into the sub-tree rooted at some parent game object. It is possible that
;; parent is not rooted in the scene tree, in which case the spawned game object is part of that
;; parent's disjoint tree until it is connected to the main scene tree. This allows building up
;; trees of game objects that are not yet registered with the scene.
(u:fn-> spawn-game-object
        (core game-object &key (:parent game-object) (:viewport symbol))
        game-object)
(defun spawn-game-object (core game-object &key parent (viewport :default))
  (declare (optimize speed))
  (with-allowed-scopes spawn-game-object
      (:prelude :prefab-instantiate :trait-setup-hook :trait-destroy-hook
       :trait-attach-hook :trait-detach-hook :trait-physics-hook :trait-update-hook)
    (reparent-game-object core game-object (or parent (core-scene-tree core)))
    (setf (game-object-viewport game-object) viewport)
    (dolist (child (game-object-children game-object))
      (spawn-game-object core child :parent game-object))
    game-object))

(u:fn-> destroy-game-object (core game-object &key (:reparent-p boolean)) null)
(defun destroy-game-object (core game-object &key reparent-p)
  (declare (optimize speed))
  (with-allowed-scopes destroy-game-object
      (:prefab-recompile :trait-setup-hook :trait-destroy-hook :trait-attach-hook
       :trait-detach-hook :trait-physics-hook :trait-update-hook :collision-hook)
    (flet ((deregister-prefab (core game-object)
             (u:when-let ((prefab-name (game-object-prefab-name game-object))
                          (table (core-prefabs core)))
               (u:deletef (the list (u:href table prefab-name)) game-object)
               (unless (u:href table prefab-name)
                 (remhash prefab-name table))
               nil)))
      #-zed.release
      (debug-check (not (game-object-root-p game-object)))
      (let ((parent (game-object-parent game-object)))
        (dolist (child (game-object-children game-object))
          (if reparent-p
              (reparent-game-object core child parent)
              (destroy-game-object core child)))
        (destroy-all-traits game-object)
        (deregister-prefab core game-object)
        (u:deletef (game-object-children parent) game-object)
        nil))))
