(in-package #:cl-user)

(defpackage #:%zed.game-object
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:tm #:%zed.trait.manager)
   (#:ts #:%zed.transform-state)
   (#:util #:%zed.util))
  (:use #:cl)
  (:export
   #:game-object
   #:game-object-enabled-p
   #:game-object-paused-p
   #:make-game-object
   #:pause-game-object
   #:unpause-game-object))

(in-package #:%zed.game-object)

(deftype pause-mode () '(member :pause :ignore :inherit))

(declaim (inline %make-game-object))
(defstruct (game-object
            (:constructor %make-game-object)
            (:conc-name nil)
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
  (enabled-p t :type boolean)
  ;; Whether the game object is currently paused.
  (paused-p nil :type boolean)
  ;; Determines if the game object and its children should be paused when the game is paused.
  ;; :pause - The game object and its children are paused when the game is paused.
  ;; :ignore - The game object and its children continue to update when the game is paused.
  ;; :inherit - The pause mode for the game object is the pause mode of its parent game object.
  (pause-mode :inherit :type pause-mode)
  ;; The transform state of the game object.
  (transform (ts::make-state) :type ts::state)
  ;; An object that manages the currently attached traits this game object has.
  (traits (tm::make-manager) :type tm::manager)
  ;; The name of the root prefab node this game object was created from, or NIL if it was created
  ;; from a child of a prefab, or manually
  (prefab-name nil :type symbol))

(u:define-printer (game-object stream :type nil)
  (format stream "GAME-OBJECT: ~a" (path game-object)))

;; Create a new game object. This game object is not yet rooted and thus does not exist in the scene
;; until #'insert is called on it. `:disabled-p`, if non-NIL, marks the game object to not be
;; processed in the #game loop. This can be used to implement object pooling and other tasks. This
;; causes the scene #tree traversal to stop at a disabled game object, so its children are also
;; disabled, even if not #explicitly marked as such. `:pause-mode` may be one of `:pause`,
;; `:ignore`, or `:inherit`, and #their behavior is documented in the struct definition at the top
;; of this file.
(u:fn-> make-game-object (&key (:label string) (:disabled-p boolean) (:pause-mode pause-mode))
        game-object)
(declaim (inline make-game-object))
(defun make-game-object (&key (label "[NO-LABEL]") disabled-p (pause-mode :inherit))
  (declare (optimize speed))
  (%make-game-object :label label :enabled-p (not disabled-p) :pause-mode pause-mode))

(defun make-root ()
  (%make-game-object :label "[ROOT]" :path "/" :root-p t :depth 0 :pause-mode :pause))

(u:fn-> game-object-enabled-p (game-object) boolean)
(declaim (inline game-object-enabled-p))
(defun game-object-enabled-p (game-object)
  (declare (optimize speed))
  (enabled-p game-object))

(u:fn-> game-object-paused-p (game-object) boolean)
(declaim (inline game-object-paused-p))
(defun game-object-paused-p (game-object)
  (declare (optimize speed))
  (paused-p game-object))

(u:fn-> pause-game-object (game-object) null)
(declaim (inline pause-game-object))
(defun pause-game-object (game-object)
  (declare (optimize speed))
  (util::check (not (root-p game-object)))
  (setf (paused-p game-object) t)
  nil)

(u:fn-> unpause-game-object (game-object) null)
(declaim (inline unpause-game-object))
(defun unpause-game-object (game-object)
  (declare (optimize speed))
  (setf (paused-p game-object) nil)
  nil)
