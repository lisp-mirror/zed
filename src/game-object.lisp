(in-package #:cl-user)

(defpackage #:%zed.game-object
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:trs #:%zed.transform-state))
  (:use #:cl))

(in-package #:%zed.game-object)

(deftype pause-mode () '(member :pause :ignore :inherit))

(declaim (inline make-game-object))
(defstruct (game-object
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
  (transform (trs::make-state) :type trs::state)
  ;; A list of traits attached to this game object.
  (traits nil :type list))

(u:define-printer (game-object stream :type nil)
  (format stream "GAME-OBJECT: ~a" (path game-object)))

(defun make-root ()
  (make-game-object :label "[ROOT]" :path "/" :root-p t :depth 0 :pause-mode :pause))
