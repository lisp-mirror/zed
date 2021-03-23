(in-package #:cl-user)

(defpackage #:%zed.draw-order
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:rb #:%zed.red-black-tree)
   (#:trait #:%zed.trait))
  (:use #:cl)
  (:shadow
   #:map))

(in-package #:%zed.draw-order)

(defstruct (manager
            (:constructor %make-manager)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (tree nil :type rb::tree)
  (table (u:dict #'eq) :type hash-table))

(u:define-printer (manager stream :type nil)
  (format stream "DRAW-ORDER MANAGER"))

(u:fn-> manager-manager (function) manager)
(defun make-manager (sort-func)
  (declare (optimize speed))
  (%make-manager :tree (rb::make-tree :sort-func sort-func)))

(u:fn-> register (ctx::context trait:trait) null)
(defun register (context render-trait)
  (declare (optimize speed))
  (let* ((manager (ctx::draw-order context))
         (game-object (trait::owner render-trait))
         (node (rb::insert (tree manager) render-trait)))
    (setf (u:href (table manager) game-object) node)
    nil))

(u:fn-> deregister (ctx::context trait:trait) null)
(defun deregister (context render-trait)
  (declare (optimize speed))
  (let ((manager (ctx::draw-order context))
        (game-object (trait::owner render-trait)))
    (rb::delete (tree manager) render-trait)
    (remhash game-object (table manager))
    nil))

;; Sort the draw call for the given game object by removing its associated node from the draw order
;; manager's tree, then inserting a new node with the same render trait. This is called whenever a
;; game object is inserted or moved around in the scene tree, because it then has a new depth, which
;; is the secondary sorting criterium we sort draw calls by.
(u:fn-> resort (ctx::context gob::game-object) null)
(defun resort (context game-object)
  (declare (optimize speed))
  (u:when-let* ((manager (ctx::draw-order context))
                (node (u:href (table manager) game-object))
                (tree (tree manager))
                (value (rb::value node)))
    (rb::delete-node tree node)
    (setf (u:href (table manager) game-object) (rb::insert tree value))
    nil))

(u:fn-> map (manager function) null)
(declaim (inline map))
(defun map (manager func)
  (declare (optimize speed))
  (rb::walk (tree manager) func))
