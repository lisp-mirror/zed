(in-package #:zed)

(defstruct (draw-order-manager
            (:constructor %make-draw-order-manager)
            (:predicate nil)
            (:copier nil))
  (tree nil :type util.rb::tree)
  (table (u:dict #'eq) :type hash-table))

(u:define-printer (draw-order-manager stream :type nil)
  (format stream "DRAW-ORDER-MANAGER"))

(u:fn-> make-draw-order-manager (function) draw-order-manager)
(defun make-draw-order-manager (sort-func)
  (declare (optimize speed))
  (%make-draw-order-manager :tree (util.rb::make-tree :sort-func sort-func)))

(u:fn-> register-draw-order (draw-order-manager trait) null)
(defun register-draw-order (manager render-trait)
  (declare (optimize speed))
  (let ((game-object (trait-owner render-trait))
        (node (util.rb::insert (draw-order-manager-tree manager) render-trait)))
    (setf (u:href (draw-order-manager-table manager) game-object) node)
    nil))

(u:fn-> deregister-draw-order (draw-order-manager trait) null)
(defun deregister-draw-order (manager render-trait)
  (declare (optimize speed))
  (let ((tree (draw-order-manager-tree manager))
        (table (draw-order-manager-table manager))
        (game-object (trait-owner render-trait)))
    (util.rb::delete-node tree (u:href table game-object))
    (remhash game-object table)
    nil))

;; Sort the draw call for the given game object by removing its associated node from the draw order
;; manager's tree, then inserting a new node with the same render trait. This is called whenever a
;; game object is inserted or moved around in the scene tree, because it then has a new depth, which
;; is the secondary sorting criterium we sort draw calls by.
(u:fn-> resort-draw-order-object (draw-order-manager game-object) null)
(defun resort-draw-order-object (manager game-object)
  (declare (optimize speed))
  (u:when-let* ((table (draw-order-manager-table manager))
                (node (u:href table game-object))
                (tree (draw-order-manager-tree manager))
                (value (util.rb::value node)))
    (util.rb::delete-node tree node)
    (setf (u:href table game-object) (util.rb::insert tree value))
    nil))

(u:fn-> resort-draw-order (core draw-order-manager) null)
(defun resort-draw-order (core manager)
  (declare (optimize speed))
  (u:when-let ((dirty-objects (core-draw-order-dirty-objects core)))
    (dolist (game-object dirty-objects)
      (resort-draw-order-object manager game-object))
    nil))

(u:fn-> map-draw-order (core draw-order-manager function) null)
(declaim (inline map-draw-order))
(defun map-draw-order (core manager func)
  (declare (optimize speed))
  (resort-draw-order core manager)
  (util.rb::walk (draw-order-manager-tree manager) func))
