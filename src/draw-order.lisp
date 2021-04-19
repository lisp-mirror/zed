(in-package #:zed)

(u:fn-> make-draw-order-manager (function) draw-order-manager)
(defun make-draw-order-manager (sort-func)
  (declare (optimize speed))
  (%make-draw-order-manager :tree (util.rb::make-tree :sort-func sort-func)))

(u:fn-> register-draw-order (core trait) null)
(defun register-draw-order (core render-trait)
  (declare (optimize speed))
  (let* ((manager (core-draw-order core))
         (game-object (trait-owner render-trait))
         (node (util.rb::insert (draw-order-manager-tree manager) render-trait)))
    (setf (u:href (draw-order-manager-table manager) game-object) node)
    nil))

(u:fn-> deregister-draw-order (core trait) null)
(defun deregister-draw-order (core render-trait)
  (declare (optimize speed))
  (let* ((manager (core-draw-order core))
         (tree (draw-order-manager-tree manager))
         (table (draw-order-manager-table manager))
         (game-object (trait-owner render-trait)))
    (util.rb::delete-node tree (u:href table game-object))
    (remhash game-object table)
    nil))

;; Sort the draw call for the given game object by removing its associated node from the draw order
;; manager's tree, then inserting a new node with the same render trait. This is called whenever a
;; game object is inserted or moved around in the scene tree, because it then has a new depth, which
;; is the secondary sorting criterium we sort draw calls by.
(u:fn-> resort-draw-order (core game-object) null)
(defun resort-draw-order (core game-object)
  (declare (optimize speed))
  (u:when-let* ((manager (core-draw-order core))
                (node (u:href (draw-order-manager-table manager) game-object))
                (tree (draw-order-manager-tree manager))
                (value (util.rb::value node)))
    (util.rb::delete-node tree node)
    (setf (u:href (draw-order-manager-table manager) game-object)
          (util.rb::insert tree value))
    nil))

(u:fn-> map-draw-order (draw-order-manager function) null)
(declaim (inline map-draw-order))
(defun map-draw-order (manager func)
  (declare (optimize speed))
  (util.rb::walk (draw-order-manager-tree manager) func))
