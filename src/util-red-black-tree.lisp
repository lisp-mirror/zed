(in-package #:%zed.utility.red-black-tree)

(defstruct (tree
            (:constructor %make-tree)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (sentinel nil :type (or node null))
  (root nil :type (or node null))
  (key-func #'identity :type function)
  (sort-func #'< :type function))

(u:define-printer (tree stream :type nil)
  (format stream "RED-BLACK-TREE"))

(declaim (inline %make-node))
(defstruct (node
            (:constructor %make-node)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (tree nil :type tree)
  (parent nil :type (or node null))
  (left nil :type (or node null))
  (right nil :type (or node null))
  (color :black :type (member :red :black))
  (key nil :type t)
  (value nil :type t))

(u:define-printer (node stream :type nil)
  (format stream "RED-BLACK-TREE-NODE"))

(u:fn-> make-node (tree t) node)
(defun make-node (tree item)
  (declare (optimize speed))
  (let ((node (%make-node :tree tree
                          :key (when item (funcall (key-func tree) item))
                          :value item)))
    (setf (parent node) node
          (left node) node
          (right node) node)
    node))

(u:fn-> make-tree (&key (:key-func function) (:sort-func function)) tree)
(defun make-tree (&key (key-func #'identity) (sort-func #'<))
  (declare (optimize speed))
  (let* ((tree (%make-tree :key-func key-func :sort-func sort-func))
         (sentinel (make-node tree nil)))
    (setf (sentinel tree) sentinel
          (root tree) sentinel)
    tree))

(u:fn-> node-p ((or node null)) (or node null))
(declaim (inline node-p))
(defun node-p (node)
  (declare (optimize speed))
  (unless (and node (eq node (sentinel (tree node))))
    node))

(u:fn-> walk (tree function) null)
(defun walk (tree func)
  (declare (optimize speed))
  (u:when-let ((node (node-p (root tree))))
    (loop :with current = node
          :with stack
          :do (cond
                ((node-p current)
                 (push current stack)
                 (setf current (left current)))
                (stack
                 (setf current (pop stack))
                 (funcall func (value current))
                 (setf current (right current)))
                (t (loop-finish))))))

(u:fn-> find (tree t) (values t &optional node))
(defun find (tree item)
  (declare (optimize speed))
  (labels ((%find (node key sort-func)
             (declare (function sort-func))
             (u:when-let ((result (and (node-p node) (key node))))
               (cond
                 ((funcall sort-func key result)
                  (%find (left node) key sort-func))
                 ((funcall sort-func result key)
                  (%find (right node) key sort-func))
                 (t node)))))
    (u:when-let ((node (%find (root tree)
                              (funcall (key-func tree) item)
                              (sort-func tree))))
      (values (value node)
              node))))

(u:fn-> rotate-left (tree node) null)
(defun rotate-left (tree node)
  (declare (optimize speed))
  (let ((right (right node)))
    (setf (right node) (left right))
    (when (node-p (left right))
      (setf (parent (left right)) node))
    (setf (parent right) (parent node))
    (cond
      ((not (node-p (parent node)))
       (setf (root tree) right))
      ((eq node (left (parent node)))
       (setf (left (parent node)) right))
      (t
       (setf (right (parent node)) right)))
    (setf (left right) node
          (parent node) right)
    nil))

(u:fn-> rotate-right (tree node) null)
(defun rotate-right (tree node)
  (declare (optimize speed))
  (let ((left (left node)))
    (setf (left node) (right left))
    (when (node-p (right left))
      (setf (parent (right left)) node))
    (setf (parent left) (parent node))
    (cond
      ((not (node-p (parent node)))
       (setf (root tree) left))
      ((eq node (right (parent node)))
       (setf (right (parent node)) left))
      (t
       (setf (left (parent node)) left)))
    (setf (right left) node
          (parent node) left)
    nil))

(u:fn-> insert-fixup (tree node) null)
(defun insert-fixup (tree node)
  (declare (optimize speed))
  (let ((node node))
    (u:while (eq (color (parent node)) :red)
      (if (eq (parent node) (left (parent (parent node))))
          (let ((y (right (parent (parent node)))))
            (cond
              ((eq (color y) :red)
               (setf (color (parent node)) :black
                     (color y) :black
                     (color (parent (parent node))) :red
                     node (parent (parent node))))
              (t
               (when (eq node (right (parent node)))
                 (setf node (parent node))
                 (rotate-left tree node))
               (setf (color (parent node)) :black
                     (color (parent (parent node))) :red)
               (rotate-right tree (parent (parent node))))))
          (let ((y (left
                    (parent (parent node)))))
            (cond
              ((eq (color y) :red)
               (setf (color (parent node)) :black
                     (color y) :black
                     (color (parent (parent node))) :red
                     node (parent (parent node))))
              (t
               (when (eq node (left (parent node)))
                 (setf node (parent node))
                 (rotate-right tree node))
               (setf (color (parent node)) :black
                     (color (parent (parent node))) :red)
               (rotate-left tree (parent (parent node)))))))))
  (setf (color (root tree)) :black)
  nil)

(u:fn-> insert (tree t) node)
(defun insert (tree item)
  (declare (optimize speed))
  (flet ((%insert (tree node)
           (let ((sort-func (sort-func tree))
                 (x (root tree))
                 (y (sentinel tree)))
             (u:while (node-p x)
               (setf y x)
               (if (funcall sort-func (key node) (key x))
                   (setf x (left x))
                   (setf x (right x))))
             (setf (parent node) y)
             (cond
               ((not (node-p y))
                (setf (root tree) node))
               ((funcall sort-func (key node) (key y))
                (setf (left y) node))
               (t
                (setf (right y) node)))
             (setf (left node) (sentinel tree)
                   (right node) (sentinel tree)
                   (color node) :red)
             (insert-fixup tree node))))
    (let ((node (make-node tree item)))
      (%insert tree node)
      node)))

(u:fn-> transplant (tree node node) null)
(defun transplant (tree u v)
  (declare (optimize speed))
  (cond
    ((not (node-p (parent u)))
     (setf (root tree) v))
    ((eq u (left (parent u)))
     (setf (left (parent u)) v))
    (t
     (setf (right (parent u)) v)))
  (setf (parent v) (parent u))
  nil)

(u:fn-> delete-fixup (tree node) null)
(defun delete-fixup (tree node)
  (declare (optimize speed))
  (let ((node node))
    (u:while (and (not (eq node (root tree)))
                  (eq (color node) :black))
      (if (eq node (left (parent node)))
          (let ((w (right (parent node))))
            (when (eq (color w) :red)
              (setf (color w) :black
                    (color (parent node)) :red)
              (rotate-left tree (parent node))
              (setf w (right (parent node))))
            (cond
              ((and (eq (color (left w)) :black)
                    (eq (color (right w)) :black))
               (setf (color w) :red
                     node (parent node)))
              (t
               (when (eq (color (right w)) :black)
                 (setf (color (left w)) :black
                       (color w) :red)
                 (rotate-right tree w)
                 (setf w (right (parent node))))
               (setf (color w) (color (parent node))
                     (color (parent node)) :black
                     (color (right w)) :black)
               (rotate-left tree (parent node))
               (setf node (root tree)))))
          (let ((w (left (parent node))))
            (when (eq (color w) :red)
              (setf (color w) :black
                    (color (parent node)) :red)
              (rotate-left tree (parent node))
              (setf w (left (parent node))))
            (cond
              ((and (eq (color (right w)) :black)
                    (eq (color (left w)) :black))
               (setf (color w) :red
                     node (parent node)))
              (t
               (when (eq (color (left w)) :black)
                 (setf (color (right w)) :black
                       (color w) :red)
                 (rotate-left tree w)
                 (setf w (left (parent node))))
               (setf (color w) (color (parent node))
                     (color (parent node)) :black
                     (color (left w)) :black)
               (rotate-right tree (parent node))
               (setf node (root tree))))))))
  (setf (color node) :black)
  nil)

(u:fn-> delete-node (tree node) null)
(defun delete-node (tree node)
  (declare (optimize speed))
  (flet ((minimum (x)
           (u:while (node-p (left x))
             (setf x (left x)))
           x))
    (let* ((x nil)
           (y node)
           (y-color (color y)))
      (cond
        ((not (node-p (left node)))
         (setf x (right node))
         (transplant tree node (right node)))
        ((not (node-p (right node)))
         (setf x (left node))
         (transplant tree node (left node)))
        (t
         (setf y (minimum (right node))
               y-color (color y)
               x (right y))
         (cond
           ((eq (parent y) node)
            (setf (parent x) y))
           (t
            (transplant tree y (right y))
            (setf (right y) (right node)
                  (parent (right y)) y)))
         (transplant tree node y)
         (setf (left y) (left node)
               (parent (left y)) y
               (color y) (color node))))
      (when (eq y-color :black)
        (delete-fixup tree x)))))
