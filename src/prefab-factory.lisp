(in-package #:zed)

(defun realize-prefab-node (context node root)
  (let* ((factory (prefab-factory (prefab-node-prefab node)))
         (game-objects (prefab-factory-game-objects factory))
         (game-object (u:href game-objects (prefab-node-path node))))
    (u:do-hash (type args (prefab-node-trait-thunked-args node))
      (loop :for (k v) :on (u:hash->plist args) :by #'cddr
            :collect k :into args
            :collect (let ((arg (funcall v)))
                       (if (typep arg 'prefab-reference)
                           (funcall (prefab-reference-func arg) factory)
                           arg))
              :into args
            :finally (let ((trait (apply #'make-trait context type args)))
                       (attach-trait game-object trait))))
    (spawn-game-object context
                       game-object
                       (u:if-let ((parent (prefab-node-parent node)))
                         (u:href game-objects (prefab-node-path parent))
                         root))
    game-object))

(defun register-prefab-root (context prefab)
  (let* ((factory (prefab-factory prefab))
         (root (u:href (prefab-factory-game-objects factory)
                       (prefab-node-path (prefab-root prefab))))
         (prefab-name (prefab-name prefab)))
    (push root (u:href (context-prefabs context) prefab-name))
    (setf (game-object-prefab-name root) prefab-name)
    root))

(defun initialize-prefab-node-transforms (game-object node)
  (let ((node-options (prefab-node-options node))
        (transform (game-object-transform game-object)))
    (initialize-translate-state transform
                                (u:href node-options :translate)
                                (u:href node-options :translate-velocity))
    (initialize-rotate-state transform
                             (u:href node-options :rotate)
                             (u:href node-options :rotate-velocity))
    (initialize-scale-state transform
                            (u:href node-options :scale)
                            (u:href node-options :scale-velocity))))

(defun make-prefab-factory-function (prefab)
  (lambda (context &key parent)
    (let ((factory (prefab-factory prefab))
          (nodes (prefab-nodes prefab)))
      (u:do-hash (path node nodes)
        (let* ((label (format nil "~(~a~)" (first (last path))))
               (game-object (make-game-object :label label)))
          (initialize-prefab-node-transforms game-object node)
          (setf (u:href (prefab-factory-game-objects factory) path) game-object)))
      (u:do-hash-values (node nodes)
        (setf (prefab-factory-current-node factory) node)
        (with-scope (:prefab-instantiate)
          (realize-prefab-node context node parent)))
      (setf (prefab-factory-current-node factory) nil)
      (register-prefab-root context prefab))))

(defun build-prefab (prefab)
  (let ((factory (prefab-factory prefab)))
    (setf (prefab-factory-func factory) (make-prefab-factory-function prefab))))
