(in-package #:cl-user)

(defpackage #:%zed.prefab.factory
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:pf.def #:%zed.prefab.definitions)
   (#:tr #:%zed.trait)
   (#:tree #:%zed.tree))
  (:use #:cl))

(in-package #:%zed.prefab.factory)

(defun realize-game-object (context node root)
  (let* ((factory (pf.def::factory (pf.def::prefab node)))
         (game-objects (pf.def::factory-game-objects factory))
         (game-object (u:href game-objects (pf.def::path node))))
    (u:do-hash (type args (pf.def::trait-thunked-args node))
      (loop :for (k v) :on (u:hash->plist args) :by #'cddr
            :collect k :into args
            :collect (let ((arg (funcall v)))
                       (if (typep arg 'pf.def::reference)
                           (funcall (pf.def::reference-func arg) factory)
                           arg))
              :into args
            :finally (let ((trait (apply #'tr::make-trait context type args)))
                       (tr::attach-trait game-object trait))))
    (tree::insert context
                  game-object
                  (u:if-let ((parent (pf.def::parent node)))
                    (u:href game-objects (pf.def::path parent))
                    (or root (ctx::scene-tree context))))
    game-object))

(defun register-root (context prefab)
  (let* ((factory (pf.def::factory prefab))
         (root (u:href (pf.def::factory-game-objects factory)
                       (pf.def::path (pf.def::root prefab))))
         (prefab-name (pf.def::name prefab)))
    (push root (u:href (ctx::prefabs context) prefab-name))
    (setf (gob::prefab-name root) prefab-name)
    root))

(defun make-func (prefab)
  (lambda (context &key parent)
    (let ((factory (pf.def::factory prefab))
          (nodes (pf.def::nodes prefab)))
      (u:do-hash-keys (k nodes)
        (let* ((label (format nil "~(~a~)" (first (last k))))
               (game-object (gob::make-game-object :label label)))
          (setf (u:href (pf.def::factory-game-objects factory) k) game-object)))
      (u:do-hash-values (node nodes)
        (setf (pf.def::factory-current-node factory) node)
        (realize-game-object context node parent))
      (setf (pf.def::factory-current-node factory) nil)
      (register-root context prefab))))

(defun build (prefab)
  (let ((factory (pf.def::factory prefab)))
    (setf (pf.def::factory-func factory) (make-func prefab))))
