(in-package #:cl-user)

(defpackage #:%zed.render
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:dbg #:%zed.debug)
   (#:gob #:%zed.game-object)
   (#:mat.data #:%zed.material.data)
   (#:mat.def #:%zed.material.definition)
   (#:trait #:%zed.trait)
   (#:tr.ren #:zed.trait.render)
   (#:tree #:%zed.tree))
  (:use #:cl))

(in-package #:%zed.render)

(u:fn-> render-game-object (gob::game-object) null)
(defun render-game-object (game-object)
  (declare (optimize speed))
  (let ((render-trait (trait::find game-object 'tr.ren::render)))
    (dbg::with-debug-group (format nil "Game Object: ~a" (gob::label game-object))
      (let ((material (tr.ren::current-material render-trait)))
        (funcall (mat.data::render-func (mat.def::data material)) render-trait)))
    nil))

(u:fn-> render-frame (ctx::context) null)
(defun render-frame (context)
  (declare (optimize speed))
  (tree::walk-tree (x (ctx::scene-tree context))
    (render-game-object x)
    nil))
