(in-package #:cl-user)

(defpackage #:%zed.recompilation-hooks
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:dbg #:%zed.debug)
   (#:gob #:%zed.game-object)
   (#:live #:%zed.live-coding)
   (#:mat #:%zed.material)
   (#:mat.data #:%zed.material.data)
   (#:pf #:%zed.prefab)
   (#:rc #:%zed.resource-cache)
   (#:tex #:%zed.texture)
   (#:tr #:%zed.transform)
   (#:tree #:%zed.tree)
   (#:uni #:%zed.material.uniform)
   (#:vp #:%zed.viewport)
   (#:vp.mgr #:%zed.viewport.manager)
   (#:wl #:%zed.whitelist))
  (:use #:cl))

(in-package #:%zed.recompilation-hooks)

(defmethod live::recompile ((type (eql :shader)) data)
  (shadow:recompile-shaders data)
  (dolist (x data)
    (format t "Recompiled shader: ~s.~%" x)))

(defmethod live::recompile ((type (eql :texture)) data)
  (u:when-let ((texture (rc::find dbg::=context= :texture data)))
    (gl:delete-texture (tex::id texture))
    (rc::delete dbg::=context= :texture data)
    (tex::load dbg::=context= data :width (tex::width texture) :height (tex::height texture))
    (dolist (material-name (tex::materials texture))
      (live::recompile :material material-name))
    (format t "Recompiled texture: ~s~%" data)))

(defmethod live::recompile ((type (eql :material)) data)
  (u:when-let ((shader (mat.data::shader (mat.data::find data))))
    (live::recompile :shader (list shader)))
  (u:when-let ((material (u:href (ctx::materials dbg::=context=) data)))
    (uni::make-uniforms dbg::=context= material)
    (mat::ensure-framebuffer dbg::=context= material))
  (format t "Recompiled material: ~s~%" data))

(defmethod live::recompile ((type (eql :prefab)) data)
  (wl::with-scope (:prefab-recompile)
    (dolist (game-object (u:href (ctx::prefabs dbg::=context=) data))
      (let* ((parent (gob::parent game-object))
             (translation (tr::get-translation game-object))
             (new-game-object (pf::load-prefab dbg::=context= data :parent parent)))
        (tr::translate new-game-object translation :replace-p t)
        (tree:destroy-game-object dbg::=context= game-object)))
    (format t "Recompiled prefab: ~s.~%" data)))

(defmethod live::recompile ((type (eql :viewport)) data)
  (u:when-let ((viewport (vp.mgr::find (ctx::viewports dbg::=context=) data)))
    (vp::update viewport)))
