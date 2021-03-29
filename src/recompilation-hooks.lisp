(in-package #:cl-user)

(defpackage #:%zed.recompilation-hooks
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:live #:%zed.live-coding)
   (#:log #:%zed.logging)
   (#:mat #:%zed.material)
   (#:mat.data #:%zed.material.data)
   (#:pf #:%zed.prefab)
   (#:rc #:%zed.resource-cache)
   (#:tex #:%zed.texture)
   (#:tr #:%zed.transform)
   (#:tree #:%zed.tree)
   (#:uni #:%zed.material.uniform)
   (#:util #:%zed.util)
   (#:vp #:%zed.viewport)
   (#:vp.mgr #:%zed.viewport.manager)
   (#:wl #:%zed.whitelist))
  (:use #:cl))

(in-package #:%zed.recompilation-hooks)

(defmethod live::recompile ((type (eql :shader)) data)
  (shadow:recompile-shaders data)
  (dolist (x data)
    (log::debug :zed.recompile "Recompiled shader: ~s" x)))

(defmethod live::recompile ((type (eql :texture)) data)
  (u:when-let ((texture (rc::find util::=context= :texture data)))
    (gl:delete-texture (tex::id texture))
    (rc::delete util::=context= :texture data)
    (tex::load util::=context= data :width (tex::width texture) :height (tex::height texture))
    (dolist (material-name (tex::materials texture))
      (live::recompile :material material-name))
    (log::debug :zed.recompile "Recompiled texture: ~s" data)))

(defmethod live::recompile ((type (eql :material)) data)
  (u:when-let ((shader (mat.data::shader (mat.data::find data))))
    (live::recompile :shader (list shader)))
  (u:when-let ((material (u:href (ctx::materials util::=context=) data)))
    (uni::make-uniforms util::=context= material)
    (mat::ensure-framebuffer util::=context= material))
  (log::debug :zed.recompile "Recompiled material: ~s" data))

(defmethod live::recompile ((type (eql :prefab)) data)
  (wl::with-scope (:prefab-recompile)
    (dolist (game-object (u:href (ctx::prefabs util::=context=) data))
      (tree:destroy-game-object util::=context= game-object)
      (let* ((parent (gob::parent game-object))
             (translation (tr::get-translation game-object))
             (new-game-object (pf::load-prefab util::=context= data :parent parent)))
        (tr::translate new-game-object translation :replace-p t)))
    (log::debug :zed.recompile "Recompiled prefab: ~s" data)))

(defmethod live::recompile ((type (eql :viewport)) data)
  (u:when-let ((viewport (vp.mgr::find (ctx::viewports util::=context=) data)))
    (vp::update viewport)
    (log::debug :zed.recompile "Recompiled viewport: ~s" data)))
