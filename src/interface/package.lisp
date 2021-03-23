(in-package #:cl-user)

(defpackage #:zed
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:cfg #:%zed.config)
   (#:ctx #:%zed.context)
   (#:dbg #:%zed.debug)
   (#:gob #:%zed.game-object)
   (#:in #:%zed.input)
   (#:in.mgr #:%zed.input.manager)
   (#:in.mouse #:%zed.input.mouse)
   (#:in.tr #:%zed.input.transition)
   (#:loop #:%zed.game-loop)
   (#:tree #:%zed.tree)
   (#:win #:%zed.window))
  (:use #:cl)

  ;; Core
  (:export
   #:pause-game
   #:start-game
   #:stop-game
   #:unpause-game)

  ;; Input
  (:export
   #:disable-relative-mouse-mode
   #:enable-relative-mouse-mode
   #:get-mouse-position
   #:get-mouse-scroll
   #:on-button-enabled
   #:on-button-enter
   #:on-button-exit
   #:on-window-event-enabled
   #:on-window-event-enter
   #:on-window-event-exit
   #:relative-mouse-mode-p)

  ;; Game object
  (:import-from
   #:%zed.tree
   #:pause-game-object
   #:unpause-game-object)
  (:export
   #:game-object-enabled-p
   #:game-object-paused-p
   #:insert-game-object
   #:make-game-object
   #:pause-game-object
   #:reparent-game-object
   #:unpause-game-object)

  ;; Trait
  (:import-from
   #:%zed.trait
   #:attach-trait
   #:detach-all-traits
   #:detach-trait
   #:detach-trait-type
   #:find-trait
   #:make-trait)
  (:export
   #:attach-trait
   #:detach-all-traits
   #:detach-trait
   #:detach-trait-type
   #:find-trait
   #:make-trait)

  ;; Prefab
  (:import-from
   #:%zed.prefab
   #:define-prefab
   #:load-prefab
   #:ref)
  (:export
   #:define-prefab
   #:load-prefab
   #:ref)

  ;; Asset pool
  (:import-from
   #:%zed.asset-pool
   #:define-asset-pool)
  (:export
   #:define-asset-pool)

  ;; Texture
  (:import-from
   #:%zed.texture.data
   #:define-texture)
  (:export
   #:define-texture)

  ;; Material
  (:import-from
   #:%zed.material
   #:define-material)
  (:export
   #:define-material)
  )

(uiop:define-package #:zed.shader
  (:use-reexport
   #:shadow.glsl
   #:umbra.common))
