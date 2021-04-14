(in-package #:cl-user)

(defpackage #:zed
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:io #:fast-io)
   (#:lp #:lparallel)
   (#:lpq #:lparallel.queue)
   (#:ss #:split-sequence)
   (#:sv #:static-vectors)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:const #:zed.math.constants)
   (#:frustum #:zed.math.frustum)
   (#:line3d #:zed.math.line3d)
   (#:m3 #:zed.math.matrix3)
   (#:m4 #:zed.math.matrix4)
   (#:p3 #:zed.math.point3d)
   (#:q #:zed.math.quaternion)
   (#:util.bin #:%zed.utility.binary-parser)
   (#:util.oc #:%zed.utility.ordered-class)
   (#:util.rb #:%zed.utility.red-black-tree)
   (#:util.ss #:%zed.utility.stream-slice)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  ;; Core
  (:export
   #:context
   #:pause-game
   #:start-game
   #:stop-game
   #:unpause-game)
  ;; Game object
  (:export
   #:destroy-game-object
   #:game-object
   #:game-object-enabled-p
   #:game-object-paused-p
   #:make-game-object
   #:on-game-object-picked
   #:pause-game-object
   #:reparent-game-object
   #:spawn-game-object
   #:unpause-game-object)
  ;; Trait
  (:export
   #:attach-trait
   #:define-trait
   #:detach-all-traits
   #:detach-trait
   #:detach-trait-type
   #:find-trait
   #:make-trait
   #:trait
   #:trait-context)
  ;; Prefab
  (:export
   #:define-prefab
   #:load-prefab
   #:ref)
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
  ;; DSLs
  (:export
   #:define-collision-hook
   #:define-collision-plan
   #:define-framebuffer
   #:define-geometry
   #:define-geometry-layout
   #:define-material
   #:define-texture))
