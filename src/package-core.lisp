(in-package #:cl-user)

(defpackage #:zed
  ;; Third-party aliases
  (:local-nicknames
   (#:font #:3b-bmfont)
   (#:glob #:global-vars)
   (#:io #:fast-io)
   (#:lp #:lparallel)
   (#:lpq #:lparallel.queue)
   (#:ss #:split-sequence)
   (#:sv #:static-vectors)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:aabb #:zed.math.aabb)
   (#:const #:zed.math.constants)
   (#:curve #:%zed.utility.bezier-curve)
   (#:easing #:zed.math.easing)
   (#:frustum #:zed.math.frustum)
   (#:geo.test #:zed.math.primitive-test)
   (#:m3 #:zed.math.matrix3)
   (#:m4 #:zed.math.matrix4)
   (#:obb #:zed.math.obb)
   (#:p3 #:zed.math.point3d)
   (#:q #:zed.math.quaternion)
   (#:ray #:zed.math.ray)
   (#:raycast #:zed.math.raycast)
   (#:sphere #:zed.math.sphere)
   (#:util.bin #:%zed.utility.binary-parser)
   (#:util.dll #:%zed.utility.doubly-linked-list)
   (#:util.oc #:%zed.utility.ordered-class)
   (#:util.rb #:%zed.utility.red-black-tree)
   (#:util.ss #:%zed.utility.stream-slice)
   (#:v2 #:zed.math.vector2)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4)
   (#:zsl #:zed.shader-library))
  (:use #:cl)
  ;; Core
  (:export
   #:core
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
   #:trait-core
   #:trait-owner)
  ;; Transform
  (:export
   #:get-rotation
   #:get-scale
   #:get-translation
   #:rotate
   #:rotate/velocity
   #:scale
   #:scale/velocity
   #:transform-direction
   #:transform-direction!
   #:transform-point
   #:transform-point!
   #:transform-vector
   #:transform-vector!
   #:translate
   #:translate/clamp
   #:translate/velocity)
  ;; Prefab
  (:export
   #:load-prefab
   #:prefab-reference)
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
  ;; Animate trait
  (:export
   #:enqueue-animation)
  ;; DSLs
  (:export
   #:define-animate-hook
   #:define-collision-hook
   #:define-collision-plan
   #:define-context
   #:define-curve
   #:define-framebuffer
   #:define-geometry
   #:define-geometry-layout
   #:define-material
   #:define-prefab
   #:define-texture
   #:define-viewport))
