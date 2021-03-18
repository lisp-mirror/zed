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
  (:import-from
   #:%zed.tree
   #:pause-game
   #:unpause-game)
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
  (:export
   #:game-object-enabled-p
   #:game-object-paused-p
   #:insert-game-object
   #:make-game-object
   #:pause-game-object
   #:reparent-game-object
   #:unpause-game-object))