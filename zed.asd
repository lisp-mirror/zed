(asdf:defsystem #:zed
  :description "Experimental game engine"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://git.mfiano.net/mfiano/zed.git")
  :encoding :utf-8
  :depends-on (#:cl-opengl
               #:golden-utils
               #:origin
               #:sdl2
               #:trivial-garbage)
  :pathname "src"
  :serial t
  :components
  ((:module "base"
    :components
    ((:file "debug")
     (:file "clock")
     (:file "live-coding")
     (:file "config")))
   (:module "render-backend"
    :components
    ((:file "gl-context")
     (:file "monitor")
     (:file "window")))
   (:module "input"
    :components
    ((:file "manager")
     (:file "transition")
     (:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "input")))
   (:module "game-object"
    :components
    ((:file "transform-state")
     (:file "game-object")
     (:file "transform")
     (:file "tree")))
   (:module "core"
    :components
    ((:file "context")
     (:file "game-loop")))
   (:module "protocol"
    :components
    ((:file "input")
     (:file "game-object")
     (:file "core")))))
