(asdf:defsystem #:zed
  :description "Experimental game engine"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://git.mfiano.net/mfiano/zed.git")
  :encoding :utf-8
  :depends-on (#:cl-cpus
               #:cl-opengl
               #:closer-mop
               #:golden-utils
               #:lparallel
               #:origin
               #:sdl2
               #:shadow
               #:trivial-garbage
               #:umbra)
  :pathname "src"
  :serial t
  :components
  ((:module "base"
    :components
    ((:file "debug")
     (:file "config")
     (:file "thread-pool")
     (:file "clock")
     (:file "live-coding")
     (:file "ordered-class")))
   (:module "render"
    :components
    ((:file "gl-context")
     (:file "monitor")
     (:file "window")
     (:file "shader-program")))
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
     (:file "jobs")))
   (:module "core"
    :components
    ((:file "context")
     (:file "transform")
     (:file "tree")
     (:file "trait")
     (:file "game-loop")))
   (:module "trait")
   (:module "protocol"
    :components
    ((:file "input")
     (:file "game-object")
     (:file "core")))))
