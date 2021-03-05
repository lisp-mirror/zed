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
  ((:file "package")
   (:module "render-backend"
    :components
    ((:file "monitor")
     (:file "gl-context")
     (:file "window")))
   (:module "core"
    :components
    ((:file "clock")
     (:file "live-coding")
     (:file "config")
     (:file "context")
     (:file "game-loop")
     (:file "core")))))
