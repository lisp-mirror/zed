(asdf:defsystem #:zed
  :description "Experimental game engine"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://git.mfiano.net/mfiano/zed.git")
  :encoding :utf-8
  :depends-on (#:babel
               #:cffi
               #:cl-cpus
               #:cl-opengl
               #:cl-slug
               #:closer-mop
               #:global-vars
               #:golden-utils
               #:lparallel
               #:origin
               #:pngload
               #:sdl2
               #:shadow
               #:split-sequence
               #:trivial-garbage
               #:uiop
               #:umbra)
  :pathname "src"
  :serial t
  :components
  ((:file "debug")
   (:file "ordered-class")
   (:file "config")
   (:file "thread-pool")
   (:file "clock")
   (:file "live-coding")
   (:file "opengl")
   (:file "monitor")
   (:file "window")
   (:file "shader-program")
   (:file "shader-buffer-state")
   (:file "input-manager")
   (:file "input-transition")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input")
   (:file "transform-state")
   (:file "game-object")
   (:file "jobs")
   (:file "context")
   (:file "asset-pool")
   (:file "image")
   (:file "image-png")
   (:file "image-hdr")
   (:file "camera-state")
   (:file "transform")
   (:file "tree")
   (:file "spritesheet")
   (:file "trait")
   (:file "prefab-definitions")
   (:file "prefab-factory")
   (:file "prefab")
   (:file "texture-data")
   (:file "texture")
   (:file "texture-2d")
   (:file "texture-2d-array")
   (:file "texture-cube-map")
   (:file "texture-cube-map-array")
   (:file "framebuffer-data")
   (:file "framebuffer")
   (:file "material-data")
   (:file "material-definition")
   (:file "material-uniform")
   (:file "material")
   (:file "trait-render")
   (:file "trait-camera")
   (:file "trait-sprite")
   (:file "game-loop")
   (:module "interface"
    :components
    ((:file "package")
     (:file "core")
     (:file "input")
     (:file "game-object")))))
