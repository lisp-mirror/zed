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
               #:closer-mop
               #:defpackage-plus
               #:fast-io
               #:global-vars
               #:golden-utils
               #:jsown
               #:lparallel
               #:pngload
               #:sdl2
               #:sdl2-mixer
               #:shadow
               #:split-sequence
               #:static-vectors
               #:trivial-garbage
               #:trivial-gray-streams
               #:uiop
               #:verbose)
  :pathname "src"
  :serial t
  :components
  ((:file "package-util")
   (:file "package-math")
   (:file "package-core")
   (:file "package-trait")
   (:file "math-common")
   (:file "math-constants")
   (:file "math-vector2")
   (:file "math-vector3")
   (:file "math-vector4")
   (:file "math-matrix2")
   (:file "math-matrix3")
   (:file "math-matrix4")
   (:file "math-quaternion")
   (:file "math-point2d")
   (:file "math-point3d")
   (:file "shader-library-swizzle")
   (:file "shader-library-common")
   (:file "shader-library-color")
   (:file "shader-library-sprite")
   (:file "shader-library-collider")
   (:file "binary-parser")
   (:file "ordered-class")
   (:file "red-black-tree")
   (:file "slice-stream")
   (:file "early-types")
   (:file "common")
   (:file "config")
   (:file "logging")
   (:file "thread-pool")
   (:file "pack")
   (:file "asset")
   (:file "image")
   (:file "image-png")
   (:file "image-hdr")
   (:file "gltf")
   (:file "whitelist")
   (:file "clock")
   (:file "live-coding")
   (:file "opengl")
   (:file "monitor")
   (:file "window")
   (:file "audio")
   (:file "shader-manager")
   (:file "viewport-data")
   (:file "viewport")
   (:file "viewport-manager")
   (:file "input-transition")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input")
   (:file "geometry-attribute")
   (:file "geometry-group")
   (:file "geometry-layout")
   (:file "geometry")
   (:file "transform-state")
   (:file "game-object")
   (:file "jobs")
   (:file "context")
   (:file "trait")
   (:file "collision-plan")
   (:file "collision-state")
   (:file "collision-system")
   (:file "collision-volume-box")
   (:file "collision-volume-sphere")
   (:file "draw-order")
   (:file "resource-cache")
   (:file "camera-state")
   (:file "transform")
   (:file "spritesheet")
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
   (:file "material")
   (:file "uniform")
   (:file "trait-camera")
   (:file "trait-geometry")
   (:file "trait-render")
   (:file "trait-mesh")
   (:file "trait-sprite")
   (:file "trait-collider")
   (:file "game-loop")
   (:file "core")))
