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
  ((:file "util")
   (:file "config")
   (:file "logging")
   (:file "ordered-class")
   (:file "slice-stream")
   (:file "red-black-tree")
   (:file "binary-parser")
   (:file "pack")
   (:file "math-common")
   (:file "math-constants")
   (:file "math-vector2")
   (:file "math-vector3")
   (:file "math-vector4")
   (:file "math-matrix2")
   (:file "math-matrix3")
   (:file "math-matrix4")
   (:file "math-point2d")
   (:file "math-point3d")
   (:file "math-quaternion")
   (:file "asset")
   (:file "whitelist")
   (:file "thread-pool")
   (:file "clock")
   (:file "live-coding")
   (:file "opengl")
   (:file "monitor")
   (:file "window")
   (:file "audio")
   (:file "shader-manager")
   (:file "shader-swizzle")
   (:file "shader-library-package")
   (:file "shader-library-common")
   (:file "shader-library-color")
   (:file "shader-library-sprite")
   (:file "shader-library-collider")
   (:file "viewport-data")
   (:file "viewport")
   (:file "viewport-manager")
   (:file "input-manager")
   (:file "input-transition")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input")
   (:file "transform-state")
   (:file "trait-manager")
   (:file "game-object")
   (:file "jobs")
   (:file "context")
   (:file "input-interface")
   (:file "trait")
   (:file "draw-order")
   (:file "resource-cache")
   (:file "image")
   (:file "image-png")
   (:file "image-hdr")
   (:file "mesh-gltf")
   (:file "camera-state")
   (:file "transform")
   (:file "tree")
   (:file "spritesheet")
   (:file "geometry-attribute")
   (:file "geometry-group")
   (:file "geometry-layout-data")
   (:file "geometry-data")
   (:file "geometry-buffer")
   (:file "geometry")
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
   (:file "recompilation-hooks")
   (:file "trait-camera")
   (:file "trait-geometry")
   (:file "trait-render")
   (:file "trait-mesh")
   (:file "collision-volume-struct")
   (:file "collision-volume-box")
   (:file "collision-volume-sphere")
   (:file "collision-volume")
   (:file "trait-collider")
   (:file "trait-sprite")
   (:file "game-loop")
   (:file "core")
   (:file "package")))
