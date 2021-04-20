(push :cl-opengl-no-masked-traps *features*)
(push :cl-opengl-no-check-error *features*)

(asdf:defsystem #:zed
  :description "Experimental game engine"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://git.mfiano.net/mfiano/zed.git")
  :encoding :utf-8
  :depends-on (#:3b-bmfont
               #:3b-bmfont/json
               #:babel
               #:bordeaux-threads
               #:cffi
               #:cl-cpus
               #:cl-digraph
               #:cl-opengl
               #:closer-mop
               #:defpackage-plus
               #:fast-io
               #:global-vars
               #:golden-utils
               #:jsown
               #:lparallel
               #:pngload
               #:printv
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
  ((:file "math-package")
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
   (:file "math-line3d")
   (:file "math-easing")
   (:file "math-aabb")
   (:file "math-obb")
   (:file "math-sphere")
   (:file "math-ray")
   (:file "math-frustum")
   (:file "math-primitive-test")
   (:file "math-raycast")
   (:file "util-package")
   (:file "util-binary-parser")
   (:file "util-ordered-class")
   (:file "util-red-black-tree")
   (:file "util-slice-stream")
   (:file "util-bezier-curve")
   (:file "util-doubly-linked-list")
   (:file "shader-library-package")
   (:file "shader-library-swizzle")
   (:file "shader-library-common")
   (:file "shader-library-default")
   (:file "shader-library-color")
   (:file "shader-library-sprite")
   (:file "shader-library-collider")
   (:file "shader-library-font")
   (:file "package-core")
   (:file "package-trait")
   (:file "package-nicknames")
   (:file "types")
   (:file "common")
   (:file "config")
   (:file "logging")
   (:file "thread-pool")
   (:file "whitelist")
   (:file "clock")
   (:file "time-buffer")
   (:file "live-coding")
   (:file "opengl")
   (:file "monitor")
   (:file "window")
   (:file "audio")
   (:file "core")
   (:file "viewport-data")
   (:file "viewport")
   (:file "viewport-manager")
   (:file "pack")
   (:file "asset")
   (:file "image")
   (:file "image-png")
   (:file "image-hdr")
   (:file "gltf")
   (:file "font")
   (:file "curve-data")
   (:file "shader")
   (:file "resource-cache")
   (:file "spritesheet")
   (:file "input-manager")
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
   (:file "transform")
   (:file "trait-order")
   (:file "trait")
   (:file "trait-manager")
   (:file "prefab")
   (:file "prefab-printer")
   (:file "free-look-state")
   (:file "collision-plan")
   (:file "collision-volume")
   (:file "collision-hash-grid")
   (:file "collision-tests")
   (:file "collision-system")
   (:file "texture-data")
   (:file "texture")
   (:file "texture-2d")
   (:file "texture-2d-array")
   (:file "texture-cube-map")
   (:file "texture-cube-map-array")
   (:file "framebuffer-data")
   (:file "framebuffer")
   (:file "draw-order")
   (:file "material-data")
   (:file "material")
   (:file "uniform")
   (:file "trait-camera")
   (:file "trait-geometry")
   (:file "trait-render")
   (:file "trait-mesh")
   (:file "trait-sprite")
   (:file "trait-font")
   (:file "trait-collider")
   (:file "trait-turn-table")
   (:file "trait-curve")
   (:file "picking")
   (:file "game-loop")
   (:file "context")
   (:file "engine")))
