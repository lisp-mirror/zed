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
  ((:module "math"
    :components
    ((:file "package")
     (:file "common")
     (:file "constants")
     (:file "vector2")
     (:file "vector3")
     (:file "vector4")
     (:file "matrix2")
     (:file "matrix3")
     (:file "matrix4")
     (:file "quaternion")
     (:file "point2d")
     (:file "point3d")
     (:file "line3d")
     (:file "frustum")
     (:file "easing")))
   (:module "util"
    :components
    ((:file "package")
     (:file "binary-parser")
     (:file "ordered-class")
     (:file "red-black-tree")
     (:file "slice-stream")))
   (:module "shader-library"
    :components
    ((:file "package")
     (:file "swizzle")
     (:file "common")
     (:file "color")
     (:file "sprite")
     (:file "collider")
     (:file "font")))
   (:module "early"
    :components
    ((:file "package-core")
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
     (:file "context")
     (:file "viewport-data")
     (:file "viewport")
     (:file "viewport-manager")))
   (:module "asset"
    :components
    ((:file "pack")
     (:file "asset")
     (:file "image")
     (:file "image-png")
     (:file "image-hdr")
     (:file "gltf")
     (:file "font")
     (:file "shader")
     (:file "resource-cache")
     (:file "spritesheet")))
   (:module "input"
    :components
    ((:file "manager")
     (:file "transition")
     (:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "input")))
   (:module "geometry"
    :components
    ((:file "attribute")
     (:file "group")
     (:file "layout")
     (:file "geometry")))
   (:module "game-object"
    :components
    ((:file "transform-state")
     (:file "game-object")
     (:file "transform")
     (:file "trait-order")
     (:file "trait")
     (:file "trait-manager")
     (:file "prefab")
     (:file "prefab-printer")))
   (:module "collision-system"
    :components
    ((:file "plan")
     (:file "volume")
     (:file "volume-box")
     (:file "volume-sphere")
     (:file "hash-grid")
     (:file "tests")
     (:file "system")))
   (:module "texture"
    :components
    ((:file "data")
     (:file "texture")
     (:file "2d")
     (:file "2d-array")
     (:file "cube-map")
     (:file "cube-map-array")))
   (:module "render"
    :components
    ((:file "framebuffer-data")
     (:file "framebuffer")
     (:file "draw-order")
     (:file "material-data")
     (:file "material")
     (:file "uniform")))
   (:module "trait"
    :components
    ((:file "camera")
     (:file "geometry")
     (:file "render")
     (:file "mesh")
     (:file "sprite")
     (:file "font")
     (:file "collider")
     (:file "turn-table")))
   (:module "late"
    :components
    ((:file "picking")
     (:file "game-loop")
     (:file "core")))))
