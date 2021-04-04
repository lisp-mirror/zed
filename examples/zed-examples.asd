(asdf:defsystem #:zed-examples
  :description "Examples for the Zed game engine."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://git.mfiano.net/mfiano/zed.git")
  :encoding :utf-8
  :depends-on (#:golden-utils
               #:zed)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "sprite")
   (:file "mesh-shader")
   (:file "mesh")
   (:file "geometry-shader")
   (:file "geometry")
   (:file "colliders")))
