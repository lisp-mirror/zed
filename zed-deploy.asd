(push :zed.release *features*)
(push :cl-opengl-no-masked-traps *features*)
(push :cl-opengl-no-check-error *features*)

(asdf:defsystem #:zed-deploy
  :description "Deployment of games created with the Zed game engine"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://git.mfiano.net/mfiano/zed.git")
  :encoding :utf-8
  :depends-on (#:zed)
  :pathname "src/deploy"
  :serial t
  :components
  ((:file "package")
   (:file "deploy")))
