(in-package #:cl-user)

(defpackage #:zed-examples
  (:local-nicknames
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3)
   (#:z #:zed)
   (#:z.camera #:zed.trait.camera)
   (#:z.render #:zed.trait.render)
   (#:z.sprite #:zed.trait.sprite))
  (:use #:cl))
