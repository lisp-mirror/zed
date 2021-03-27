(in-package #:cl-user)

(defpackage #:zed-deploy
  (:local-nicknames
   (#:pack #:%zed.pack)
   (#:util #:%zed.util)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:deploy))
