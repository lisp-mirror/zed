(in-package #:cl-user)

(defpackage #:zed-deploy
  ;; Internal aliases
  (:local-nicknames
   (#:log #:%zed.logging)
   (#:pack #:%zed.pack)
   (#:util #:%zed.util)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:deploy))
