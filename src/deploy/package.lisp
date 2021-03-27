(in-package #:cl-user)

(defpackage #:zed-deploy
  ;; Third-party aliases
  (:local-nicknames
   (#:log #:verbose))
  ;; Internal aliases
  (:local-nicknames
   (#:pack #:%zed.pack)
   (#:util #:%zed.util)
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:deploy))
