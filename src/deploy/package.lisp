(in-package #:cl-user)

(defpackage #:zed-deploy
  ;; Internal aliases
  (:local-nicknames
   (#:z #:zed))
  (:use #:cl)
  (:export
   #:deploy))
