(in-package #:cl-user)

(defpackage #:%zed.utility.binary-parser
  (:local-nicknames
   (#:io #:fast-io))
  (:use #:cl))

(defpackage #:%zed.utility.ordered-class
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(defpackage #:%zed.utility.red-black-tree
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:tree))

(defpackage #:%zed.utility.stream-slice
  (:local-nicknames
   (#:tgs #:trivial-gray-streams))
  (:use #:cl)
  (:shadow
   #:length
   #:stream))
