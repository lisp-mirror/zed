(in-package #:cl-user)

(defpackage #:%zed.utility.binary-parser
  (:local-nicknames
   (#:io #:fast-io))
  (:use #:cl))

(defpackage #:%zed.utility.octree
  (:local-nicknames
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:shadow
   #:count
   #:search))

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
