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

(defpackage #:%zed.utility.bezier-curve
  (:local-nicknames
   (#:m4 #:zed.math.matrix4)
   (#:p3 #:zed.math.point3d)
   (#:u #:golden-utils)
   (#:v3 #:zed.math.vector3)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  (:export
   #:add-points
   #:collect-points
   #:collect-segments
   #:curve
   #:edit-point
   #:evaluate
   #:make-curve
   #:point-count-valid-p
   #:point-index-present-p))
