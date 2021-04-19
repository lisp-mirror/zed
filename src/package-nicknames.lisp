(in-package #:cl-user)

(defpackage #:%zed.nicknames
  (:import-from
   #+allegro #:excl
   #+ccl #:ccl
   #+lispworks #:hcl
   #+sbcl #:sb-ext
   #+(or abcl clasp ecl) #:ext
   #:add-package-local-nickname)
  (:use #:cl))

(in-package #:%zed.nicknames)

(defmacro add (nickname package local-package)
  `(add-package-local-nickname ',nickname ',package ',local-package))

(add #:tr.cam #:zed.trait.camera #:zed)
(add #:tr.col #:zed.trait.collider #:zed)
(add #:tr.ren #:zed.trait.render #:zed)
