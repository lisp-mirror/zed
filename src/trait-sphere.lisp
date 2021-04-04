(in-package #:zed.trait.sphere)

(z::define-internal-trait sphere ()
  ((%visible-p :reader visible-p
               :inline t
               :type boolean
               :initarg :visible-p
               :initform t)))
