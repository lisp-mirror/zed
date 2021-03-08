(in-package #:cl-user)

(defpackage #:%zed.base.config
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.base.config)

(defstruct (config
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (window-width 1280 :type u:ub16)
  (window-height 720 :type u:ub16)
  (window-title "" :type string)
  (anti-alias-p t :type boolean)
  (delta-time nil :type (or real null)))
