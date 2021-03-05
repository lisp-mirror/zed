(in-package #:cl-user)

(defpackage #:%zed.core.config
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.core.config)

(defstruct (config
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (window-width 1280 :type u:ub16)
  (window-height 720 :type u:ub16)
  (window-title "" :type string)
  (anti-alias-p t :type boolean)
  (delta-time nil :type (or u:ub8 null)))
