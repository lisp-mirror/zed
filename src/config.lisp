(in-package #:cl-user)

(defpackage #:%zed.config
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.config)

(defstruct (config
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (profile-p nil :type boolean)
  (frame-count nil :type (or (and (integer 1) fixnum) boolean))
  (prelude (constantly nil) :type (or function symbol))
  (window-width 1280 :type u:ub16)
  (window-height 720 :type u:ub16)
  (window-title "" :type string)
  (anti-alias-p t :type boolean)
  (delta-time nil :type (or real null)))
