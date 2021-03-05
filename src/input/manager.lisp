(in-package #:cl-user)

(defpackage #:%zed.input.manager
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.input.manager)

(defstruct (manager
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (gamepad-instances (u:dict #'eq) :type hash-table)
  (gamepad-ids (u:dict #'eq) :type hash-table)
  (detached-gamepads nil :type list)
  (entering (u:dict #'eq) :type hash-table)
  (exiting (u:dict #'eq) :type hash-table)
  (states (u:dict #'equal) :type hash-table))
