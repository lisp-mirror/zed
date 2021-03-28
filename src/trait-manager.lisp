(in-package #:cl-user)

(defpackage #:%zed.trait.manager
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames)
  (:use #:cl))

(in-package #:%zed.trait.manager)

(defstruct (manager
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (by-id (u:dict #'eq) :type hash-table)
  (by-type (u:dict #'eq) :type hash-table)
  (order nil :type list))
