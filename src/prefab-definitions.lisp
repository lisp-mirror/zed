(in-package #:cl-user)

(defpackage #:%zed.prefab.definitions
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:use #:cl))

(in-package #:%zed.prefab.definitions)

(defstruct (node
            (:constructor %make-node)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (prefab nil :type (or prefab null))
  (path nil :type list)
  (parent nil :type (or node null))
  (options (u:dict #'eq) :type hash-table)
  (template nil :type (or node null))
  (trait-options (u:dict #'eq) :type hash-table)
  (trait-args (u:dict #'eq) :type hash-table)
  (trait-thunked-args (u:dict #'eq) :type hash-table))

(u:define-printer (node stream :type nil)
  (format stream "PREFAB-NODE: ~{~a~^/~}" (path node)))

(defstruct (prefab
            (:constructor %make-prefab)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (data nil :type list)
  (root nil :type (or node null))
  (nodes (u:dict #'equal) :type hash-table)
  (masters nil :type list)
  (slaves nil :type list)
  (factory (%make-factory) :type factory))

(u:define-printer (prefab stream :type nil)
  (format stream "PREFAB: ~s" (name prefab)))

(defstruct (factory
            (:constructor %make-factory)
            (:predicate nil)
            (:copier nil))
  (prefab-name nil :type symbol)
  (current-node nil :type (or node null))
  (game-objects (u:dict #'equal) :type hash-table)
  (func (constantly nil) :type function))

(defstruct (reference
            (:constructor %make-reference)
            (:predicate nil)
            (:copier nil))
  (func (constantly nil) :type function))
