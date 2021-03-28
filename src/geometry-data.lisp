(in-package #:cl-user)

(defpackage #:%zed.geometry.data
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:geo.layout.data #:%zed.geometry.layout.data))
  (:use #:cl))

(in-package #:%zed.geometry.data)

(glob:define-global-var =data= (u:dict #'eq))

(defstruct (data
            (:constructor %make-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (id 0 :type u:ub32)
  (layout (geo.layout.data::%make-data) :type geo.layout.data::data)
  (buffers (vector) :type vector)
  (buffer-names (u:dict #'eq) :type hash-table)
  (primitive :triangles :type keyword)
  (vertex-count 0 :type u:ub32))
