(in-package #:cl-user)

(defpackage #:%zed.material.definition
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:fb #:%zed.framebuffer)
   (#:mat.data #:%zed.material.data))
  (:use #:cl))

(in-package #:%zed.material.definition)

(defstruct (material
            (:constructor %make-material)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (data nil :type mat.data::data)
  (uniforms (u:dict #'eq) :type hash-table)
  (framebuffer nil :type (or fb::framebuffer null))
  (attachments nil :type list)
  (texture-unit-state 0 :type u:ub8)
  (textures nil :type list))

(u:define-printer (material stream :type nil)
  (format stream "MATERIAL: ~s" (mat.data::name (data material))))
