(in-package #:zed)

(defstruct (uniform
            (:predicate nil)
            (:copier nil))
  (program nil :type (or shadow:program null))
  (key nil :type symbol)
  (type nil :type (or symbol cons))
  (resolved-type nil :type symbol)
  (value nil :type t)
  (func nil :type (or function null)))

(u:define-printer (uniform stream :type nil)
  (format stream "UNIFORM: ~s ~s"
          (uniform-resolved-type uniform)
          (uniform-value uniform)))

(defstruct (draw-order-manager
            (:constructor %make-draw-order-manager)
            (:predicate nil)
            (:copier nil))
  (tree nil :type util.rb::tree)
  (table (u:dict #'eq) :type hash-table))

(u:define-printer (draw-order-manager stream :type nil)
  (format stream "DRAW-ORDER-MANAGER"))

(defstruct (trait-manager
            (:predicate nil)
            (:copier nil))
  (order nil :type list)
  (registered (make-array 32 :fill-pointer 0 :adjustable t) :type (vector trait))
  (unregistered (make-array 32 :fill-pointer 0 :adjustable t) :type (vector trait))
  (active-by-type (u:dict #'eq) :type hash-table)
  (active-by-id (u:dict #'eq) :type hash-table))

(u:define-printer (trait-manager stream :type nil)
  (format stream "TRAIT-MANAGER"))

(glob:define-global-var =core= nil)

(defstruct (core
            (:constructor %make-core)
            (:predicate nil)
            (:copier nil))
  (running-p nil :type boolean)
  (window nil :type window)
  (clock nil :type clock)
  (input-manager nil :type input-manager)
  (shader-manager (make-shader-manager) :type shader-manager)
  (scene-tree (make-root-game-object) :type game-object)
  (trait-manager (make-trait-manager) :type trait-manager)
  (resource-cache (u:dict #'eq) :type hash-table)
  (framebuffers (u:dict #'eq) :type hash-table)
  (materials (u:dict #'eq) :type hash-table)
  (prefabs (u:dict #'eq) :type hash-table)
  (viewports nil :type viewport-manager)
  (draw-order nil :type (or draw-order-manager null))
  (collision-system nil :type collision-system)
  (active-camera nil))

(u:define-printer (core stream :type nil :identity t)
  (format stream "CORE"))
