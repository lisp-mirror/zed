(in-package #:zed)

(defstruct (input-manager
            (:constructor %make-input-manager)
            (:predicate nil)
            (:copier nil))
  (event (sdl2:new-event) :type sdl2-ffi:sdl-event)
  (gamepad-instances (u:dict #'eq) :type hash-table)
  (gamepad-ids (u:dict #'eq) :type hash-table)
  (detached-gamepads nil :type list)
  (entering (u:dict #'eq) :type hash-table)
  (exiting (u:dict #'eq) :type hash-table)
  (states (u:dict #'equal) :type hash-table))

(u:define-printer (input-manager stream :type nil)
  (format stream "INPUT-MANAGER"))

(deftype collision-volume-type () '(member :box :sphere))

(defstruct (collision-state
            (:predicate nil)
            (:copier nil))
  (contact-count 0 :type fixnum)
  (hit-p nil :type boolean))

(defstruct (collision-volume
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (type :box :type collision-volume-type)
  (collider nil :type trait)
  (mesh-name "" :type string)
  (center (v3:zero) :type v3:vec)
  (update-func (constantly nil) :type function)
  (update-visualization-func (constantly nil) :type function))

(u:define-printer (collision-volume stream :type nil)
  (format stream "COLLISION-VOLUME: ~s" (collision-volume-type collision-volume)))

(defstruct (prefab-node
            (:constructor %make-prefab-node)
            (:predicate nil)
            (:copier nil))
  (prefab nil :type (or prefab null))
  (path nil :type list)
  (parent nil :type (or prefab-node null))
  (options (u:dict #'eq) :type hash-table)
  (template nil :type (or prefab-node null))
  (trait-options (u:dict #'eq) :type hash-table)
  (trait-args (u:dict #'eq) :type hash-table)
  (trait-thunked-args (u:dict #'eq) :type hash-table))

(u:define-printer (prefab-node stream :type nil)
  (format stream "PREFAB-NODE: ~{~a~^/~}" (prefab-node-path prefab-node)))

(defstruct (prefab-factory
            (:constructor %make-prefab-factory)
            (:predicate nil)
            (:copier nil))
  (prefab-name nil :type symbol)
  (current-node nil :type (or prefab-node null))
  (game-objects (u:dict #'equal) :type hash-table)
  (func (constantly nil) :type function))

(defstruct (prefab-reference
            (:constructor %make-prefab-reference)
            (:predicate nil)
            (:copier nil))
  (func (constantly nil) :type function))

(defstruct (prefab
            (:constructor %make-prefab)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (data nil :type list)
  (root nil :type (or prefab-node null))
  (nodes (u:dict #'equal) :type hash-table)
  (masters nil :type list)
  (slaves nil :type list)
  (factory (%make-prefab-factory) :type prefab-factory))

(u:define-printer (prefab stream :type nil)
  (format stream "PREFAB: ~s" (prefab-name prefab)))

(defstruct (uniform
            (:predicate nil)
            (:copier nil))
  (program nil :type (or shadow::program null))
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

(defstruct (jobs
            (:predicate nil)
            (:copier nil))
  (enable-traits nil :type list)
  (disable-traits nil :type list))

(u:define-printer (jobs stream :type nil)
  (format stream "JOBS"))

(glob:define-global-var =context= nil)

(defstruct (context
            (:constructor %make-context)
            (:predicate nil)
            (:copier nil))
  (running-p nil :type boolean)
  (window nil :type window)
  (clock nil :type clock)
  (input-manager nil :type input-manager)
  (shader-manager (make-shader-manager) :type shader-manager)
  (scene-tree (make-root-game-object) :type game-object)
  (jobs (make-jobs) :type jobs)
  (resource-cache (u:dict #'eq) :type hash-table)
  (framebuffers (u:dict #'eq) :type hash-table)
  (materials (u:dict #'eq) :type hash-table)
  (prefabs (u:dict #'eq) :type hash-table)
  (viewports nil :type viewport-manager)
  (draw-order nil :type (or draw-order-manager null))
  (active-camera nil)
  (collision-system nil :type collision-system))

(u:define-printer (context stream :type nil :identity t)
  (format stream "CONTEXT"))
