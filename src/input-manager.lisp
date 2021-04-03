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
