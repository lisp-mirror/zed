(in-package #:zed)

(defstruct (gamepad
            (:predicate nil)
            (:copier nil))
  (id :gamepad1 :type keyword)
  instance
  name
  handle)

(defstruct (gamepad-analog-state
            (:predicate nil)
            (:copier nil))
  (x 0.0 :type u:f32)
  (y 0.0 :type u:f32)
  (deadzone 0.0 :type u:f32))

(u:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y) (:triggers :x)
      (:triggers :y))
  :test #'equalp)

(u:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button :left-shoulder
      :right-shoulder :up :down :left :right)
  :test #'equalp)

(u:fn-> get-gamepad-by-instance (input-manager u:b32) gamepad)
(declaim (inline get-gamepad-by-instance))
(defun get-gamepad-by-instance (manager instance)
  (declare (optimize speed))
  (u:href (input-manager-gamepad-instances manager) instance))

(u:fn-> generate-gamepad-id (input-manager) keyword)
(defun generate-gamepad-id (manager)
  (declare (optimize speed))
  (values
   (or (pop (input-manager-detached-gamepads manager))
       (u:format-symbol :keyword "GAMEPAD~d"
                        (1+ (hash-table-count (input-manager-gamepad-instances manager)))))))

(defun load-gamepad-database ()
  (let ((asset '(:zed "gamepad-db.txt")))
    (with-asset (asset path data :length-binding length)
      (sv:with-static-vector (sv length)
        (read-sequence sv data)
        (sdl2-ffi.functions:sdl-game-controller-add-mappings-from-rw
         (sdl2-ffi.functions:sdl-rw-from-mem (sv:static-vector-pointer sv) length)
         1)))))

(defun prepare-gamepads ()
  (sdl2:init* '(:gamecontroller))
  (load-gamepad-database)
  (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1"))

(u:fn-> shutdown-gamepads (input-manager) null)
(defun shutdown-gamepads (manager)
  (declare (optimize speed))
  (u:when-let ((instances (input-manager-gamepad-instances manager)))
    (u:do-hash-values (v instances)
      (sdl2:game-controller-close (gamepad-handle v)))
    (clrhash instances))
  nil)

(u:fn-> normalize-gamepad-analog-value (keyword keyword u:b16) u:f32)
(defun normalize-gamepad-analog-value (sub-device axis value)
  (declare (optimize speed))
  (if (eq sub-device :triggers)
      (values (u:map-domain 0 32767 0 1 value))
      (let ((clamped (u:clamp value -32768 32767)))
        (values
         (ecase axis
           (:x (u:map-domain -32768 32767 -1 1 clamped))
           (:y (u:map-domain -32768 32767 1 -1 clamped)))))))

(u:fn-> input-event/gamepad-attach (input-manager u:b32) null)
(defun input-event/gamepad-attach (manager index)
  (declare (optimize speed))
  (when (sdl2:game-controller-p index)
    (let* ((handle (sdl2:game-controller-open index))
           (instance (sdl2:game-controller-instance-id handle))
           (id (generate-gamepad-id manager))
           (gamepad (make-gamepad :id id
                                  :instance instance
                                  :name (sdl2:game-controller-name handle)
                                  :handle handle)))
      (setf (u:href (input-manager-gamepad-instances manager) instance) gamepad
            (u:href (input-manager-gamepad-ids manager) id) gamepad)
      (input-transition-in manager :gamepad :attach id)
      (v:debug :zed "Gamepad attached: ~s" id)
      nil)))

(u:fn-> input-event/gamepad-detach (input-manager u:b32) null)
(defun input-event/gamepad-detach (manager instance)
  (declare (optimize speed))
  (let* ((instances (input-manager-gamepad-instances manager))
         (gamepad (u:href instances instance))
         (id (gamepad-id gamepad)))
    (sdl2:game-controller-close (gamepad-handle gamepad))
    (u:appendf (input-manager-detached-gamepads manager) (list id))
    (remhash id (input-manager-gamepad-ids manager))
    (remhash instance instances)
    (input-transition-out manager :gamepad :attach id)
    (v:debug :zed "Gamepad detached: ~s" id)
    nil))

(u:fn-> input-event/gamepad-analog-move (input-manager u:b32 u:ub32 u:b16) null)
(defun input-event/gamepad-analog-move (manager instance axis value)
  (declare (optimize speed))
  (destructuring-bind (sub-device axis) (aref +gamepad-axis-names+ axis)
    (let* ((states (input-manager-states manager))
           (gamepad (get-gamepad-by-instance manager instance))
           (key (list (gamepad-id gamepad) sub-device))
           (value (normalize-gamepad-analog-value sub-device axis value)))
      (u:if-let ((state (u:href states key)))
        (ecase axis
          (:x (setf (gamepad-analog-state-x state) value))
          (:y (setf (gamepad-analog-state-y state) value)))
        (setf (u:href states key) (make-gamepad-analog-state)))))
  nil)

(u:fn-> input-event/gamepad-button-up (input-manager u:b16 u:ub32) null)
(declaim (inline input-event/gamepad-button-up))
(defun input-event/gamepad-button-up (manager instance button)
  (declare (optimize speed))
  (let ((id (gamepad-id (get-gamepad-by-instance manager instance))))
    (input-transition-out manager id (aref +gamepad-button-names+ button))
    (input-transition-out manager id :any)
    nil))

(u:fn-> input-event/gamepad-button-down (input-manager u:b16 u:ub32) null)
(declaim (inline input-event/gamepad-button-down))
(defun input-event/gamepad-button-down (manager instance button)
  (declare (optimize speed))
  (let ((id (gamepad-id (get-gamepad-by-instance manager instance))))
    (input-transition-in manager id (aref +gamepad-button-names+ button))
    (input-transition-in manager id :any)
    nil))
