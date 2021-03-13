(in-package #:cl-user)

(defpackage #:%zed.input.gamepad
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:man #:%zed.input.manager)
   (#:tr #:%zed.input.transition))
  (:use #:cl))

(in-package #:%zed.input.gamepad)

(defstruct (gamepad
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (id :gamepad1 :type keyword)
  instance
  name
  handle)

(defstruct (analog-state
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (x 0.0 :type u:f32)
  (y 0.0 :type u:f32)
  (deadzone 0.0 :type u:f32))

(u:define-constant +axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y) (:triggers :x)
      (:triggers :y))
  :test #'equalp)

(u:define-constant +button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button :left-shoulder
      :right-shoulder :up :down :left :right)
  :test #'equalp)

(u:fn-> get-by-instance (man::manager u:b32) gamepad)
(declaim (inline get-by-instance))
(defun get-by-instance (manager instance)
  (declare (optimize speed))
  (u:href (man::gamepad-instances manager) instance))

(u:fn-> generate-id (man::manager) keyword)
(defun generate-id (manager)
  (declare (optimize speed))
  (values
   (or (pop (man::detached-gamepads manager))
       (u:format-symbol :keyword "GAMEPAD~d"
                        (1+ (hash-table-count (man::gamepad-instances manager)))))))

;; TODO: Add gamepad mapping database when we can load files agnostic of asdf or relative to
;; deployed binary.
(defun prepare-gamepads ()
  (sdl2:init* '(:gamecontroller))
  (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1"))

(u:fn-> shutdown-gamepads (man::manager) null)
(defun shutdown-gamepads (manager)
  (declare (optimize speed))
  (u:when-let ((instances (man::gamepad-instances manager)))
    (u:do-hash-values (v instances)
      (sdl2:game-controller-close (handle v)))
    (clrhash instances))
  nil)

(u:fn-> normalize-analog-value (keyword keyword u:b16) u:f32)
(defun normalize-analog-value (sub-device axis value)
  (declare (optimize speed))
  (if (eq sub-device :triggers)
      (values (u:map-domain 0 32767 0 1 value))
      (let ((clamped (u:clamp value -32768 32767)))
        (values
         (ecase axis
           (:x (u:map-domain -32768 32767 -1 1 clamped))
           (:y (u:map-domain -32768 32767 1 -1 clamped)))))))

(u:fn-> attach (man::manager u:b32) null)
(defun attach (manager index)
  (declare (optimize speed))
  (when (sdl2:game-controller-p index)
    (let* ((handle (sdl2:game-controller-open index))
           (instance (sdl2:game-controller-instance-id handle))
           (id (generate-id manager))
           (gamepad (make-gamepad :id id
                                  :instance instance
                                  :name (sdl2:game-controller-name handle)
                                  :handle handle)))
      (setf (u:href (man::gamepad-instances manager) instance) gamepad
            (u:href (man::gamepad-ids manager) id) gamepad)
      (tr::in manager :gamepad :attach id)
      nil)))

(u:fn-> detach (man::manager u:b32) null)
(defun detach (manager instance)
  (declare (optimize speed))
  (let* ((instances (man::gamepad-instances manager))
         (gamepad (u:href instances instance))
         (id (id gamepad)))
    (sdl2:game-controller-close (handle gamepad))
    (u:appendf (man::detached-gamepads manager) (list id))
    (remhash id (man::gamepad-ids manager))
    (remhash instance instances)
    (tr::out manager :gamepad :attach id)
    nil))

(u:fn-> analog-move (man::manager u:b32 u:ub32 u:b16) null)
(defun analog-move (manager instance axis value)
  (declare (optimize speed))
  (destructuring-bind (sub-device axis) (aref +axis-names+ axis)
    (let* ((states (man::states manager))
           (gamepad (get-by-instance manager instance))
           (key (list (id gamepad) sub-device))
           (value (normalize-analog-value sub-device axis value)))
      (u:if-let ((state (u:href states key)))
        (ecase axis
          (:x (setf (x state) value))
          (:y (setf (y state) value)))
        (setf (u:href states key) (make-analog-state)))))
  nil)

(u:fn-> button-up (man::manager u:b16 u:ub32) null)
(declaim (inline button-up))
(defun button-up (manager instance button)
  (declare (optimize speed))
  (let ((id (id (get-by-instance manager instance))))
    (tr::out manager id (aref +button-names+ button))
    (tr::out manager id :any)
    nil))

(u:fn-> button-down (man::manager u:b16 u:ub32) null)
(declaim (inline button-down))
(defun button-down (manager instance button)
  (declare (optimize speed))
  (let ((id (id (get-by-instance manager instance))))
    (tr::in manager id (aref +button-names+ button))
    (tr::in manager id :any)
    nil))
