(in-package #:zed)

;; Construct the input manager.
(defun make-input-manager ()
  (let ((manager (%make-input-manager))
        (mouse-state (make-mouse-state)))
    (prepare-gamepads)
    (setf (u:href (input-manager-states manager) '(:mouse :motion)) mouse-state)
    manager))

(defun destroy-input-manager (manager)
  (sdl2:free-event (input-manager-event manager))
  (shutdown-gamepads manager))

(defmacro input-event-case ((event) &body handlers)
  (let (events)
    (dolist (handler handlers)
      (destructuring-bind (type options . body) handler
        (let ((body (list* `(declare (ignorable ,@(u:plist-values options))) body)))
          (dolist (type (u:ensure-list type))
            (u:when-let ((x (sdl2:expand-handler event type options body)))
              (push x events))))))
    `(case (sdl2:get-event-type ,event)
       ,@(nreverse events))))

;; Update the input manager data structure for any input event that comes in.
(defun dispatch-input-event (manager window viewports event)
  (input-event-case (event)
    ;;; Window events.
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (input-event/window-show manager))
       (:hide (input-event/window-hide manager))
       (:move (input-event/window-move manager window data1 data2))
       (:resize (input-event/window-resize manager window viewports data1 data2))
       (:minimize (input-event/window-minimize manager))
       (:maximize (input-event/window-maximize manager))
       (:restore (input-event/window-restore manager))
       (:mouse-focus-enter (input-event/window-mouse-focus-enter manager))
       (:mouse-focus-exit (input-event/window-mouse-focus-exit manager))
       (:keyboard-focus-enter (input-event/window-keyboard-focus-enter manager))
       (:keyboard-focus-exit (input-event/window-keyboard-focus-exit manager))
       (:close (input-event/window-close manager))))
    ;;; Mouse events.
    (:mousebuttonup
     (:button button)
     (input-event/mouse-button-up manager button))
    (:mousebuttondown
     (:button button)
     (input-event/mouse-button-down manager button))
    (:mousewheel
     (:x x :y y)
     (input-event/mouse-wheel-scroll manager x y))
    (:mousemotion
     (:x x :y y :xrel rx :yrel ry)
     (let ((window-height (window-height window)))
       (input-event/mouse-move manager window-height x y rx ry)))
    ;;; Keyboard events.
    (:keyup
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (input-event/key-up manager (sdl2:scancode-value keysym))))
    (:keydown
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (input-event/key-down manager (sdl2:scancode-value keysym))))
    ;;; Gamepad events.
    (:controllerdeviceadded
     (:which id)
     (input-event/gamepad-attach manager id))
    (:controllerdeviceremoved
     (:which id)
     (input-event/gamepad-detach manager id))
    (:controlleraxismotion
     (:which id :axis axis :value value)
     (input-event/gamepad-analog-move manager id axis value))
    (:controllerbuttonup
     (:which id :button button)
     (input-event/gamepad-button-up manager id button))
    (:controllerbuttondown
     (:which id :button button)
     (input-event/gamepad-button-down manager id button))))

;; Listen for any user input. This is called early in the main game loop.
(u:fn-> handle-input-events (input-manager window viewport-manager) null)
(defun handle-input-events (manager window viewports)
  (declare (optimize speed))
  (input-enable-entering manager)
  (input-disable-exiting manager)
  (reset-mouse-state manager)
  (loop :with event = (input-manager-event manager)
        :until (zerop (the u:ub32 (sdl2:next-event event :poll)))
        :do (dispatch-input-event manager window viewports event)))

(u:fn-> on-button-enter (context &rest t) boolean)
(declaim (inline on-button-enter))
(defun on-button-enter (context &rest args)
  (declare (optimize speed))
  (u:when-let* ((manager (context-input-manager context))
                (state (u:href (input-manager-states manager) args)))
    (input-transition-enter state)))

(u:fn-> on-button-enabled (context &rest t) boolean)
(declaim (inline on-button-enabled))
(defun on-button-enabled (context &rest args)
  (declare (optimize speed))
  (u:when-let* ((manager (context-input-manager context))
                (state (u:href (input-manager-states manager) args)))
    (input-transition-enabled state)))

(u:fn-> on-button-exit (context &rest t) boolean)
(declaim (inline on-button-exit))
(defun on-button-exit (context &rest args)
  (declare (optimize speed))
  (u:when-let* ((manager (context-input-manager context))
                (state (u:href (input-manager-states manager) args)))
    (input-transition-exit state)))
