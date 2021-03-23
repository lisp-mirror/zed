(in-package #:cl-user)

(defpackage #:%zed.input
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:in.gp #:%zed.input.gamepad)
   (#:in.kb #:%zed.input.keyboard)
   (#:in.mgr #:%zed.input.manager)
   (#:in.mouse #:%zed.input.mouse)
   (#:in.tr #:%zed.input.transition)
   (#:in.win #:%zed.input.window)
   (#:win #:%zed.window)
   (#:vp.mgr #:%zed.viewport.manager))
  (:use #:cl))

(in-package #:%zed.input)

;; Construct the input manager.
(defun make-input-manager ()
  (let ((manager (in.mgr::make-manager))
        (mouse-state (in.mouse::make-state)))
    (in.gp::prepare-gamepads)
    (setf (u:href (in.mgr::states manager) '(:mouse :motion)) mouse-state)
    manager))

(defun destroy (manager)
  (in.gp::shutdown-gamepads manager))

(defmacro event-case ((event) &body handlers)
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
(defun dispatch-event (manager window viewports event)
  (event-case (event)
    ;;; Window events.
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref in.win::+event-names+ event-type)
       (:show (in.win::show manager))
       (:hide (in.win::hide manager))
       (:move (in.win::move manager window data1 data2))
       (:resize (in.win::resize manager window viewports data1 data2))
       (:minimize (in.win::minimize manager))
       (:maximize (in.win::maximize manager))
       (:restore (in.win::restore manager))
       (:mouse-focus-enter (in.win::mouse-focus-enter manager))
       (:mouse-focus-exit (in.win::mouse-focus-exit manager))
       (:keyboard-focus-enter (in.win::keyboard-focus-enter manager))
       (:keyboard-focus-exit (in.win::keyboard-focus-exit manager))
       (:close (in.win::close manager))))
    ;;; Mouse events.
    (:mousebuttonup
     (:button button)
     (in.mouse::button-up manager button))
    (:mousebuttondown
     (:button button)
     (in.mouse::button-down manager button))
    (:mousewheel
     (:x x :y y)
     (in.mouse::wheel manager x y))
    (:mousemotion
     (:x x :y y :xrel rx :yrel ry)
     (let ((window-height (win::height window)))
       (in.mouse::move manager window-height x y rx ry)))
    ;;; Keyboard events.
    (:keyup
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (in.kb::up manager (sdl2:scancode-value keysym))))
    (:keydown
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (in.kb::down manager (sdl2:scancode-value keysym))))
    ;;; Gamepad events.
    (:controllerdeviceadded
     (:which id)
     (in.gp::attach manager id))
    (:controllerdeviceremoved
     (:which id)
     (in.gp::detach manager id))
    (:controlleraxismotion
     (:which id :axis axis :value value)
     (in.gp::analog-move manager id axis value))
    (:controllerbuttonup
     (:which id :button button)
     (in.gp::button-up manager id button))
    (:controllerbuttondown
     (:which id :button button)
     (in.gp::button-down manager id button))))

;; Listen for any user input. This is called early in the main game loop.
(u:fn-> handle-events (in.mgr::manager win::window vp.mgr::manager) null)
(defun handle-events (manager window viewports)
  (declare (optimize speed))
  (in.tr::enable-entering manager)
  (in.tr::disable-exiting manager)
  (in.mouse::reset-state manager)
  (loop :with event = (sdl2:new-event)
        :until (zerop (the u:ub32 (sdl2:next-event event :poll)))
        :do (dispatch-event manager window viewports event)
        :finally (sdl2:free-event event)))

(u:fn-> on-button-enter (in.mgr::manager &rest t) boolean)
(declaim (inline on-button-enter))
(defun on-button-enter (manager &rest args)
  (declare (optimize speed))
  (u:when-let ((state (u:href (in.mgr::states manager) args)))
    (in.tr::enter state)))

(u:fn-> on-button-enabled (in.mgr::manager &rest t) boolean)
(declaim (inline ob-button-enabled))
(defun on-button-enabled (manager &rest args)
  (declare (optimize speed))
  (u:when-let ((state (u:href (in.mgr::states manager) args)))
    (in.tr::enabled state)))

(u:fn-> on-button-exit (in.mgr::manager &rest t) boolean)
(declaim (inline ob-button-exit))
(defun on-button-exit (manager &rest args)
  (declare (optimize speed))
  (u:when-let ((state (u:href (in.mgr::states manager) args)))
    (in.tr::exit state)))
