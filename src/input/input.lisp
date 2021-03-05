(in-package #:cl-user)

(defpackage #:%zed.input
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:in.kb #:%zed.input.keyboard)
   (#:in.mouse #:%zed.input.mouse)
   (#:in.win #:%zed.input.window)
   (#:man #:%zed.input.manager)
   (#:tr #:%zed.input.transition)
   (#:win #:%zed.render-backend.window))
  (:use #:cl))

(in-package #:%zed.input)

;; Construct the input manager.
(defun make-input-manager ()
  (let ((manager (man::make-manager))
        (mouse-state (in.mouse::make-state)))
    (setf (u:href (man::states manager) '(:mouse :motion)) mouse-state)
    manager))

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
(defun dispatch-event (manager window event)
  (event-case (event)
    ;;; Window events.
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref in.win::+event-names+ event-type)
       (:show (in.win::show manager))
       (:hide (in.win::hide manager))
       (:move (in.win::move manager data1 data2))
       (:resize (in.win::resize manager data1 data2))
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
       (in.kb::down manager (sdl2:scancode-value keysym))))))

;; Listen for any user input. This is called early in the main game loop.
(defun handle-events (manager window)
  (declare (optimize speed))
  (tr::enable-entering manager)
  (tr::disable-exiting manager)
  (in.mouse::reset-state manager)
  (loop :with event = (sdl2:new-event)
        :until (zerop (the u:ub32 (sdl2:next-event event :poll)))
        :do (dispatch-event manager window event)
        :finally (sdl2:free-event event)))
