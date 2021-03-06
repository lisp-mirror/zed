(in-package #:cl-user)

(defpackage #:%zed.protocol.input
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.core.context)
   (#:in #:%zed.input)
   (#:man #:%zed.input.manager)
   (#:mouse #:%zed.input.mouse)
   (#:tr #:%zed.input.transition)
   (#:win #:%zed.render-backend.window))
  (:use #:cl)
  (:export
   #:disable-relative-mouse-mode
   #:enable-relative-mouse-mode
   #:get-mouse-position
   #:get-mouse-scroll
   #:on-button-enabled
   #:on-button-enter
   #:on-button-exit
   #:on-window-event-enabled
   #:on-window-event-enter
   #:on-window-event-exit
   #:relative-mouse-mode-p))

(in-package #:%zed.protocol.input)

(defun on-button-enter (context &rest args)
  (apply #'in::on-button-enter (ctx::input-manager context) args))

(defun on-button-enabled (context &rest args)
  (apply #'in::on-button-enabled (ctx::input-manager context) args))

(defun on-button-exit (context &rest args)
  (apply #'in::on-button-exit (ctx::input-manager context) args))

(defun on-window-event-enter (context event)
  (declare (optimize speed))
  (u:when-let ((state (u:href (man::states (ctx::input-manager context)) (list :window event))))
    (tr::enter state)))

(defun on-window-event-enabled (context event)
  (declare (optimize speed))
  (u:when-let ((state (u:href (man::states (ctx::input-manager context)) (list :window event))))
    (tr::enabled state)))

(defun on-window-event-exit (context event)
  (declare (optimize speed))
  (u:when-let ((state (u:href (man::states (ctx::input-manager context)) (list :window event))))
    (tr::exit state)))

(defun get-mouse-position (context)
  (declare (optimize speed))
  (let* ((state (u:href (man::states (ctx::input-manager context)) '(:mouse :motion)))
         (x (mouse::x state))
         (y (mouse::y state))
         (relative-x (mouse::relative-x state))
         (relative-y (mouse::relative-y state)))
    (values x y relative-x relative-y)))

(defun get-mouse-scroll (context axis)
  (declare (optimize speed))
  (let ((states (man::states (ctx::input-manager context))))
    (ecase axis
      (:horizontal (or (u:href states '(:mouse :scroll-horizontal)) 0))
      (:vertical (or (u:href states '(:mouse :scroll-vertical)) 0)))))

(defun enable-relative-mouse-mode (context)
  (declare (optimize speed))
  (let* ((state (u:href (man::states (ctx::input-manager context)) '(:mouse :motion)))
         (window-height (win::height (ctx::window context)))
         (x (mouse::x state))
         (y (- window-height (mouse::y state))))
    (sdl2:set-relative-mouse-mode 1)
    (setf (mouse::relative-mode-p state) t
          (mouse::warp-x state) x
          (mouse::warp-y state) y)))

(defun disable-relative-mouse-mode (context &key (warp-p t))
  (declare (optimize speed))
  (let ((state (u:href (man::states (ctx::input-manager context)) '(:mouse :motion))))
    (sdl2:set-relative-mouse-mode 0)
    (setf (mouse::relative-mode-p state) nil)
    (when warp-p
      (let ((x (mouse::warp-x state))
            (y (mouse::warp-y state)))
        (sdl2:warp-mouse-in-window nil x y)))))

(defun relative-mouse-mode-p (context)
  (declare (optimize speed))
  (let ((state (u:href (man::states (ctx::input-manager context)) '(:mouse :motion))))
    (mouse::relative-mode-p state)))
