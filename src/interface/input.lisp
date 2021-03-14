(in-package #:zed)

(defun on-button-enter (context &rest args)
  (apply #'in::on-button-enter (ctx::input-manager context) args))

(defun on-button-enabled (context &rest args)
  (apply #'in::on-button-enabled (ctx::input-manager context) args))

(defun on-button-exit (context &rest args)
  (apply #'in::on-button-exit (ctx::input-manager context) args))

(defun on-window-event-enter (context event)
  (declare (optimize speed))
  (u:when-let ((state (u:href (in.mgr::states (ctx::input-manager context)) (list :window event))))
    (in.tr::enter state)))

(defun on-window-event-enabled (context event)
  (declare (optimize speed))
  (u:when-let ((state (u:href (in.mgr::states (ctx::input-manager context)) (list :window event))))
    (in.tr::enabled state)))

(defun on-window-event-exit (context event)
  (declare (optimize speed))
  (u:when-let ((state (u:href (in.mgr::states (ctx::input-manager context)) (list :window event))))
    (in.tr::exit state)))

(defun get-mouse-position (context)
  (declare (optimize speed))
  (let* ((state (u:href (in.mgr::states (ctx::input-manager context)) '(:mouse :motion)))
         (x (in.mouse::x state))
         (y (in.mouse::y state))
         (relative-x (in.mouse::relative-x state))
         (relative-y (in.mouse::relative-y state)))
    (values x y relative-x relative-y)))

(defun get-mouse-scroll (context axis)
  (declare (optimize speed))
  (let ((states (in.mgr::states (ctx::input-manager context))))
    (ecase axis
      (:horizontal (or (u:href states '(:mouse :scroll-horizontal)) 0))
      (:vertical (or (u:href states '(:mouse :scroll-vertical)) 0)))))

(defun enable-relative-mouse-mode (context)
  (declare (optimize speed))
  (let* ((state (u:href (in.mgr::states (ctx::input-manager context)) '(:mouse :motion)))
         (window-height (win::height (ctx::window context)))
         (x (in.mouse::x state))
         (y (- window-height (in.mouse::y state))))
    (sdl2:set-relative-mouse-mode 1)
    (setf (in.mouse::relative-mode-p state) t
          (in.mouse::warp-x state) x
          (in.mouse::warp-y state) y)))

(defun disable-relative-mouse-mode (context &key (warp-p t))
  (declare (optimize speed))
  (let ((state (u:href (in.mgr::states (ctx::input-manager context)) '(:mouse :motion))))
    (sdl2:set-relative-mouse-mode 0)
    (setf (in.mouse::relative-mode-p state) nil)
    (when warp-p
      (let ((x (in.mouse::warp-x state))
            (y (in.mouse::warp-y state)))
        (sdl2:warp-mouse-in-window nil x y)))))

(defun relative-mouse-mode-p (context)
  (declare (optimize speed))
  (let ((state (u:href (in.mgr::states (ctx::input-manager context)) '(:mouse :motion))))
    (in.mouse::relative-mode-p state)))
