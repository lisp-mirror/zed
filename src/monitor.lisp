(in-package #:zed)

(defstruct (monitor
            (:constructor %make-monitor)
            (:predicate nil)
            (:copier nil))
  (id 0 :type u:ub8)
  (width 0 :type u:ub16)
  (height 0 :type u:ub16)
  (x 0 :type u:ub16)
  (y 0 :type u:ub16)
  (refresh-rate 0 :type u:ub8))

(u:define-printer (monitor stream :type nil)
  (format stream "MONITOR: ~dx~d @ ~dHz"
          (monitor-width monitor)
          (monitor-height monitor)
          (monitor-refresh-rate monitor)))

(defun get-monitor-id (window-handle)
  (sdl2-ffi.functions:sdl-get-window-display-index window-handle))

(defgeneric get-monitor-dimensions (monitor)
  (:method ((monitor monitor))
    (get-monitor-dimensions (monitor-id monitor)))
  (:method ((monitor integer))
    (u:mvlet ((format width height refresh-rate (sdl2:get-current-display-mode monitor)))
      (values width height))))

(defgeneric get-monitor-position (monitor)
  (:method ((monitor monitor))
    (get-monitor-position (monitor-id monitor)))
  (:method ((monitor integer))
    (let* ((rect (sdl2:get-display-bounds monitor))
           (x (sdl2:rect-x rect))
           (y (sdl2:rect-y rect)))
      (sdl2:free-rect rect)
      (values x y))))

(defgeneric get-monitor-refresh-rate (monitor)
  (:method ((monitor monitor))
    (get-monitor-refresh-rate (monitor-id monitor)))
  (:method ((monitor integer))
    (u:mvlet ((format width height refresh-rate (sdl2:get-current-display-mode monitor)))
      refresh-rate)))

(defun make-monitor (window-handle)
  (u:mvlet* ((id (get-monitor-id window-handle))
             (width height (get-monitor-dimensions id))
             (x y (get-monitor-position id)))
    (%make-monitor :id id
                   :width width
                   :height height
                   :x x
                   :y y
                   :refresh-rate (get-monitor-refresh-rate id))))
